### Czyszczenie
rm(list = ls())

### biblioteki
library(survey)
library(tidyr)
library(dplyr)
library(stargazer)
library(car)
library(svyROC)
library(UBL)
library(parallel)

### Wczytanie danych
load("../data/NSDUH_2022_final_dataset.Rdata")
load("../data/weighting_vars.Rdata")
load("../data/harddrug_vars.Rdata")
load("../data/softdrug_vars.Rdata")
load("../data/demo_vars.Rdata")

### ścieżki
tabDir <- "../paper/tabs/"
pltDir <- "../paper/plots/"

hard_drugs <- harddrug_vars


### 
psrsq_nglk <- function(object, null_model, design_og, key_var, predicted){
  w<-weights(design_og,"sampling")
  N<-sum(w)
  n<-sum(object$prior.weights)
  minus2ell0<-null_model$deviance*(N/n)
  observed <- as.numeric(design_og$variables[[key_var]]) - 1
  predicted <- as.vector(predict(object, type = "response", design_og$variables))

  loglik <- sum(w * (observed * log(predicted) + (1 - observed) * log(1 - predicted)))
  minus2ell1 <- -2*loglik
  
  mutualinf<-(minus2ell1-minus2ell0)/N
  r2cs<-1-exp(mutualinf)
  scaling<-1-exp(-minus2ell0/N)
  r2cs/scaling
}



fit_svyglm2 <- function(hard_drug, data, soft_drugs, demo, weighting_vars, boot_n){

  # Funkcja do dynamicznego trenowania regresji logistycznej i zwracania
  # modeli na danych zwazonych, niezwazonych oraz wskaznikow dopasowania dla modelu
  # na danych zwazonych.
  # 
  # Input:
  # - hard_drug - zmienna objasniana wskazująca na to czy kiedykolwiek doszło do
  # inicjacji z daną niedozwoloną uzywka
  # - data - zbior danych nsduh
  # - soft_drugs - wektor z 3 literowymi nazwami miekkich uzywek wykorzystywanych
  # w modelu
  # - demo - zmienne demograficzne sa wpisywane do formuly modelu bez zmian
  # - weighting_vars - zmienne definiujace strukture ankietowa zbioru
  # - boot_n - liczba prób bootstrapowych
  # 
  # output:
  # - funkcja zapisuje modele, oraz odpowiadające im ramki danych z dopasowaniem
  # do danych oryginalnych, oraz współczynniki
  
  ### start czasu
  start_time = Sys.time()
  
  ### ścieżka do zapisywania danych
  modDir <- "../data/model_results/"
  
  ### przewidywana zmienna
  key_var <- paste0("EVER_", hard_drug)
  
  ### nazwy zmiennych niezaleznych
  soft_drugs_przed <- unname(sapply(soft_drugs, 
                                    function(drug) 
                                      paste0(drug, "_przed_", hard_drug)))

  ### tworzenie wektora predyktorów i filtrowanie kolumn zbioru
  predictors <- c(soft_drugs_przed, demo)
  data = data %>%
    dplyr::select(all_of(c(predictors, key_var, weighting_vars))) %>%
    mutate(!!sym(key_var) := factor(.[[key_var]]),
           across(contains("przed"), ~as.factor(.))) 
  
  ### standaryzowanie nazw predyktorow dla wszystkich modeli
  variable_names = gsub(paste0("_przed_", hard_drug), "_przed", colnames(data))
  predictors = gsub(paste0("_przed_", hard_drug), "_przed", predictors)
  colnames(data) = variable_names
  
  ### próg odciecia dla regresji
  cramer_threshold = 
    sum((data[[key_var]] == 1) * data$ANALWT2_C) / sum(data$ANALWT2_C)
  
  ### formula modelu
  formula <- as.formula(paste(key_var, "~", 
                              paste(predictors, collapse = " + ")))
  
  ### model prosty na potrzeby R^2
  survey_null_design = svydesign(ids = ~VEREP,
                                strata = ~VESTR_C,
                                weights = ~ANALWT2_C,
                                data = data,
                                nest = TRUE)
  null_formula = as.formula(paste(key_var, "~ 1"))
  null_model = svyglm(null_formula,
                       design = survey_null_design,
                       family = quasibinomial())
  
  ### tomeklinks
  data_tomek <- data %>%
    mutate(VEREP = as.factor(VEREP),
           VESTR_C = as.factor(VESTR_C)) %>%
    group_by(VESTR_C) %>%
    group_modify(~ {
      result <- TomekClassif(formula,
                             dat = as.data.frame(.x),
                             dist = "HEOM",
                             Cl = "0",
                             rem = "maj")
      processed_data <- result[[1]]
      return(as.data.frame(processed_data))
    })
  
  ### tworzenie "bootstrapow" i trenowanie modeli
  for (i in 1:boot_n) {
    
 ### smote
 data_replicate <- data_tomek %>%
    group_modify(~{
        smote_data <-
          SmoteClassif(
            form = formula,
            dat = as.data.frame(.x),
            C.perc = list("0" = 1, "1" = 1.5),
            k = 5,
            dist = "HVDM"
            )
        return(smote_data)
        }) %>%
    ungroup() %>%
    mutate(VEREP = as.numeric(VEREP),
           VESTR_C = as.numeric(VESTR_C))

  ### projekt ankiety dla replikatu
  survey_design_replica = svydesign(ids = ~VEREP,
            strata = ~VESTR_C,
            weights = ~ANALWT2_C,
            data = data_replicate,
            nest = TRUE)
  
  ### model ankietowy
  model_svy <- svyglm(formula, 
                      design = survey_design_replica, 
                      family = quasibinomial())
  
  ### tworzenie predykcji na danych pierwotnych
  data_pred = data %>%
    mutate(est_prob = as.vector(predict(model_svy, 
                              newdata = data,
                              type = "response"))) %>%
    mutate(predictions = factor(ifelse(est_prob >= cramer_threshold, 1, 0)))
  
  ### projekt ankiety dla oryginalnych danych dla predykcji
  survey_design_og = svydesign(ids = ~VEREP,
                               strata = ~VESTR_C,
                               weights = ~ANALWT2_C,
                               data = data_pred,
                               nest = TRUE)
  
  ### AUC calc
  auc <- wauc(response.var = data_pred[[key_var]],
              phat.var = data_pred$est_prob,
              weights.var = data_pred$ANALWT2_C,
              tag.event = "1",
              tag.nonevent = "0")
  
  ### macierz bledow
  conf_matrix <- svytable(as.formula(paste("~", key_var, "+ predictions")),
                          design = survey_design_og)
  
  if(ncol(conf_matrix)==2){
  ### liczenie metryk dla modelu ankietowego na podstawie macierzy bledow
    TP <- conf_matrix["1", "1"]
    FP <- conf_matrix["0", "1"]
    FN <- conf_matrix["1", "0"] 
    TN <- conf_matrix["0", "0"]
    recall <- TP / (TP + FN)
    precision <- TP / (TP + FP)
    f1_score <- 2 * (precision * recall) / (precision + recall)
  }else{
    recall = 0
    precision = 0
    f1_score = NA_real_
  }
  
  ### macierz podsumowujaca z metrykami
  metrics_example <- data.frame(
    R_boot = psrsq(model_svy, method = "Nagelkerke"),
    R_NK = psrsq_nglk(model_svy, 
                      null_model, 
                      survey_design_og, 
                      key_var, 
                      data_pred$est_prob),
    Recall = recall,
    Precision = precision,
    F1_Score = f1_score,
    AUC = auc$AUCw
    )
  coefs_extracted <- as.data.frame(t(model_svy$coefficients))

  ### celem oszczędzania RAMu dane są zapisywane na dysku a obiekty usuwane
  save(model_svy,
       file = paste0(modDir, hard_drug, "_model_", i, ".RData"))
  save(metrics_example,
       file = paste0(modDir, hard_drug, "_fit_metrics_", i, ".RData"))
  save(coefs_extracted,
       file = paste0(modDir, hard_drug, "_coefs_", i, ".RData"))
  rm(model_svy, 
     metrics_example, 
     coefs_extracted, 
     conf_matrix, 
     survey_design_og,
     data_pred,
     survey_design_replica)
  gc(verbose = FALSE)
  
  if (i == 1){print("First rep done")}
  if (i == floor(boot_n * 0.25)) {
    print(paste(hard_drug, "25%"))
  } else if(i == floor(boot_n * 0.50)){
    print(paste(hard_drug, "50%"))
  } else if(i == floor(boot_n * 0.75)){
    print(paste(hard_drug, "75%"))
  }
  }
  exe_time = Sys.time() - start_time
  print(paste(hard_drug, "100%"))
  print(exe_time)
}


### trenowanie modeli dla wszystkich uzywek
mclapply(c(hard_drugs, "HDG"), 
       function(drug) fit_svyglm2(drug, 
                                  NSDUH_2022_final_dataset, 
                                  c("ALC", "VAP", "SMK", "MRJ"), 
                                  c("GENDER", "ETHNIC", "RLGINF"), 
                                  weighting_vars,
                                  boot_n = 1000),
       mc.cores = 2)
