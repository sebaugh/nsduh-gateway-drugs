### Czyszczenie
rm(list = ls())

library(survey)
library(tidyr)
library(dplyr)
library(stargazer)

### Wczytanie zbioru danych
load("../data/NSDUH_2022_final_dataset.Rdata")
load("../data/weighting_vars.Rdata")
load("../data/harddrug_vars.Rdata")
load("../data/softdrug_vars.Rdata")
load("../data/demo_vars.Rdata")

# ścieżki
tabDir <- "../paper/tabs/"
pltDir <- "../paper/plots/"

### kopia wektora
hard_drugs <- harddrug_vars

### definiowanie funkcji do tworzenia modeli coxa
fit_svycoexph2 <- function(hard_drug, 
                           data, 
                           soft_drugs, 
                           demo, 
                           weighting_vars, 
                           strata) {
  
  ### zmienna wydarzenia
  key_var <- paste0("EVER_", hard_drug)
  
  ### tworzenie czasu zdarzenia/końca obserwacji
  data <- data %>%
    mutate(TIME_END = if_else(is.na(!!sym(hard_drug)), 
                              ESTAGE, 
                              !!sym(hard_drug)))
  
  ### korygowanie czasu końca obserwacji względem najwcześniejszej obserwacji
  ### tak aby w modelu nie było okresu bez ryzyka
  min_obs_end <- min(data$TIME_END)
  data <- data %>%
    mutate(TIME_END = OBS_END - min_obs_end)

  ### tworzenie wektora z nazwami predyktorow uzywek zainicjowanych przed badana
  soft_drugs_przed <- unname(sapply(soft_drugs, 
                                    function(drug) 
                                      paste0(drug, "_przed_", hard_drug)))
  
  ### tworzenie wektora z nazwami wszystkich zmiennych i filtrowanie zbioru
  predictors <- c(soft_drugs_przed, demo)
  all_vars <- c(predictors, key_var, "TIME_END", weighting_vars, strata)
  filtered_data <- data %>%
    select(all_of(all_vars))
  
  
  ### tworzenie formuly modelu w zaleznosci od tego czy definiowane 
  ### jest stratowanie
  if (length(strata) == 0) {
    formula <- as.formula(paste0("Surv(TIME_END, ", key_var, ") ~ ", 
                                 paste(predictors, collapse = " + ")))
  }else{
    formula <- as.formula(paste0("Surv(TIME_END, ", key_var, ") ~ ", 
                                 paste(predictors, collapse = " + "), 
                                 " + strata(", 
                                 paste(strata, collapse = ", "), ")"))
  }

  
  ### tworzenie projektu ankiety
  survey_design <- svydesign(
    ids = ~VEREP,
    strata = ~VESTR_C,
    weights = ~ANALWT2_C,
    data = filtered_data,
    nest = TRUE
  )
  
  ### trenowanie modeli na projekcie ankiety i niewazonym
  model <- svycoxph(formula, design = survey_design)
  model_binom <- coxph(formula, data = survey_design$variables)

  return(list(svycoxph = model, coxph = model_binom))
}




demo_vars_used <- demo_vars[!(demo_vars %in% c("AGECAT", "ETHNIC", "INCOME", "AGEPRC", "EDUCAT"))]

cox_models <- lapply(hard_drugs, function(drug) 
  fit_svycoexph2(drug, 
                NSDUH_2022_final_dataset, 
                c("MRJ", "ALC", "FSM", "ESM"), 
                c("GENDER", "ETHNIC", "RLGINF", "TOTINC"), 
                weighting_vars,
                c()))
names(cox_models) <- hard_drugs
summary(cox_models$NMD$svycoxph)
cox.zph(cox_models$NMD$svycoxph)
