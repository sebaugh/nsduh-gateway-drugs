### Czyszczenie
rm(list = ls())

### Biblioteki
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(skimr)
library(stargazer)

### UWAGA: Założenie jest takie, że katalog roboczy to jest główny
### katalog repo.

### Ścieżki
tabDir <- "../paper/tabs/"

### Wczytanie oryginalnego zbioru danych
load("../../data/NSDUH_2022.Rdata")

NSDUH_2022 <- puf2022_110424
rm(puf2022_110424)

# zestandaryzowanie nazw zmiennych
names(NSDUH_2022) <- toupper(names(NSDUH_2022))

weighting_vars <- c("VEREP",
                    "VESTR_C",
                    "ANALWT2_C"
                    ) ### zmienne na potrzeby konstrukcji projektu ankiety

softdrug_vars <- c("IRCDUAGE", ### age started smoking every day
                   "IRCIGAGE", ### age tried smoking for the first time
                   "IRNICVAPAGE", ### age tried vaping for the first time
                   "IRALCAGE", ### age tried alcohol for the first time
                   "IRMJAGE" ### age tried marijuana for the first time
                   )

nmddrug_vars <- c("IRCOCAGE", ### age tried cocaine for the first time
                   "IRHERAGE", ### age tried heroin for the first time
                   "IRCRKAGE", ### age tried crack for the first time
                   "IRHALLUCAGE", ### age tried hallucinogens for the first time
                   "IRMETHAMAGE" ### age tried methamphetamine for the first time
                  )
                  
meddrug_vars <- c("IRSEDNMAGE", ### sedatives up to 12 months before interview
                  "IRTRQNMAGE", ### tranquilizers up to 12 months before interview
                  "IRPNRNMAGE", ### pain killers up to 12 months before interview
                  "IRSTMNMAGE" ### stimulants up to 12 months before interview
                  )

imputation_ind <- c(
  "IICDUAGE", ### imputation smoking every day
  "IICIGAGE", ### imputation smoking for the first time
  "IINICVAPAGE", ### imputation vaping for the first time
  "IIALCAGE", ### imputation alcohol for the first time
  "IIMJAGE", ### imputation marijuana for the first time
  "IICOCAGE", ### imputation cocaine for the first time
  "IIHERAGE", ### imputation heroin for the first time
  "IICRKAGE", ### imputation crack for the first time
  "IIHALLUCAGE", ### imputation hallucinogens for the first time
  "IISEDNMAGE", ### imputation sedatives up to 12 months before interview
  "IITRQNMAGE", ### imputation tranquilizers up to 12 months before interview
  "IIPNRNMAGE", ### imputation pain killers up to 12 months before interview
  "IISTMNMAGE", ### imputation stimulants up to 12 months before interview
  "IIMETHAMAGE"                 
  )

demo_vars <- c("IRSEX", ### Płeć
               "AGE3", ### kategorie wiekowe
               "CATAGE", ### zagregowane kategorie wiekowe
               "INCOME", ### recode tot family income revised
               "NEWRACE2", ### grupa etniczna
               "SNRLDCSN", ### religious beliefs influence decisions adults
               "YERLDCSN", ### religious beliefs influence decisions youth
               "EDUHIGHCAT", ### najwyższy stopień edukacji
               "PDEN10" ### gęstośc populacji
               )

demo_imp <- c("IIEDUHIGHST2", ### imputation for education
              "IIFAMIN3" ### imputation for tot family
              )

# wybór zmiennych
NSDUH_2022_filtered <- NSDUH_2022 %>%
  dplyr::select(all_of(c(weighting_vars,
      softdrug_vars,
      nmddrug_vars,
      meddrug_vars,
      demo_vars,
      imputation_ind,
      demo_imp)
    )
  )
rm(NSDUH_2022)

# Slownik z nowymi nazwami
new_names <- c(
  # Zmienne miekkich narkotykow
  "ESM" = "IRCDUAGE",
  "FSM" = "IRCIGAGE",
  "VAP" = "IRNICVAPAGE",
  "ALC" = "IRALCAGE",
  "MRJ" = "IRMJAGE",
  
  # Zmienne twardych narkotykow
  "COC" = "IRCOCAGE",
  "HER" = "IRHERAGE",
  "CRK" = "IRCRKAGE",
  "HAL" = "IRHALLUCAGE",
  "SED" = "IRSEDNMAGE",
  "TRQ" = "IRTRQNMAGE",
  "PNK" = "IRPNRNMAGE",
  "STM" = "IRSTMNMAGE",
  "MTH" = "IRMETHAMAGE",
  
  # Zmienne imputacji narkotykow
  "ESM_IMP" = "IICDUAGE", 
  "FSM_IMP" = "IICIGAGE", 
  "VAP_IMP" = "IINICVAPAGE", 
  "ALC_IMP" = "IIALCAGE", 
  "MRJ_IMP" = "IIMJAGE", 
  "COC_IMP" = "IICOCAGE", 
  "HER_IMP" = "IIHERAGE", 
  "CRK_IMP" = "IICRKAGE", 
  "HAL_IMP" = "IIHALLUCAGE", 
  "SED_IMP" = "IISEDNMAGE", 
  "TRQ_IMP" = "IITRQNMAGE", 
  "PNK_IMP" = "IIPNRNMAGE", 
  "STM_IMP" = "IISTMNMAGE", 
  "MTH_IMP" = "IIMETHAMAGE",
  
  # Demograficzne
  "GENDER" = "IRSEX",
  "AGECAT" = "CATAGE",
  "AGEPRC" = "AGE3",
  "TOTINC" = "INCOME",
  "TOTINC_IMP" = "IIFAMIN3",
  "ETHNIC" = "NEWRACE2",
  "RLGIFA" = "SNRLDCSN",
  "RLGIFY" = "YERLDCSN",
  "EDUCAT" = "EDUHIGHCAT",
  "EDUCAT_IMP" = "IIEDUHIGHST2",
  "POPDEN" = "PDEN10"
)

# zmiana nazw kolumn
NSDUH_2022_renamed <- NSDUH_2022_filtered %>% rename(!!!new_names)
rm(NSDUH_2022_filtered)

# zmiana nazw w wektorach
softdrug_vars <- names(new_names)[new_names %in% softdrug_vars]
meddrug_vars <- names(new_names)[new_names %in% meddrug_vars]
nmddrug_vars <- names(new_names)[new_names %in% nmddrug_vars]
demo_vars <- names(new_names)[new_names %in% demo_vars]
demo_imp <- names(new_names)[new_names %in% demo_imp]



# funkcja do porównania wieków
create_comparisons <- function(data, var, others) {
  others <- others[others != var]
  comparisons <- map_dfc(others, ~ {
    ## zastosowanie tylko do środków leczniczych wykorzystywanych bez
    ## recepty 993 - użytkownik ale nie znamy wieku inicjacji, 999
    ## nigdy nie korzystał
    new_col <- if_else(data[[.x]] %in% c(993, 999), ## użytkownik miękkich narkotyków
      if_else(data[[var]] < 150,
        ## przypadek w którym użytkownik zaczął nadużywać leki bez
        ## recepty ale nie wiadomo w jakim wieku, a jednocześnie
        ## zażywa miękkie używki stąd brak danych nie ma możliwości
        ## porównania
        if_else(data[[.x]] == 993 & .x != "ESM", NA, 1L),
        ## nigdy nie był użytkownikiem miękkiej używki
        0L),
      ## jeśli nie był użytkownikiem miękkiej używki to możliwe
      ## wartości data[[var]] to 991 i 993 w obu przypadkach jeśli
      ## respondent nie jest użytkownikiem twardych używek to
      ## najwyższa wartość to 991 bo 993 i 999 odrzuciliśmy zatem
      ## data[[var]] dla nieużytkowników miękkich będzie większe lub
      ## równe zatem da wartość FALSE
      data[[var]] < data[[.x]]) 
    setNames(as_tibble(new_col), paste(var, "przed", .x, sep = "_"))
  })
  return(comparisons)
}

# dodanie zmiennych dla twardych narkotyków bez medycznych i medycznych
NSDUH_2022_renamed <- NSDUH_2022_renamed %>%
  mutate(MED = pmin(SED, TRQ, PNK, STM),
         NMD = pmin(COC, HER, CRK, HAL, MTH),
         HDG = pmin(COC, HER, CRK, HAL, MTH, SED, TRQ, PNK, STM))

# wektor z nazwami kolumn narkotyków
meddrug_vars <- c("MED", meddrug_vars)
nmddrug_vars <- c("NMD", nmddrug_vars)
harddrug_vars <- c(meddrug_vars, nmddrug_vars)
drugs <- c(softdrug_vars, harddrug_vars)

# utworzenie porównań
comparison_results <- map(softdrug_vars, 
                          ~ create_comparisons(NSDUH_2022_renamed, 
                                               .x, 
                                               c(harddrug_vars, "HDG")))

# dołączenie porównań do oryginalnego zbioru
NSDUH_2022_final_dataset <- bind_cols(NSDUH_2022_renamed, 
                                      bind_cols(comparison_results)) %>%
  mutate(across(all_of(c(harddrug_vars, "HDG")),
                ~ get(paste0("FSM_przed_", cur_column())) + 
                  get(paste0("ESM_przed_", cur_column())),
                .names = "SMK_przed_{.col}"))
rm(NSDUH_2022_renamed, comparison_results)

### sprawdzenie czy zmienne dotyczące inicjacji palenia i nałogowego są poprawne
### względem siebie
violations <- sapply(c(harddrug_vars, "HDG"), function(drug) {
  any(NSDUH_2022_final_dataset[[paste0("ESM_przed_", drug)]] > 
        NSDUH_2022_final_dataset[[paste0("FSM_przed_", drug)]], na.rm = TRUE)
})
print(violations)



### usunięcie zmiennych palenia
NSDUH_2022_final_dataset <- NSDUH_2022_final_dataset %>%
  select(-matches("(ESM|FSM)_przed_"))

# wizualizacja zmiennych imputacji
NSDUH_2022_final_dataset %>%
  dplyr::select(contains("_IMP"), c("ANALWT2_C")) %>%
  pivot_longer(contains("_IMP"), names_to = "drug", values_to = "values") %>%
  ggplot() + geom_bar(aes(x = values, 
                          weight = ANALWT2_C)) + 
  facet_wrap(~drug) + 
  theme_minimal()

# rekodowanie zmiennych ind i dodanie zmiennych czy kiedykolwiek brana używka
# przekształcenie zmiennych demograficznych na typ factor
NSDUH_2022_final_dataset <- NSDUH_2022_final_dataset %>%
  mutate(across(contains("_IMP"),~ ifelse(. %in% c(1, 9), 0, 1)),
         across(all_of(c(softdrug_vars, nmddrug_vars)), 
                ~ ifelse(. < 100, 1, 0), .names = "EVER_{.col}"),
         across(all_of(c(meddrug_vars, "HDG")), 
                ~ ifelse((. < 100) | (. == 993) , 1, 0), .names = "EVER_{.col}"),
         across(all_of(demo_vars), ~as.factor(.)),
         across(contains("_przed_"), ~as.factor(.)))

# wykresy dla zmiennych demograficznych
NSDUH_2022_final_dataset %>%
  dplyr::select(matches("^.{6}$"), c("ANALWT2_C")) %>%
  pivot_longer(matches("^.{6}$"), 
               names_to = 'zmienna', 
               values_to = 'values') %>%
  ggplot() + geom_bar(aes(x = values, weight = ANALWT2_C)) + 
  facet_wrap(~zmienna, scales = "free") + 
  theme_minimal()

#rekodowanie zmiennych demograficznych
NSDUH_2022_final_dataset <- NSDUH_2022_final_dataset %>%
  mutate(
    ETHNIC = factor(case_when(
      ETHNIC %in% c(3, 4, 5, 6) ~ 4, # Łączenie kategorii 3,4,5,6
      ETHNIC == 1 ~ 1,
      ETHNIC == 2 ~ 2,
      ETHNIC == 7 ~ 3)
    ),
    RLGINF = factor(case_when(
      AGECAT == 1 & RLGIFY %in% c(1, 2, 3, 4) ~ RLGIFY,
      !(AGECAT == 1 ) & RLGIFA %in% c(1, 2, 3, 4) ~ RLGIFA,
      TRUE ~ NA), levels = 1:4),
    AGEPRC = factor(AGEPRC),
    GENDER = factor(GENDER)
    ) %>%
  dplyr::select(-c("RLGIFA", "RLGIFY", "AGECAT"))

# # tworzenie zmiennych do oszacowania wieku respondentów, oraz początku inicjacji
# NSDUH_2022_final_dataset <- NSDUH_2022_final_dataset %>%
#   mutate(across(matches("^.{3}$"), ~ if_else(. > 100, NA, .)),
#          MIN_INIT = do.call(pmin, c(across(all_of(softdrug_vars)), 
#                                     na.rm = TRUE)),
#          INIT_BFR_20 = if_else(MIN_INIT <= 20, 1, 0),
#          MAX_INIT = do.call(pmax, c(across(matches("^.{3}$")), na.rm = TRUE)),
#          AGEPRC_LOWER = case_when(AGEPRC == 1 ~ 12,
#                                   AGEPRC == 2 ~ 14,
#                                   AGEPRC == 3 ~ 16,
#                                   AGEPRC == 4 ~ 18,
#                                   AGEPRC == 5 ~ 21,
#                                   AGEPRC == 6 ~ 24,
#                                   AGEPRC == 7 ~ 26,
#                                   AGEPRC == 8 ~ 30,
#                                   AGEPRC == 9 ~ 35,
#                                   AGEPRC == 10 ~ 50,
#                                   AGEPRC == 11 ~ 65,
#                                   TRUE ~ NA_real_
#          ),
#          ESTAGE = pmax(AGEPRC_LOWER, MAX_INIT, na.rm = TRUE)) %>%
#   mutate(across(matches("^.{3}$"), ~ replace(., is.na(.), 999)))

demo_vars <- demo_vars[!demo_vars %in% c("RLGIFY", "RLGIFA")] 
demo_vars <- c(demo_vars, "RLGINF")


### tabela z podsumowaniem danych
basic_stats <- NSDUH_2022_final_dataset %>%
  skim()

names(basic_stats)

# sprawdzanie liczby obserwacji z NA
sum(apply(NSDUH_2022_final_dataset, 1, function(x) any(is.na(x))))
dim(NSDUH_2022_final_dataset)

missing_data <- basic_stats %>%
  dplyr::select(skim_variable, n_missing, complete_rate) %>%
  filter(n_missing > 0) %>%
  mutate(complete_rate = format(round((1 - complete_rate), 2), digits = 2)) %>%
  arrange(desc(n_missing)) %>%
  rename(
    Zmienna = skim_variable,
    N = n_missing,
    "Udział braków" = complete_rate
  )
print(missing_data)

stargazer(missing_data,
          float = FALSE,
          summary = FALSE,
          out = paste0(tabDir, "tab_00.tex"))

#saving data
save_path <- "../data/"
save(weighting_vars, file = paste0(save_path, "weighting_vars.Rdata"))
save(harddrug_vars, file = paste0(save_path, "harddrug_vars.Rdata"))
save(softdrug_vars, file = paste0(save_path, "softdrug_vars.Rdata"))
save(imputation_ind, file = paste0(save_path, "imputation_ind.Rdata"))
save(demo_vars, file = paste0(save_path, "demo_vars.Rdata"))
save(NSDUH_2022_final_dataset, 
     file = paste0(save_path, "NSDUH_2022_final_dataset.Rdata"))


### imputacja danych; z jakiegoś powodu pakiet działa tylko na świeżej sesji R
setwd("~/projekty_git/magisterska/code_r")
library(missForest)
load("../data/NSDUH_2022_final_dataset.Rdata")
imputed <- missForest(NSDUH_2022_final_dataset)

NSDUH_2022_final_dataset <- imputed$ximp

save_path <- "../data/"
save(NSDUH_2022_final_dataset, 
     file = paste0(save_path, "NSDUH_2022_final_dataset.Rdata"))  