### Czyszczenie
rm(list = ls())

### Wczytanie zbioru danych
load("../data/NSDUH_2022_final_dataset.Rdata")

### Ścieżki
tabDir <- "../paper/tabs/"

library(dplyr)


# odfiltrowanie zbioru usuwajac zmienne demograficzne i wiersze z brakiem 
# jakiejkolwiek inicjacji
NSDUH_guttman_data <- NSDUH_2022_final_dataset %>%
  select(matches("^.{3}$"), contains("EVER_"), "ANALWT2_C", "AGECAT") %>%
  mutate(across(matches("^.{3}$"), ~ if_else(is.na(.), 999, .))) %>%
  filter(rowSums(across(contains("EVER_"), ~ . != 0)) > 0) %>%
  mutate(NIC = min(FSM, VAP))

#funkcja do liczenia CR i CS dla skali guttmana
hdrug_guttman <- function(data, hdrug, nicotine){
  
  # wybór kolumn z wiekiem inicjacji i odfiltrowanie wierszy bez jakiejkolwiek 
  # inicjacji
  data = data %>%
    select(all_of(c("ALC", "FSM", "NIC", "MRJ", hdrug, 
             "EVER_ALC", "EVER_FSM", "EVER_VAP", "EVER_MRJ", paste0("EVER_", hdrug)))) %>%
    filter(if_any(contains("EVER_"), ~ . != 0))
  
  ### warunek sprawdzający czy agregować palenie i vaping
  if (nicotine == TRUE) {
  data = data %>%
    select(all_of(c("NIC", "ALC", "MRJ", hdrug)))
  }else{
  data = data %>%
    select(all_of(c("FSM", "ALC", "MRJ", hdrug)))
  }
    
  # sprawdzenie czy wiek rośnie zgodnie z badaną sekwencją inicjacji brak 
  # inicjacji jest oznaczony jako 993 zatem zaburza uporządkowanie jeśli 
  # wystąpi w sekwencji przed inicjacją "twardszej" używki
  order_check = apply(data, 1, function(ages) {
    
    if (is.unsorted(ages)) {
      return(1) 
    } else {
      return(0)
    }
  })
  
  # współczynnik reprodukowalności
  total_errors = sum(order_check)
  total_responses = nrow(data) * ncol(data)
  CR <- 1 - (total_errors/total_responses)
  
  #współczynnik skalowalnosci

  # maksymalna liczba błędów dla obiektów
  modal_frequencies_items <- apply(data, 2, 
                                   function(col) {max(table(col))}
                                   )
  max_errors_items <- total_responses - sum(modal_frequencies_items)
  
  # maksymalna liczba błędów dla respondentów
  modal_frequencies_individuals <- apply(data, 1, 
                                         function(row) {max(table(row))}
                                         )
  max_errors_individuals <- total_responses - sum(modal_frequencies_individuals)
  
  # dobranie bardziej konserwatywnej miary maks błędów
  max_errors <- min(max_errors_items, max_errors_individuals)
  CS = 1 - (total_errors/max_errors)
  
  return(data.frame(CR = CR, CS = CS))
}

### obliczanie wskaźników dla wszystkich używek
hdrugs <- c("COC", "HER", "CRK", "HAL", "SED", "TRQ", "PNK", "STM", "MTH")

# samo palenie
guttman_results <- sapply(hdrugs, function(hdrug) hdrug_guttman(NSDUH_guttman_data, hdrug, nicotine = FALSE), simplify = FALSE)
results_transposed <- t(do.call(rbind, guttman_results))
results_rounded <- round(results_transposed, 3)

# nikotyna
guttman_results_nicotine <- sapply(hdrugs, function(hdrug) hdrug_guttman(NSDUH_guttman_data, hdrug, nicotine = TRUE), simplify = FALSE)
results_transposed_nicotine <- t(do.call(rbind, guttman_results_nicotine))
results_rounded_nicotine <- round(results_transposed_nicotine, 3)

# zapisywanie tabeli z wynikami
stargazer(results_rounded,
          float = FALSE,
          summary = FALSE,
          out = paste0(tabDir, "tab_guttman.tex"))

# zapisywanie tabeli z wynikami dla nikotyny
stargazer(results_rounded_nicotine,
          float = FALSE,
          summary = FALSE,
          out = paste0(tabDir, "tab_guttman_nikotyna.tex"))


