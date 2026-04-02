### Cleaning environment
rm(list = ls())

### Load dataset
load("../data/NSDUH_2022_final_dataset.Rdata")

### paths
tabDir <- "../paper/tabs/"

### libraries
library(dplyr)


# removing demographic variables and rows with no drug use ever
NSDUH_guttman_data <- NSDUH_2022_final_dataset %>%
  select(matches("^.{3}$"), contains("EVER_"), "ANALWT2_C", "AGECAT") %>%
  mutate(across(matches("^.{3}$"), ~ if_else(is.na(.), 999, .))) %>%
  filter(rowSums(across(contains("EVER_"), ~ . != 0)) > 0) %>%
  mutate(NIC = min(FSM, VAP))

# function to calculate CR i CS for guttman scale
hdrug_guttman <- function(data, hdrug, nicotine){
  
  # selecting rows with first time age use and filtering out no use ever 
  data = data %>%
    select(all_of(c("ALC", "FSM", "NIC", "MRJ", hdrug, 
             "EVER_ALC", "EVER_FSM", "EVER_VAP", "EVER_MRJ", paste0("EVER_", hdrug)))) %>%
    filter(if_any(contains("EVER_"), ~ . != 0))
  
  ### condition to aggregate smoking and vaping
  if (nicotine == TRUE) {
  data = data %>%
    select(all_of(c("NIC", "ALC", "MRJ", hdrug)))
  }else{
  data = data %>%
    select(all_of(c("FSM", "ALC", "MRJ", hdrug)))
  }
    
  # check if age is increasing according to the initiation sequence
  # no use ever = 993, disrupts the sequence if occurs before "harder" drug
  order_check = apply(data, 1, function(ages) {
    
    if (is.unsorted(ages)) {
      return(1) 
    } else {
      return(0)
    }
  })
  
  # coefficient of reproducibility
  total_errors = sum(order_check)
  total_responses = nrow(data) * ncol(data)
  CR <- 1 - (total_errors/total_responses)
  
  #scaling coefficient

  # maximum count of errors for an item
  modal_frequencies_items <- apply(data, 2, 
                                   function(col) {max(table(col))}
                                   )
  max_errors_items <- total_responses - sum(modal_frequencies_items)
  
  # maximum count of errors per individual
  modal_frequencies_individuals <- apply(data, 1, 
                                         function(row) {max(table(row))}
                                         )
  max_errors_individuals <- total_responses - sum(modal_frequencies_individuals)
  
  # choose more conservative count of errors
  max_errors <- min(max_errors_items, max_errors_individuals)
  CS = 1 - (total_errors/max_errors)
  
  return(data.frame(CR = CR, CS = CS))
}

### compute coefficients for all drugs
hdrugs <- c("COC", "HER", "CRK", "HAL", "SED", "TRQ", "PNK", "STM", "MTH")

# smoking with no vaping
guttman_results <- sapply(hdrugs, function(hdrug) hdrug_guttman(NSDUH_guttman_data, hdrug, nicotine = FALSE), simplify = FALSE)
results_transposed <- t(do.call(rbind, guttman_results))
results_rounded <- round(results_transposed, 3)

# nicotine = smoking + vaping
guttman_results_nicotine <- sapply(hdrugs, function(hdrug) hdrug_guttman(NSDUH_guttman_data, hdrug, nicotine = TRUE), simplify = FALSE)
results_transposed_nicotine <- t(do.call(rbind, guttman_results_nicotine))
results_rounded_nicotine <- round(results_transposed_nicotine, 3)

# save results for smoking with no vaping
stargazer(results_rounded,
          float = FALSE,
          summary = FALSE,
          out = paste0(tabDir, "tab_guttman.tex"))

# save results for nicotine = smoking + vaping
stargazer(results_rounded_nicotine,
          float = FALSE,
          summary = FALSE,
          out = paste0(tabDir, "tab_guttman_nikotyna.tex"))


