### working directory
setwd("~/projekty_git/magisterska/code_r")

### libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(stargazer)

### model names
load("../data/harddrug_vars.Rdata")
harddrug_vars <- c("HDG", harddrug_vars)

### directories
tabDir <- "../paper/tabs/"
pltDir <- "../paper/plots/"

# ### f for binding boot data
# combine_metrics <-
#   function(hard_drug) {

#     # filenames
#     modDir <- "../data/model_results/"
#     metrics_list <- lapply(1:1000, function(i) {
#       metric_file_path <- paste0(modDir,
#                                  hard_drug,
#                                  "_fit_metrics_", i,
#                                  ".RData")
#       coefs_file_path <- paste0(modDir,
#                                 hard_drug,
#                                 "_coefs_", i,
#                                 ".RData")

#       # load and bind
#       load(metric_file_path)
#       load(coefs_file_path)
#       combined_data <- cbind(metrics_example,
#                              coefs_extracted,
#                              key_var = as.vector(hard_drug))

#       return(combined_data)
#     })

#     #bind everything
#     combined_metrics <- do.call(rbind, metrics_list)
#     return(combined_metrics)
#   }

# ### binding data
# metrics_list <- lapply(harddrug_vars, combine_metrics)
# names(metrics_list) <- harddrug_vars

# ### saving binded file
# save(metrics_list,
#      file = "../data/boot_df_results_list.RData")

load("../data/boot_df_results_list.RData")

### func for summarizing data
plots_metrics_tables <- function(hard_drug) {

  ### preparing data
  metric_summary = metrics_list[[hard_drug]] %>%
    select(-key_var) %>%
    pivot_longer(everything(),
                 names_to = "var",
                 values_to = "val") %>%
    group_by(var)

  ### order for variables
  desired_order = c(
    "(Intercept)",
    "ALC_przed1", "VAP_przed1", "SMK_przed1", "SMK_przed2", "MRJ_przed1",
    "GENDER2", "ETHNIC2", "ETHNIC3", "ETHNIC4",
    "RLGINF2", "RLGINF3", "RLGINF4"
  )

  ### summary for coeffs
  param_est = metric_summary %>%
    summarize(Mean = mean(exp(val)),
              SD = sd(exp(val)),
              p_val = 2 * min(mean(val > 0), mean(val < 0))) %>%
    filter(!(var %in%
               c("R_NK",
                 "R_boot",
                 "Recall",
                 "Precision",
                 "F1_Score",
                 "AUC"))) %>%
    mutate(var = factor(var, levels = desired_order)) %>%
    arrange(var) %>%
    mutate(
      Zmienna = as.character(var),
      Odds_ratio = round(Mean, 3),
      SE = round(SD, 3),
      p_val = round(p_val, 4)
    ) %>%
    select(Zmienna, Odds_ratio, SE, p_val) %>%
    rename("Iloraz szans (OR)" = Odds_ratio)

  stargazer(param_est,
            float = FALSE,
            type = "latex",
            title = "Podsumowanie estymowanych parametrów",
            summary = FALSE,
            digits = 3,
            out = paste0(tabDir, "tab_", hard_drug, "_boot_coefs.tex"))

  metrics_order = c(
    "R_NK", "Precision", "Recall", "F1_Score", "AUC"
  )

  ### summary for fit metrics
  metrics_est = metric_summary %>%
    summarize(Mean = mean(val),
              SD = sd(val)) %>%
    filter((var %in%
              c("R_NK", "Recall", "Precision", "F1_Score", "AUC"))) %>%
    mutate(
      var = factor(var, levels = metrics_order),
      Mean = as.numeric(as.character(round(Mean, 4))),
      SD = round(SD, 4)
    ) %>%
    arrange(var) %>%
    mutate(var = as.character(var)) %>%
    mutate(
          var = case_when(
                          var == "Recall" ~ "Czułość",
                          var == "Precision" ~ "Precyzja",
                          var == "F1_Score" ~ "F1 score",
                          var == "R_NK" ~ "R_{NK}",
                          TRUE ~ var
    )) %>%    
    rename(Metryka = var,
           "Wartość" = Mean)

  stargazer(as.data.frame(metrics_est),
            float = FALSE,
            summary = FALSE,
            out = paste0(tabDir, "tab_", hard_drug, "_boot_fit.tex"))

# Reading the entire file as a single string
file_path <- paste0(tabDir, "tab_", hard_drug, "_boot_fit.tex")
file_raw <- readChar(file_path, file.info(file_path)$size)

# Poprawiona zamiana wzorca
file_fixed <- gsub("R\\\\_\\\\\\{NK\\\\\\}", "\\\n\\\\ensuremath{R^{2}_{NK}}", file_raw)
file_fixed

# Zapisanie poprawionego pliku
writeLines(file_fixed, paste0(tabDir, "tab_", hard_drug, "_boot_fit.tex"), useBytes = TRUE)

}
### summary for all models
lapply(harddrug_vars, plots_metrics_tables)