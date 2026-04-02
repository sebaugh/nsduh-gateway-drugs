### cleaning the environment
rm(list = ls())

### libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(stargazer)
library(skimr)
library(stringr)
library(viridis)

### loading data
load("../data/NSDUH_2022_final_dataset.Rdata")
load("../data/harddrug_vars.Rdata")

### paths
tabDir <- "../paper/tabs/"
pltDir <- "../paper/plots/"


# plot for the imputation variables
imputation_share <- NSDUH_2022_final_dataset %>%
  dplyr::select(contains("_IMP"), c("ANALWT2_C")) %>%
  summarise(across(contains("_IMP"), 
                   ~ mean(.) * 100)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "values") %>%
  arrange(values) %>%
  mutate(variable = factor(variable, levels = variable)) %>%
  ggplot(aes(x = variable, y = values)) + 
  geom_bar(stat = "identity", 
           alpha = 0.9) + 
  geom_text(aes(label = sprintf("%.2f", values)), 
            hjust = -0.2,                          
            size = 4,                               
            color = "black") +  
  scale_y_continuous(limits = c(0, 100)) +
  labs(x = "",
       y = "Odsetek (%)") +
  theme_minimal() + 
  theme(
    legend.position = "none",
    axis.text.x = element_text(hjust = 1),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 12)
  ) + 
  scale_fill_viridis(discrete=TRUE) +
  coord_flip()
ggsave(paste0(pltDir, "imputations.png"), 
       plot = imputation_share, dpi = 300)



# weighed plot for the initiation share in the population
plot_drugs_ever_taken <- NSDUH_2022_final_dataset %>%
  summarise(across(starts_with("EVER_"), 
                   ~ sum(. * ANALWT2_C) / sum(ANALWT2_C), 
                   .names = "{.col}")) %>%
  pivot_longer(everything(), names_to = "Drug", 
               values_to = "Proportion") %>%
  mutate(Proportion = Proportion * 100, 
         Drug = factor(Drug, 
                       levels = Drug[order(Proportion)]),
         Kategoria = case_when(
           str_detect(Drug, "EVER_(ALC|FSM|MRJ|VAP|ESM)") ~ "'Soft'",
           str_detect(Drug, "EVER_(HDG|NMD|MED)") ~ "'Hard' aggregated",
           TRUE ~ "'Hard'")) %>%
ggplot(aes(x = Proportion, y = Drug, fill = Kategoria)) +
  geom_bar(stat = "identity", alpha = 0.9) +
  geom_text(aes(label = sprintf("%.1f", Proportion)), 
            hjust = -0.2, size = 4, color = "black") +
  scale_x_continuous(limits = c(0, 100)) +
  labs(
    x = "Share (%)",
    y = ""
  ) +
  theme_minimal() +
  scale_fill_viridis(discrete=TRUE) +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold"), 
    axis.title = element_text(face = "bold"))
ggsave(paste0(pltDir, "drugs_ever_taken.png"), 
       plot = plot_drugs_ever_taken, dpi = 300)



# unweighed plot for the initiation share in the population
plot_drugs_ever_taken_no_weights <- NSDUH_2022_final_dataset %>%
  summarise(across(starts_with("EVER_"), 
                   ~ mean(.), 
                   .names = "{.col}")) %>%
  pivot_longer(everything(), names_to = "Drug", 
               values_to = "Proportion") %>%
  mutate(Proportion = Proportion * 100, 
         Drug = factor(Drug, 
                       levels = Drug[order(Proportion)]),
         Kategoria = case_when(
           str_detect(Drug, "EVER_(ALC|FSM|MRJ|VAP|ESM)") ~ "'Soft'",
           str_detect(Drug, "EVER_(HDG|NMD|MED)") ~ "'Hard' aggregated",
           TRUE ~ "'Hard'")) %>%
  ggplot(aes(x = Proportion, y = Drug, fill = Kategoria)) +
  geom_bar(stat = "identity", alpha = 0.9) +
  geom_text(aes(label = sprintf("%.1f", Proportion)), 
            hjust = -0.2, size = 4, color = "black") +
  scale_x_continuous(limits = c(0, 100)) +
  labs(
    x = "Share (%)",
    y = ""
  ) +
  theme_minimal() +
  scale_fill_viridis(discrete=TRUE) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 12), 
    legend.title = element_text(size = 14, face = "bold"), 
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12)) 
ggsave(paste0(pltDir, "drugs_ever_taken_noweights.png"), 
       plot = plot_drugs_ever_taken_no_weights, dpi = 300)


# weighed distributions of first time use age
age_density_if_initiated <- NSDUH_2022_final_dataset %>%
  dplyr::select(matches("^.{3}$"), c("ANALWT2_C")) %>%
  pivot_longer(matches("^.{3}$"), names_to = 'drugs', values_to = 'age') %>%
  mutate(Category = case_when(
    drugs %in% c("ALC", "FSM", "MRJ", "VAP", "ESM") ~ "'Soft'",
    drugs %in% c("HDG", "MED", "NMD") ~ "'Hard' aggregated",
    drugs %in% c("COC", "HER", "CRK", "HAL", "MTH", 
                 "PNK", "STM", "TRQ", "SED") ~ "'Hard'"),
    drugs = factor(drugs, 
                   levels = c("ALC", "FSM", "MRJ", "VAP", "ESM", "HDG", "NMD", 
                              "COC", "HER", "CRK", "HAL", "MTH", "MED", "PNK", 
                              "STM", "TRQ", "SED"))) %>%
  filter(age < 100) %>%
  ggplot(aes(drugs, age, weight = ANALWT2_C)) + 
  geom_violin(aes(fill = Category), alpha = 0.7) +
  geom_boxplot(aes(color = Category), alpha = 0.5) + 
  labs(x = "Drug", y = "Age") +
  theme_minimal() + 
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold"), 
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(breaks = seq(0, 90, by = 5))
ggsave(paste0(pltDir, "age_density.png"), 
       plot = age_density_if_initiated, width = 10, height = 10, dpi = 300)


# weighed demographic plots
demographic_distr_data <- NSDUH_2022_final_dataset %>%
  dplyr::select(matches("^.{6}$"), c("ANALWT2_C")) %>%
  dplyr::select(-any_of(c("ESTAGE"))) %>%
  pivot_longer(matches("^.{6}$"), 
               names_to = 'variable', 
               values_to = 'values') %>%
  mutate(values = factor(values, , levels = 1:11, ordered = TRUE)) %>%
  mutate(label = case_when(
    variable == "GENDER" & values == 1 ~ "M",
    variable == "GENDER" & values == 2 ~ "F",
    
    variable == "AGEPRC" & values == 1 ~ "12-13",
    variable == "AGEPRC" & values == 2 ~ "14-15",
    variable == "AGEPRC" & values == 3 ~ "16-17",
    variable == "AGEPRC" & values == 4 ~ "18-20",
    variable == "AGEPRC" & values == 5 ~ "21-23",
    variable == "AGEPRC" & values == 6 ~ "24-25",
    variable == "AGEPRC" & values == 7 ~ "26-29",
    variable == "AGEPRC" & values == 8 ~ "30-34",
    variable == "AGEPRC" & values == 9 ~ "35-49",
    variable == "AGEPRC" & values == 10 ~ "50-64",
    variable == "AGEPRC" & values == 11 ~ "65+",
    
    variable == "ETHNIC" & values == 1 ~ "White",
    variable == "ETHNIC" & values == 2 ~ "Black",
    variable == "ETHNIC" & values == 3 ~ "Hispanic",
    variable == "ETHNIC" & values == 4 ~ "Other",
    
    variable == "TOTINC" & values == 1 ~ "≤ $20k",
    variable == "TOTINC" & values == 2 ~ "$20k–$49k",
    variable == "TOTINC" & values == 3 ~ "$50k–$74k",
    variable == "TOTINC" & values == 4 ~ "$75k+",
    
    variable == "POPDEN" & values == 1 ~ "CBSA ≥1 mln",
    variable == "POPDEN" & values == 2 ~ "CBSA <1 mln",
    variable == "POPDEN" & values == 3 ~ "Outside CBSA",
    
    variable == "EDUCAT" & values == 1 ~ "Element.",
    variable == "EDUCAT" & values == 2 ~ "Średnie",
    variable == "EDUCAT" & values == 3 ~ "Stud. cz.",
    variable == "EDUCAT" & values == 4 ~ "Higher",
    variable == "EDUCAT" & values == 5 ~ "12–17 lat",
    
    variable == "RLGINF" & values == 1 ~ "1 S. No",
    variable == "RLGINF" & values == 2 ~ "2 No",
    variable == "RLGINF" & values == 3 ~ "3 Yes",
    variable == "RLGINF" & values == 4 ~ "4 S. Yes",
    
    TRUE ~ as.character(values)
  ))

demographic_distr <- demographic_distr_data %>%
  ggplot() + 
  geom_bar(aes(x = label, 
               weight = ANALWT2_C, 
               y = after_stat(prop), group = 1)
           ) + 
  facet_wrap(~variable, scales = "free") + 
  scale_fill_viridis(discrete=TRUE) +
  theme_minimal() + 
  labs(x = "Category",
       y = "Share") + 
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.title = element_text(face = "bold")
  )
ggsave(paste0(pltDir, "demo_dist.png"), 
       plot = demographic_distr, dpi = 300)



# plots for demographic variables
demographic_distr_no_weights <- demographic_distr_data %>%
  ggplot() + 
  geom_bar(aes(x = label, 
               y = after_stat(prop), group = 1)
  ) + 
  facet_wrap(~variable, scales = "free") + 
  scale_fill_viridis(discrete=TRUE) +
  theme_minimal() + 
  labs(x = "Category",
       y = "Share") + 
  theme(
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90)
  )
ggsave(paste0(pltDir, "demo_dist_no_weights.png"), 
       plot = demographic_distr_no_weights, dpi = 300)


results_list <- list()

# loop over all harddrug variable
for (drug in harddrug_vars) {

  ever_var <- paste0("EVER_", drug)
  befor_vars <- grep(paste0("befor_", drug), 
                     names(NSDUH_2022_final_dataset), 
                     value = TRUE)
  
   # crossing
    temp_result <- NSDUH_2022_final_dataset %>%
      select(all_of(c(ever_var, befor_vars))) %>%
      filter(complete.cases((.))) %>%
      pivot_longer(
        cols = all_of(befor_vars),
        names_to = "befor_var",
        values_to = "befor_value"
      ) %>%
      group_by(across(all_of(c(ever_var, "befor_var", "befor_value")))) %>%
      summarise(Frequency = n(), .groups = "drop") %>%
      arrange(befor_var)
    
    # adding results
    results_list[[drug]] <- temp_result
}

### weighed plots of lifetime used by other variables
ever_rest_relationships <- NSDUH_2022_final_dataset %>%
  select(-matches("^.{3}$"), 
         -contains("_IMP"), 
         -all_of(c("VEREP", "VESTR_C"))) %>%
  pivot_longer(cols = starts_with("EVER_"),
               names_to = "Drug",
               values_to = "Initiation") %>%
  mutate(Drug = gsub("EVER_", "", Drug)) %>%
  filter(Drug %in% c(harddrug_vars, "HDG")) %>%
  pivot_longer(-any_of(c("ANALWT2_C", "Drug", "Initiation")),
               names_to = "variable",
               values_to = "categories") %>%
  filter(str_sub(variable, -3) == Drug | 
           variable %in% c("ETHNIC", "GENDER", "RLGINF")) %>%
  mutate(variable = str_remove(variable, paste0("_befor_", Drug)),
         categories = as.factor(categories)) %>%
  group_by(Drug, variable, categories) %>%
  summarize(odsetek = sum(Initiation * ANALWT2_C) / sum(ANALWT2_C), 
            .groups = "keep")


demo_relationships <- ever_rest_relationships %>%
  filter(variable %in% c("ETHNIC", "GENDER", "RLGINF")) %>%
  mutate(
    categories = case_when(
      variable == "ETHNIC" & categories == 1 ~ "White",
      variable == "ETHNIC" & categories == 2 ~ "Black",
      variable == "ETHNIC" & categories == 3 ~ "Hispanic",
      variable == "ETHNIC" & categories == 4 ~ "Other",
      variable == "GENDER" & categories == 1 ~ "Sex M",
      variable == "GENDER" & categories == 2 ~ "Sex F",
      variable == "RLGINF" & categories == 1 ~ "R1 S. No",
      variable == "RLGINF" & categories == 2 ~ "R2 No",
      variable == "RLGINF" & categories == 3 ~ "R3 Yes",
      variable == "RLGINF" & categories == 4 ~ "R4 S. Yes"
    )
  )


soft_relationships <- ever_rest_relationships %>%
  filter(variable %in% c("ALC", "SMK", "MRJ", "VAP")) %>%
  mutate(categories = ifelse(categories == 0, " never used",
                             ifelse(categories == 1, " used",
                                    " addiction")))

### mean values
mean_values <- NSDUH_2022_final_dataset %>%
  summarise(across(starts_with("EVER_"), ~ sum(. * ANALWT2_C) / sum(ANALWT2_C))) %>%
  pivot_longer(everything(),
               names_to = "Drug",
               values_to = "Overall") %>%
  mutate(Drug = str_remove(Drug, "EVER_"))

### plot for "_befor_" variables
hdrugs_soft_rel <- soft_relationships %>%
  left_join(mean_values, by = "Drug") %>%
  #  filter(Drug %in% c("COC")) %>%
  ggplot(aes(x = interaction(categories, variable), 
             y = odsetek, 
             fill = variable, 
             group = variable)) + 
  geom_col(linewidth = 3, alpha = 0.8) +
  scale_x_discrete(labels = 
                     c("Nvr. Used.", "Used.", 
                       "Nvr. Used.", "Used.", 
                       "Nvr. Used.", "Used.", "Addiction", 
                       "Nvr. Used.", "Used.")) + 
  geom_hline(aes(yintercept = Overall)) +
  scale_fill_viridis(discrete = TRUE) +
  theme_minimal() +
  theme(
    legend.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "top"
  ) + 
  labs(x = "Use status",
       y = "Share of lifetime 'hard' drug use (%)",
       fill = "'Soft' drug") +
  facet_wrap(~Drug, scales = "free", ncol = 3)
ggsave(paste0(pltDir, "hdrugs_soft_relation.png"), 
       plot = hdrugs_soft_rel, dpi = 300)


### demographic plot
hdrugs_demo_rel <- demo_relationships %>%
  left_join(mean_values, by = "Drug") %>%
  ggplot(aes(x = categories, y = odsetek, fill = variable)) + 
  geom_col(linewidth = 3, alpha = 0.8, position = "dodge") +
  geom_hline(aes(yintercept = Overall)) +
  scale_color_viridis(discrete = TRUE) +
  theme_minimal() +
  theme(
    legend.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "top"
  ) + 
  scale_fill_viridis(discrete = TRUE) + 
  labs(x = "Demographic cat.",
       y = "Share of lifetime users of the 'hard' drug (%)",
       fill = "Variable") + 
  facet_wrap(~Drug, scales = "free", ncol = 3)
ggsave(paste0(pltDir, "hdrugs_demo_relation.png"), 
       plot = hdrugs_demo_rel, dpi = 300)