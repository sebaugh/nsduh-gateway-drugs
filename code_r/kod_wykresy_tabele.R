### Czyszczenie
rm(list = ls())

### Biblioteki
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(stargazer)
library(skimr)
library(stringr)
library(viridis)

### Wczytanie zbioru danych
load("../data/NSDUH_2022_final_dataset.Rdata")
load("../data/harddrug_vars.Rdata")

### Ścieżki
tabDir <- "../paper/tabs/"
pltDir <- "../paper/plots/"


# wykres do zmiennych indykatorów imputacji
odsetki_imputowanych <- NSDUH_2022_final_dataset %>%
  dplyr::select(contains("_IMP"), c("ANALWT2_C")) %>%
  summarise(across(contains("_IMP"), 
                   ~ mean(.) * 100)) %>%
#                   ~ sum(. * ANALWT2_C) / sum(ANALWT2_C) * 100)) %>%
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
ggsave(paste0(pltDir, "imputacje.png"), 
       plot = odsetki_imputowanych, dpi = 300)



# ważony wykres do odsetka inicjacji w populacji dla używki
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
           str_detect(Drug, "EVER_(ALC|FSM|MRJ|VAP|ESM)") ~ "Miękkie",
           str_detect(Drug, "EVER_(HDG|NMD|MED)") ~ "Twarde agregat",
           TRUE ~ "Twarde")) %>%
ggplot(aes(x = Proportion, y = Drug, fill = Kategoria)) +
  geom_bar(stat = "identity", alpha = 0.9) +
  geom_text(aes(label = sprintf("%.1f", Proportion)), 
            hjust = -0.2, size = 4, color = "black") +
  scale_x_continuous(limits = c(0, 100)) +
  labs(
    x = "Odsetek (%)",
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



# nieważony wykres do odsetka inicjacji w populacji dla używki
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
           str_detect(Drug, "EVER_(ALC|FSM|MRJ|VAP|ESM)") ~ "Miękkie",
           str_detect(Drug, "EVER_(HDG|NMD|MED)") ~ "Twarde agregat",
           TRUE ~ "Twarde")) %>%
  ggplot(aes(x = Proportion, y = Drug, fill = Kategoria)) +
  geom_bar(stat = "identity", alpha = 0.9) +
  geom_text(aes(label = sprintf("%.1f", Proportion)), 
            hjust = -0.2, size = 4, color = "black") +
  scale_x_continuous(limits = c(0, 100)) +
  labs(
    x = "Odsetek (%)",
    y = ""
  ) +
  theme_minimal() +
  scale_fill_viridis(discrete=TRUE) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 12),  # Rozmiar tekstu legendy
    legend.title = element_text(size = 14, face = "bold"), 
    
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12)) 
ggsave(paste0(pltDir, "drugs_ever_taken_noweights.png"), 
       plot = plot_drugs_ever_taken_no_weights, dpi = 300)


# rozkłady wieku inicjacji ważone
age_density_if_initiated <- NSDUH_2022_final_dataset %>%
  dplyr::select(matches("^.{3}$"), c("ANALWT2_C")) %>%
  pivot_longer(matches("^.{3}$"), names_to = 'drugs', values_to = 'age') %>%
  mutate(Kategoria = case_when(
    drugs %in% c("ALC", "FSM", "MRJ", "VAP", "ESM") ~ "Miękkie",
    drugs %in% c("HDG", "MED", "NMD") ~ "Twarde agregat",
    drugs %in% c("COC", "HER", "CRK", "HAL", "MTH", 
                 "PNK", "STM", "TRQ", "SED") ~ "Twarde"),
    drugs = factor(drugs, 
                   levels = c("ALC", "FSM", "MRJ", "VAP", "ESM", "HDG", "NMD", 
                              "COC", "HER", "CRK", "HAL", "MTH", "MED", "PNK", 
                              "STM", "TRQ", "SED"))) %>%
  filter(age < 100) %>%
  ggplot(aes(drugs, age, weight = ANALWT2_C)) + 
  geom_violin(aes(fill = Kategoria), alpha = 0.7) +
  geom_boxplot(aes(color = Kategoria), alpha = 0.5) + 
  labs(x = "Używka", y = "Wiek") +
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


# wykresy dla zmiennych demograficznych ważone
demographic_distr_data <- NSDUH_2022_final_dataset %>%
  dplyr::select(matches("^.{6}$"), c("ANALWT2_C")) %>%
  dplyr::select(-any_of(c("ESTAGE"))) %>%
  pivot_longer(matches("^.{6}$"), 
               names_to = 'zmienna', 
               values_to = 'values') %>%
  mutate(values = factor(values, , levels = 1:11, ordered = TRUE)) %>%
  mutate(label = case_when(
    zmienna == "GENDER" & values == 1 ~ "M",
    zmienna == "GENDER" & values == 2 ~ "K",
    
    zmienna == "AGEPRC" & values == 1 ~ "12-13",
    zmienna == "AGEPRC" & values == 2 ~ "14-15",
    zmienna == "AGEPRC" & values == 3 ~ "16-17",
    zmienna == "AGEPRC" & values == 4 ~ "18-20",
    zmienna == "AGEPRC" & values == 5 ~ "21-23",
    zmienna == "AGEPRC" & values == 6 ~ "24-25",
    zmienna == "AGEPRC" & values == 7 ~ "26-29",
    zmienna == "AGEPRC" & values == 8 ~ "30-34",
    zmienna == "AGEPRC" & values == 9 ~ "35-49",
    zmienna == "AGEPRC" & values == 10 ~ "50-64",
    zmienna == "AGEPRC" & values == 11 ~ "65+",
    
    zmienna == "ETHNIC" & values == 1 ~ "Biali",
    zmienna == "ETHNIC" & values == 2 ~ "Czarni",
    zmienna == "ETHNIC" & values == 3 ~ "Latynosi",
    zmienna == "ETHNIC" & values == 4 ~ "Inne",
    
    zmienna == "TOTINC" & values == 1 ~ "≤ $20k",
    zmienna == "TOTINC" & values == 2 ~ "$20k–$49k",
    zmienna == "TOTINC" & values == 3 ~ "$50k–$74k",
    zmienna == "TOTINC" & values == 4 ~ "$75k+",
    
    zmienna == "POPDEN" & values == 1 ~ "CBSA ≥1 mln",
    zmienna == "POPDEN" & values == 2 ~ "CBSA <1 mln",
    zmienna == "POPDEN" & values == 3 ~ "Poza CBSA",
    
    zmienna == "EDUCAT" & values == 1 ~ "Podstaw.",
    zmienna == "EDUCAT" & values == 2 ~ "Średnie",
    zmienna == "EDUCAT" & values == 3 ~ "Stud. cz.",
    zmienna == "EDUCAT" & values == 4 ~ "Wyższe",
    zmienna == "EDUCAT" & values == 5 ~ "12–17 lat",
    
    zmienna == "RLGINF" & values == 1 ~ "1 Z. Nie",
    zmienna == "RLGINF" & values == 2 ~ "2 Nie",
    zmienna == "RLGINF" & values == 3 ~ "3 Tak",
    zmienna == "RLGINF" & values == 4 ~ "4 Z. Tak",
    
    TRUE ~ as.character(values)
  ))

demographic_distr <- demographic_distr_data %>%
  ggplot() + 
  geom_bar(aes(x = label, 
               weight = ANALWT2_C, 
               y = after_stat(prop), group = 1)
           ) + 
  facet_wrap(~zmienna, scales = "free") + 
  scale_fill_viridis(discrete=TRUE) +
  theme_minimal() + 
  labs(x = "Kategoria",
       y = "Udział") + 
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.title = element_text(face = "bold")
  )
ggsave(paste0(pltDir, "demo_dist.png"), 
       plot = demographic_distr, dpi = 300)



# wykresy dla zmiennych demograficznych
demographic_distr_no_weights <- demographic_distr_data %>%
  ggplot() + 
  geom_bar(aes(x = label, 
               y = after_stat(prop), group = 1)
  ) + 
  facet_wrap(~zmienna, scales = "free") + 
  scale_fill_viridis(discrete=TRUE) +
  theme_minimal() + 
  labs(x = "Kategoria",
       y = "Udział") + 
  theme(
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90)
  )
ggsave(paste0(pltDir, "demo_dist_no_weights.png"), 
       plot = demographic_distr_no_weights, dpi = 300)


# Lista do przechowywania wyników
results_list <- list()

# Pętla po każdej zmiennej z harddrugs
for (drug in harddrug_vars) {
  # Nazwa zmiennej EVER_{drug}
  ever_var <- paste0("EVER_", drug)
  # Zmienna zawierająca wszystkie pasujące przed_{drug}
  przed_vars <- grep(paste0("przed_", drug), 
                     names(NSDUH_2022_final_dataset), 
                     value = TRUE)
  
   # Krzyżowanie
    temp_result <- NSDUH_2022_final_dataset %>%
      select(all_of(c(ever_var, przed_vars))) %>%
      filter(complete.cases((.))) %>%
      pivot_longer(
        cols = all_of(przed_vars),
        names_to = "przed_var",
        values_to = "przed_value"
      ) %>%
      group_by(across(all_of(c(ever_var, "przed_var", "przed_value")))) %>%
      summarise(Frequency = n(), .groups = "drop") %>%
      arrange(przed_var)
    
    # Dodanie wyników do listy
    results_list[[drug]] <- temp_result
}

### ważone wykresy zależności inicjacji od zmiennych objasniajacych
ever_rest_relationships <- NSDUH_2022_final_dataset %>%
  select(-matches("^.{3}$"), 
         -contains("_IMP"), 
         -all_of(c("VEREP", "VESTR_C"))) %>%
  pivot_longer(cols = starts_with("EVER_"),
               names_to = "Uzywka",
               values_to = "Inicjacja") %>%
  mutate(Uzywka = gsub("EVER_", "", Uzywka)) %>%
  filter(Uzywka %in% c(harddrug_vars, "HDG")) %>%
  pivot_longer(-any_of(c("ANALWT2_C", "Uzywka", "Inicjacja")),
               names_to = "variable",
               values_to = "categories") %>%
  filter(str_sub(variable, -3) == Uzywka | 
           variable %in% c("ETHNIC", "GENDER", "RLGINF")) %>%
  mutate(variable = str_remove(variable, paste0("_przed_", Uzywka)),
         categories = as.factor(categories)) %>%
  group_by(Uzywka, variable, categories) %>%
  summarize(odsetek = sum(Inicjacja * ANALWT2_C) / sum(ANALWT2_C), 
            .groups = "keep")


demo_relationships <- ever_rest_relationships %>%
  filter(variable %in% c("ETHNIC", "GENDER", "RLGINF")) %>%
  mutate(
    categories = case_when(
      variable == "ETHNIC" & categories == 1 ~ "Biali",
      variable == "ETHNIC" & categories == 2 ~ "Czarni",
      variable == "ETHNIC" & categories == 3 ~ "Latynosi",
      variable == "ETHNIC" & categories == 4 ~ "Inni",
      variable == "GENDER" & categories == 1 ~ "Płeć M",
      variable == "GENDER" & categories == 2 ~ "Płeć K",
      variable == "RLGINF" & categories == 1 ~ "R1 Z. Nie",
      variable == "RLGINF" & categories == 2 ~ "R2 Nie",
      variable == "RLGINF" & categories == 3 ~ "R3 Tak",
      variable == "RLGINF" & categories == 4 ~ "R4 Z. Tak"
    )
  )


soft_relationships <- ever_rest_relationships %>%
  filter(variable %in% c("ALC", "SMK", "MRJ", "VAP")) %>%
  mutate(categories = ifelse(categories == 0, " brak inicjacji",
                             ifelse(categories == 1, " inicjacja",
                                    " nałóg")))

### średnie wartości
mean_values <- NSDUH_2022_final_dataset %>%
  summarise(across(starts_with("EVER_"), ~ sum(. * ANALWT2_C) / sum(ANALWT2_C))) %>%
  pivot_longer(everything(),
               names_to = "Uzywka",
               values_to = "Ogolem") %>%
  mutate(Uzywka = str_remove(Uzywka, "EVER_"))

### wykres dla zmiennych "_przed_"
hdrugs_soft_rel <- soft_relationships %>%
  left_join(mean_values, by = "Uzywka") %>%
  #  filter(Uzywka %in% c("COC")) %>%
  ggplot(aes(x = interaction(categories, variable), 
             y = odsetek, 
             fill = variable, 
             group = variable)) + 
  geom_col(linewidth = 3, alpha = 0.8) +
  scale_x_discrete(labels = 
                     c("Br. Inic.", "Inic.", 
                       "Br. Inic.", "Inic.", 
                       "Br. Inic.", "Inic.", "Nałóg", 
                       "Br. Inic.", "Inic.")) + 
  geom_hline(aes(yintercept = Ogolem)) +
  scale_fill_viridis(discrete = TRUE) +
  theme_minimal() +
  theme(
    legend.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "top"
  ) + 
  labs(x = "Status inicjacji",
       y = "Odsetek inicjacji twardą używką (%)",
       fill = "Miękka używka") +
  facet_wrap(~Uzywka, scales = "free", ncol = 3)
ggsave(paste0(pltDir, "hdrugs_soft_relation.png"), 
       plot = hdrugs_soft_rel, dpi = 300)


### wykres dla demografii
hdrugs_demo_rel <- demo_relationships %>%
  left_join(mean_values, by = "Uzywka") %>%
  ggplot(aes(x = categories, y = odsetek, fill = variable)) + 
  geom_col(linewidth = 3, alpha = 0.8, position = "dodge") +
  geom_hline(aes(yintercept = Ogolem)) +
  scale_color_viridis(discrete = TRUE) +
  theme_minimal() +
  theme(
    legend.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "top"
  ) + 
  scale_fill_viridis(discrete = TRUE) + 
  labs(x = "Kategorie demograficzne",
       y = "Odsetek inicjacji twardą używką (%)",
       fill = "Zmienna") + 
  facet_wrap(~Uzywka, scales = "free", ncol = 3)
ggsave(paste0(pltDir, "hdrugs_demo_relation.png"), 
       plot = hdrugs_demo_rel, dpi = 300)