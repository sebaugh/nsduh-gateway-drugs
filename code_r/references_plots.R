### Czyszczenie
rm(list = ls())

### Biblioteki
library(rvest)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stargazer)
library(stringr)
library(viridis)

### Wczytanie zbioru danych
load("../data/NSDUH_2022_final_dataset.Rdata")

### Ścieżki
tabDir <- "../paper/tabs/"
pltDir <- "../paper/plots/"

### ścieżka do danych cdc o przedawkowaniach
cdc_url <- "https://www.cdc.gov/nchs/products/databriefs/db522.htm?os=firetvfno_journeystrue&ref=app"

cdc_webpage <- read_html(cdc_url)
cdc_overdose_tables <- html_table(cdc_webpage, fill = TRUE)

opioid_deaths_df <- cdc_overdose_tables[[4]]

# Usunięcie kolumn z wartościami absolutnymi
opioid_deaths_cleaned <- opioid_deaths_df[, !grepl("Number", opioid_deaths_df[2, ])]
colnames(opioid_deaths_cleaned) <- c("Rok",
                                     "Opioidy ogółem", 
                                     "Heroina", 
                                     "Opioidy naturalne i półsyntetyczne", 
                                     "Metadon", 
                                     "Opioidy syntetyczne inne niż metadon")

opioid_deaths_cleaned <- opioid_deaths_cleaned[-c(1,2),]

opioid_deaths_plot_data <- opioid_deaths_cleaned %>%
  pivot_longer(-c("Rok"), names_to = "Kategoria", values_to = "Zgony") %>%
  mutate(Rok = as.numeric(Rok),
         Zgony = as.numeric(Zgony))
opioid_deaths_plot <- opioid_deaths_plot_data %>%
  ggplot() +
  geom_line(aes(x = Rok, y = Zgony, color = Kategoria), 
            linewidth = 1, 
            alpha = 0.8) +
  scale_x_continuous(
    breaks = seq(min(opioid_deaths_plot_data$Rok), 
                 max(opioid_deaths_plot_data$Rok), by = 2)) +
  labs(
    x = "Rok",
    y = "Zgony na 100 tys. zestandaryzowanej populacji",
    color = "Kategoria"
  ) +
  theme_minimal() +
  scale_color_viridis(discrete = TRUE) +
  theme(
    legend.position = c(0.05, 0.95),      # Pozycja legendy (x, y) w skali od 0 do 1
    legend.justification = c(0, 1),      # Punkt wyrównania legendy (lewy górny róg)
    legend.background = element_rect(fill = "white", color = "black"), # Tło legendy
    axis.title = element_text(size = 12, face = "bold")
  )
ggsave(paste0(pltDir, "cdc_opioid_od_deaths.png"), 
       plot = opioid_deaths_plot, width = 10, height = 6, dpi = 300)

