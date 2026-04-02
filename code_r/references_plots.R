### Clean evironment
rm(list = ls())

### libraries
library(rvest)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stargazer)
library(stringr)
library(viridis)

### load dataset
load("../data/NSDUH_2022_final_dataset.Rdata")

### paths
tabDir <- "../paper/tabs/"
pltDir <- "../paper/plots/"

### link to the data on opioid overdose
cdc_url <- "https://www.cdc.gov/nchs/products/databriefs/db522.htm?os=firetvfno_journeystrue&ref=app"

cdc_webpage <- read_html(cdc_url)
cdc_overdose_tables <- html_table(cdc_webpage, fill = TRUE)

opioid_deaths_df <- cdc_overdose_tables[[4]]

# removing absolute value columns
opioid_deaths_cleaned <- opioid_deaths_df[, !grepl("Number", opioid_deaths_df[2, ])]
colnames(opioid_deaths_cleaned) <- c("Year",
                                     "Opioids aggregated", 
                                     "Heroin", 
                                     "Natural and half-synthetic opioids", 
                                     "Methadone", 
                                     "Synthetic opioids other than methadone")

opioid_deaths_cleaned <- opioid_deaths_cleaned[-c(1,2),]

opioid_deaths_plot_data <- opioid_deaths_cleaned %>%
  pivot_longer(-c("Rok"), names_to = "Category", values_to = "Deaths") %>%
  mutate(Rok = as.numeric(Rok),
         Deaths = as.numeric(Deaths))
opioid_deaths_plot <- opioid_deaths_plot_data %>%
  ggplot() +
  geom_line(aes(x = Rok, y = Deaths, color = Category), 
            linewidth = 1, 
            alpha = 0.8) +
  scale_x_continuous(
    breaks = seq(min(opioid_deaths_plot_data$Rok), 
                 max(opioid_deaths_plot_data$Rok), by = 2)) +
  labs(
    x = "Year",
    y = "Deaths per 100k. of standardised population.",
    color = "Category"
  ) +
  theme_minimal() +
  scale_color_viridis(discrete = TRUE) +
  theme(
    legend.position = c(0.05, 0.95),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = "black"),
    axis.title = element_text(size = 12, face = "bold")
  )
ggsave(paste0(pltDir, "cdc_opioid_od_deaths.png"), 
       plot = opioid_deaths_plot, width = 10, height = 6, dpi = 300)

