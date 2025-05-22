##############################################
###### COVID-19 in EU/EEA 2020 - 2022 ########
##############################################


### MANUSCRIPT
### CAPTURING THE SPATIOTEMPORAL SPREAD OF COVID-19 IN 30 EUROPEAN COUNTRIES DURING 2020 - 2022

### Authors:
### Thi Huyen Trang Nguyen (1), Niel Hens (1,2), Christel Faes (1)

### Affiliation:
### (1) Data Science Institute, I-BioStat, Hasselt University, BE-3500 Hasselt, Belgium
### (2) Centre for Health Economic Research and Modelling Infectious Diseases, Vaccine and Infectious Disease Institute, University of Antwerp, BE-2000 Antwerpen, Belgium



# ====== GENERAL INFO ==========
### Document: LIBRARY - DATA SET UP
### Author: trangngpmd
### Date: 2024-10-08


# ====== LIBRARY ==========
library(surveillance)
library(hhh4addon)
library(hhh4contacts)

library(tidyverse)
library(zoo)
library(reshape2)
library(ggplot2)
library(sf)
library(readxl)
library(writexl)
library(summarytools)
library(GGally)
library(viridis)
library(ggforce)
library(cowplot)
library(gridExtra)
library(listr)
library(geosphere)


# ====== PATH ==========
path_data_raw <- "G:/My Drive/Projects/COVID19_EUEEA/data_raw/"
path_data_clean <- "G:/My Drive/Projects/COVID19_EUEEA/data_clean/"
path_model_fit <- "G:/My Drive/Projects/COVID19_EUEEA/model_fit/"
path_simulation <- "G:/My Drive/Projects/COVID19_EUEEA/simulation/"
path_simulation_results <- "G:/My Drive/Projects/COVID19_EUEEA/simulation_results/"
path_plot <- "G:/My Drive/Projects/COVID19_EUEEA/plot_output/"

# Author's note: please adjust the paths if you want to run the codes

# ====== COUNTRY LIST ==========
EU30_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus" , "Czechia", 
                    "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", 
                    "Hungary", "Iceland", "Ireland",  "Italy", "Latvia","Liechtenstein", 
                    "Lithuania", "Luxembourg", "Malta", "Netherlands","Norway",  "Poland", 
                    "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")

EU30_code2 <- c("AT", "BE", "BG", "HR", "CY", "CZ",
                "DK", "EE", "FI", "FR", "DE", "GR",
                "HU","IS", "IE", "IT", "LV","LI",
                "LT", "LU", "MT", "NL","NO", "PL",
                "PT", "RO", "SK", "SI", "ES", "SE")

# ====== DATE, DATE_WEEK FORMAT ==========
# Start-end dates for setting up all datasets
date_start <- as.Date("2020-03-23") # start at week 13-2020
date_end <- as.Date("2022-12-18")   # end at week 50-2022

# Start-end weeks for model fitting and data extraction
dateweek_start <- as.Date("2020-03-23") # start at week 13-2020
dateweek_end <- as.Date("2022-12-12")   # end at week 50-2022

# 1001 days from date_start to date_end (143 weeks)
list_date <- data.frame(
  date = seq(from=date_start, to=date_end, by="day"),
  week_num = c(rep(13:53, each=7), rep(1:52, each=7), rep(1:50, each=7)),
  year_by_week = c(rep(2020,(53-13+1)*7), rep(2021, 52*7), rep(2022,50*7))) %>%
  mutate(month = month(date), 
         year = year(date), 
         day_of_week = format(date, "%a"),
         weekend = if_else(day_of_week %in% c("Sat", "Sun"), 1,0),
         day_t = 1:as.numeric(date_end-date_start+1)) %>%
  arrange(date)

# 143 weeks from week start to week end
list_dateweek <- list_date %>%
  select(date, week_num, year_by_week) %>%
  distinct(week_num, year_by_week, .keep_all = TRUE) %>%
  rename("date_week" = "date") %>%
  mutate(week_t = 1:(53-13+1+52+50))

# Finalize the list_date
list_date <- merge(list_date, list_dateweek, by=c("week_num","year_by_week"), all.x=TRUE) %>%
  arrange(date)

#Extract date and week in character form
list_date_char <- as.character(list_date$date)
list_week_char <- as.character(list_dateweek$date_week)

n_weeks <- 143
n_countries <- 30


