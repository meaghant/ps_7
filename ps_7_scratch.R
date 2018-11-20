library(shiny)
library(tidyverse)
library(fs)
library(readr)

# Read in forecast data

download.file(url = "https://goo.gl/ZRCBda",
              destfile = "mt_2_2018-live-poll-results-master.zip",
              mode = "wb")

unzip("mt_2_2018-live-poll-results-master.zip")

file_names <- dir_ls("2018-live-poll-results-master/data/")

forecast <- map_dfr(file_names, read_csv, .id = "source")

# Read in results data

results <- read_csv("mt_2_results.csv")
  
# Get rid of Senate/Governor races from forecast and results data. Eliminate N/A responses from desired variables.

forecast_race <- forecast %>%
  select(source, race_eth) %>%
  mutate(senate = str_detect(source, "sen")) %>%
  filter(senate == "FALSE",
         source != "2018-live-poll-results-master/data/elections-poll-flgov-3.csv") %>%
  mutate(state_district_wave = str_remove(source, "2018-live-poll-results-master/data/elections-poll-"),
         state_district_wave = str_remove(state_district_wave, ".csv"),
         state_district = toupper(substr(state_district_wave, 1, 4)),
         wave = substr(state_district_wave, 6, 6)) %>%
  filter(wave == 3) %>%
  group_by(state_district) %>%
  count(race_eth) %>%
  spread(race_eth, n) %>%
  rename(n_a = "[DO NOT READ] Don't know/Refused") %>%
  mutate(total = n_a + Asian + Black + White + Other,
         percent_asian = Asian/total,
         percent_black = Black/total,
         percent_white = White/total, 
         percent_other = Other/total) %>%
  select(state_district, percent_asian, percent_black, percent_white, percent_other)

forecast_adv <- forecast %>%
  select(source, response, final_weight) %>%
  mutate(senate = str_detect(source, "sen")) %>%
  filter(senate == "FALSE",
         source != "2018-live-poll-results-master/data/elections-poll-flgov-3.csv") %>%
  group_by(source, response) %>%
  tally(final_weight, sort = FALSE) %>%
  spread(response, n) %>%
  mutate(state_district_wave = str_remove(source, "2018-live-poll-results-master/data/elections-poll-"),
         state_district_wave = str_remove(state_district_wave, ".csv"),
         state_district = toupper(substr(state_district_wave, 1, 4)),
         wave = substr(state_district_wave, 6, 6)) %>%
  filter(wave == 3) %>%
  mutate(total = Rep + Dem + Und,
         forecast_dem_adv = (Dem - Rep) / total) %>%
  ungroup() %>%
  select(state_district, forecast_dem_adv) 

forecast_joined <- left_join(forecast_adv, forecast_race, by = "state_district")

results <- read_csv("mt_2_results.csv") %>%
  filter(district != "sen",
         district != "gov") %>%
  mutate(state_district = paste(state, district, sep = "")) %>%
  group_by(state_district) %>%
  mutate(total = rep_votes + dem_votes + other_votes,
         dem_adv = (dem_votes - rep_votes) / total,
         actual_dem_outcome = case_when(dem_adv > 0 ~ "win",
                                 dem_adv < 0 ~ "lose"))

forecast_results_joined <- left_join(forecast_joined, results, by = "state_district") %>%
  select(state_district, forecast_dem_adv, actual_dem_outcome, percent_asian, percent_black, percent_white, percent_other)

forecast_results_joined_rds <- forecast_results_joined %>%
  write_rds(path = "forecast_results_joined.rds")