library(shiny)
library(tidyverse)
library(fs)

# Read in forecast data

download.file(url = "https://goo.gl/ZRCBda",
              destfile = "mt_2_2018-live-poll-results-master.zip",
              mode = "wb")

unzip("mt_2_2018-live-poll-results-master.zip")

file_names <- dir_ls("2018-live-poll-results-master/data/")

forecast <- map_dfr(file_names, read_csv, .id = "source")

# Read in results data

results <- read_csv("mt_2_results.csv")
  
# Get rid of Senate/Governor races from forecast and results data

house_forecast <- forecast %>%
  select(source) %>%
  distinct(source) %>%
  mutate(senate = str_detect(source, "sen")) %>%
  filter(senate == "FALSE") %>%
  filter(source != "2018-live-poll-results-master/data/elections-poll-flgov-3.csv") %>%
  mutate(state_district_wave = str_remove(source, "2018-live-poll-results-master/data/elections-poll-"),
         state_district_wave = str_remove(state_district_wave, ".csv"),
         state = toupper(substr(state_district_wave, 1, 2)),
         district = substr(state_district_wave, 3, 4),
         wave = substr(state_district_wave, 6, 6)) %>%
  filter(wave == 3) %>%
  View()
  
two_waves <- x %>% 
  group_by(Office) %>% 
  summarize(waves = n_distinct(wave),
            first_wave = min(wave)) %>% 
  
  filter(waves > 1)   
  

results <- read_csv("mt_2_results.csv") %>%
  filter(district != "sen",
         district != "gov") %>%
  View()