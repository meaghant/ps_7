library(shiny)
library(tidyverse)
library(fs)
library(ggplot2)

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
         percent_other = Other/total)

results <- read_csv("mt_2_results.csv") %>%
  filter(district != "sen",
         district != "gov") %>%
  mutate(state_district = paste(state, district, sep = "")) %>%
  group_by(state_district) %>%
  mutate(total = rep_votes + dem_votes + other_votes,
         dem_adv = (dem_votes - rep_votes) / total) 

race_joined <- left_join(forecast_race, results, by = "state_district") %>%
  select(state_district, percent_asian, percent_black, percent_white, percent_other, dem_adv)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Race Ethnicity Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "race_input",
          label = "Select a race/ethnicity",
          choices = c("Asian", "Black", "White", "Other")
      )),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("scatterplot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$scatterplot <- renderPlot({
     
     values <- reactiveValues()
     values$asian <- race_joined$percent_asian
     values$white <- race_joined$percent_white
     values$black <- race_joined$percent_black
    
     race_joined
       ggplot(race_joined, aes(x = input$race_input, y = dem_adv)) +
       geom_point() +
       labs(title = "How does Actual Democratic Advantage Vary by Racial Demographics of Polled Respondents?",
            x = "Race",
            y = "Democratic Advantage")
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

