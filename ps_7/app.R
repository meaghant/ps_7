library(shiny)
library(tidyverse)
library(fs)
library(ggplot2)
library(plotly)
library(ggrepel)
library(ggpubr)
library(readr)

# We had issues with reading in the rds, so we just included all of our data cleaning/editing in the app itself

# Read in forecast data
download.file(url = "https://goo.gl/ZRCBda",
              destfile = "mt_2_2018-live-poll-results-master.zip",
              mode = "wb")

unzip("mt_2_2018-live-poll-results-master.zip")

file_names <- dir_ls("2018-live-poll-results-master/data/")

forecast <- map_dfr(file_names, read_csv, .id = "source")

# Read in results data
results <- read_csv("mt_2_results.csv")

# Cleaning up forecast data and adding summary variables
forecast_race <- forecast %>%
  select(source, race_eth) %>%
  
  # Get rid of Senate/Governor races
  # Create new state district and wave variables
  # Filter to only include last wave of polling
  mutate(senate = str_detect(source, "sen")) %>%
  filter(senate == "FALSE",
         source != "2018-live-poll-results-master/data/elections-poll-flgov-3.csv") %>%
  mutate(state_district_wave = str_remove(source, "2018-live-poll-results-master/data/elections-poll-"),
         state_district_wave = str_remove(state_district_wave, ".csv"),
         state_district = toupper(substr(state_district_wave, 1, 4)),
         wave = substr(state_district_wave, 6, 6)) %>%
  filter(wave == 3) %>%
  
  # For each race/ethnicity, create percent variable
  # Remove N/As
  # Select desired variables
  
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

# Create Democratic advantage variable using original forecast dataframe, tallying and spreading response
# Create same state district and wave variables for use in joining to race data
# Filter to only include last wave of polling
# Ungroup so as to select by desired variables

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

# Join two tables together to get table of forecast results with our new variables, using state district as binding key
forecast_joined <- left_join(forecast_adv, forecast_race, by = "state_district")

# Read in actual election results data
results <- read_csv("mt_2_results.csv") %>%
  
  # Get rid of Senator/Governor races
  filter(district != "sen",
         district != "gov") %>%
  
  # Create variable for state and district that matches forecast data
  mutate(state_district = paste(state, district, sep = "")) %>%
  
  # Create outcome variable based on whether the Democrats won or lost 
  group_by(state_district) %>%
  mutate(total = rep_votes + dem_votes + other_votes,
         dem_adv = (dem_votes - rep_votes) / total,
         actual_dem_outcome = case_when(dem_adv > 0 ~ "win",
                                        dem_adv < 0 ~ "lose"))

# Join forecast data with results data, 
# select relevant variables, 
# and assign this to a new joined dataframe
forecast_results_joined <- left_join(forecast_joined, results, by = "state_district") %>%
  select(state_district, forecast_dem_adv, actual_dem_outcome, percent_asian, percent_black, percent_white, percent_other)

# Now, building the Shiny app itself. 
# Define UI for application that makes a scatterplot of election predictions and results
ui <- fluidPage(
   
   # Application title
   titlePanel("How Do Predicted and Actual Democratic Outcomes Vary by Racial Demographics of Polled Respondents?"),
   strong("Data From 2018 House Midterm Elections and Upshot Polls"),
   
   # Sidebar with an input that lets the user select which race data to view
   sidebarLayout(
      sidebarPanel(
        selectInput("race_input", 
                    "Select a Race/Ethnicity to View",
                    choices = c("Asian", "Black", "White", "Other")),
        
        # Add checkbox which lets user add/remove a best fit line 
        checkboxInput("line", label = "Show Best Fit Line", value = FALSE)
      ),
      
      # Show a scatterplot based on the user's choices
      mainPanel(
         plotOutput("scatterplot"),
         p("In general, districts with a greater percentage of Asian poll respondents were more likely 
           to yield a Democratic win. Notably, the two districts with the greatest proportion 
           of Asian poll respondents were both predicted Democratic wins than went Republican."),
           
         p("Districts with higher percentages of black poll respondents were correlated with a reduced likelihood of a Democratic win."),
         
         p("Districts with higher percentages of white people, conversely, were correlated with an increased likelihood of a Democratic win."),
           
         p("There was no significant correlation between percentage of poll respondents identifying 
           as a race other than Asian, black, or white and predicted democratic advantage. Most predictions
           were accurate."),
         
         p("These results do not necessarily reflect the way each demographic voted, but may be a proxy for other third factors, such as geographic region.")
         )
      )
   )


# Define server logic required to make a scatterplot
server <- function(input, output) {
   
   output$scatterplot <- renderPlot({
     
     # For each race input, use "if else" statements to determine which variable to use for the x-axis.
     # We used Claire Fridkin and Esteban Arellano's pset 7, posted on GitHub, to help us figure out
     # how to do this. Many thanks to them!
     # Make color correspond to whether the Democrats actually won or lost the election
     if (input$race_input == "Asian") {
       asian_plot <- forecast_results_joined %>%
         ggplot(aes(x = percent_asian, y = forecast_dem_adv, color = actual_dem_outcome)) +
         geom_point() +
         labs(title = "How Percentage of Asians in Districts Impacts Predicted Democratic Advantage",
            x = "Percentage of Asian Poll Respondents",
            y = "Predicted Democratic Advantage",
            color = "Actual Democratic Outcome")
       
         print(asian_plot)
         
         # If checkbox is checked, draw best fit line (linear model)
         if (input$line == TRUE) {
           withline<- asian_plot + 
             geom_smooth(method = lm, se = FALSE)
           print(withline)
         }
         
     }
         
     else if (input$race_input == "Black") {
       black_plot <- forecast_results_joined %>%
         ggplot(aes(x = percent_black, y = forecast_dem_adv, color = actual_dem_outcome)) +
         geom_point() +
         labs(title = "How Percentage of Black People in Districts Impacts Predicted Democratic Advantage",
              x = "Percentage of Black Poll Respondents",
              y = "Predicted Democratic Advantage",
              color = "Actual Democratic Outcome")
       
       print(black_plot)
       
       if (input$line == TRUE) {
         withline <- black_plot + 
           geom_smooth(method = lm, se = FALSE)
         print(withline)
       }
       
     }
     
     else if (input$race_input == "White") {
       white_plot <- forecast_results_joined %>%
         ggplot(aes(x = percent_white, y = forecast_dem_adv, color = actual_dem_outcome)) +
         geom_point() +
         labs(title = "How Percentage of White People in Districts Impacts Predicted Democratic Advantage",
              x = "Percentage of White Poll Respondents",
              y = "Predicted Democratic Advantage",
              color = "Actual Democratic Outcome")
       
       print(white_plot)
       
       if (input$line == TRUE) {
         withline<- white_plot + 
           geom_smooth(method = lm, se = FALSE)
         print(withline)
       }
       
     }
     
     else if (input$race_input == "Other") {
       other_plot <- forecast_results_joined %>%
         ggplot(aes(x = percent_other, y = forecast_dem_adv, color = actual_dem_outcome)) +
         geom_point() +
         labs(title = "How Percentage of Respondents Identifying as other than Asian, Black, or White Impacts Predicted Democratic Advantage",
              x = "Percentage of Poll Respondents Identifying as other than Asian, Black, or White",
              y = "Predicted Democratic Advantage",
              color = "Actual Democratic Outcome")
       
       print(other_plot)
       
       if (input$line == TRUE) {
         withline<- other_plot + 
           geom_smooth(method = lm, se = FALSE)
         print(withline)
       }
       
     }
     
   })
}


# Run the application 
shinyApp(ui = ui, server = server)

