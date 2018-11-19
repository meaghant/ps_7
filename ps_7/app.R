library(shiny)
library(tidyverse)
library(fs)
library(ggplot2)
library(ggrepel)
library(plotly)
library(ggpubr)
library(readr)

shiny_forecast_results_joined <- read_rds("forecast_results_joined.rds")
 
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("How Do Predicted and Actual Democratic Outcomes Vary by Racial Demographics of Polled Respondents?"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("race_input", 
                    "Select a Race/Ethnicity to View",
                    choices = c("Asian", "Black", "White", "Other")),
        checkboxInput("line", label = "Show Line of Best Fit", value = FALSE)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("scatterplot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$scatterplot <- renderPlot({
     
     if (input$race_input == "Asian") {
       asian_plot <- shiny_forecast_results_joined %>%
         ggplot(aes(x = percent_asian, y = forecast_dem_adv, color = actual_dem_outcome)) +
         geom_point() +
         labs(title = "How does Actual Democratic Advantage Vary by Racial Demographics of Polled Respondents?",
            x = "Percentage of Asian Poll Respondents",
            y = "Predicted Democratic Advantage",
            color = "Actual Democratic Outcome")
       
         print(asian_plot)
         
         if (input$line == TRUE) {
           withline<- asian_plot + 
             geom_smooth(method = lm, se = FALSE)
           print(withline)
         }
         
     }
         
     else if (input$race_input == "Black") {
       black_plot <- shiny_forecast_results_joined %>%
         ggplot(aes(x = percent_black, y = forecast_dem_adv, color = actual_dem_outcome)) +
         geom_point() +
         labs(title = "How does Actual Democratic Advantage Vary by Racial Demographics of Polled Respondents?",
              x = "Percentage of Black Poll Respondents",
              y = "Predicted Democratic Advantage",
              color = "Actual Democratic Outcome")
       
       print(black_plot)
       
       if (input$line == TRUE) {
         withline<- black_plot + 
           geom_smooth(method = lm, se = FALSE)
         print(withline)
       }
       
     }
     
     else if (input$race_input == "White") {
       white_plot <- shiny_forecast_results_joined %>%
         ggplot(aes(x = percent_white, y = forecast_dem_adv, color = actual_dem_outcome)) +
         geom_point() +
         labs(title = "How does Actual Democratic Advantage Vary by Racial Demographics of Polled Respondents?",
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
       other_plot <- shiny_forecast_results_joined %>%
         ggplot(aes(x = percent_other, y = forecast_dem_adv, color = actual_dem_outcome)) +
         geom_point() +
         labs(title = "How does Actual Democratic Advantage Vary by Racial Demographics of Polled Respondents?",
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

