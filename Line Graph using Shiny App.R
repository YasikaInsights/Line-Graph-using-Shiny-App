#Problem - #The user can select two periods (i.e., based on year and month) and the app will-produce a line graph that shows the evolution of the CPI between these two periods

library(shiny)
library(tidyverse)
library(readr)
library(ggplot2)

cpi.data <- read.csv("cpi.csv")

ui <- fluidPage(
  selectInput(inputId="start_month", label="Please select a start month", choices=unique(cpi.data$month)),
  selectInput(inputId= "start_year", label="Please select a start year", choices=unique(cpi.data$year)),
  selectInput(inputId="end_month", label="Please select an end month", choices=unique(cpi.data$month)),
  selectInput(inputId= "end_year", label="Please select an end year", choices=unique(cpi.data$year)),
  plotOutput(outputId= "cpi_plot")
)

server <- function(input, output, session) {
  selected_data <- reactive({
    filter(cpi.data, (year == input$start_year & month >= input$start_month) |
             (year > input$start_year & year < input$end_year) |
             (year == input$end_year & month <= input$end_month))
  })
  
  output$cpi_plot <- renderPlot({
    ggplot(data=selected_data(), aes(x=year, y=cpi)) +
      geom_line(color="blue", linewidth=0.8) +
      theme_classic()
  })
}

shinyApp(ui, server)

