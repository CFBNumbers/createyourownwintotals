library(shiny)
library(gt)
library(tidyverse) 
library(cfbplotR)

df <- read.csv("games25.csv")




slider_names <- function(x) {
  df %>%
    filter(team == x) %>%
    select(opp)
}

ui <- fluidPage(
  titlePanel("12-Slider Sum Shiny App"),
  sidebarLayout(
    sidebarPanel(
      selectInput("sidebar_option", "Select A Team:",
                  choices = unique(df$team),
                  selected = "Florida State")
    ),
    mainPanel(
      # Display all sliders on main panel
      lapply(1:12, function(i) {
        sliderInput(
          inputId = paste0("slider", i),
          label = slider_names(sidebar_option),
          min = 0,
          max = 100,
          value = 50
        )
      }),
      br(),
      gt_output("sum_table")
    )
  )
)

server <- function(input, output, session) {
  slider_values <- reactive({
    sapply(1:12, function(i) input[[paste0("slider", i)]])
  })
  
  output$sum_table <- render_gt({
    values <- slider_values()
    total <- sum(values)
    data <- data.frame(
      Name = slider_names,
      Value = values
    )
    data <- rbind(data, data.frame(Name = "Total", Value = total))
    gt(data)
  })
}

shinyApp(ui, server)