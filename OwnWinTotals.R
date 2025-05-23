library(shiny)
library(gt)
library(tidyverse) 
library(cfbplotR)

df <- read.csv("games25.csv")

df <- df %>%
  mutate(opp = ifelse(location == "Home", paste0("Week ", Week, " vs. ", opp), paste0("Week ", Week, " @ ", opp)))

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      # 1. Team selection dropdown
      selectInput(
        inputId = "team_select",
        label = "Choose a team:",
        choices = unique(df$team)
      ),
      # 2. Placeholder for dynamic sliders
      uiOutput("dynamic_sliders")
    ),
    mainPanel(
      # Your main panel content here
    )
  )
)

server <- function(input, output, session) {
  # 3. Reactive filtered data based on selected team
  filtered_opp <- reactive({
    req(input$team_select)
    df[df$team == input$team_select, "opp", drop = FALSE]
  })
  
  # 4. Dynamically render sliders for each opponent
  output$dynamic_sliders <- renderUI({
    opps <- filtered_opp()[, "opp"]
    # Generates a list of sliders, one for each 'opp'
    lapply(opps, function(opp_name) {
      sliderInput(
        inputId = paste0("slider_", opp_name),
        label = opp_name,
        min = 0,
        max = 100,
        value = 50
      )
    })
  })
}

shinyApp(ui, server)

