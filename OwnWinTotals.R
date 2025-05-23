library(shiny)
library(gt)
library(tidyverse) 
library(cfbplotR)
options(warn = -1)


df <- read.csv("games25.csv")

df <- df %>%
  mutate(opp = ifelse(location == "Home", paste0("Week ", Week, " vs. ", opp), paste0("Week ", Week, " @ ", opp)))

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "team_select",
        label = "Choose a team:",
        choices = unique(df$team)
      )
    ),
    mainPanel(
      uiOutput("dynamic_sliders"),
      gt_output("results_table")
    )
  )
)

server <- function(input, output, session) {
  filtered_df <- reactive({
    req(input$team_select)
    df[df$team == input$team_select, ]
  })
  
  output$dynamic_sliders <- renderUI({
    this_df <- filtered_df()
    lapply(seq_len(nrow(this_df)), function(i) {
      sliderInput(
        inputId = paste0("slider_", i),
        label = this_df$opp[i],
        min = 0,
        max = 100,
        value = 50,
        step = 1
      )
    })
  })
  
  slider_vals <- reactive({
    this_df <- filtered_df()
    sapply(seq_len(nrow(this_df)), function(i) input[[paste0("slider_", i)]])
  })
  
  output$results_table <- render_gt({
    this_df <- filtered_df()
    vals <- slider_vals()
    if (is.null(vals) || length(vals) != nrow(this_df)) return(gt(data.frame()))
    table_data <- this_df %>%
      select(Week, team, opp, school2) %>%
      mutate(
        odds = vals/100,
        school2 = ifelse(school2 == "East Texas A&M", "Texas A&M-Commerce", school2)
      )
    sum_val <- sum(vals/100, na.rm = TRUE)
    gt(table_data) %>%
      cols_align(align = "center") %>% 
      tab_header(title = html(glue::glue("<strong>{unique(this_df$team)} 2025 Season")),
                 subtitle = html(glue::glue("<em>My Projected Wins: {sum_val}"))) %>%
      gt_fmt_cfb_logo(columns = c(team, school2), height = 50) %>%
      cols_label(Week = "Week", 
                 team = "",
                 opp = "Game",
                 school2 = "",
                 odds = "My Odds") %>%
      fmt_percent(
        columns = c(odds),
        decimals = 0
      ) %>%
      tab_options(heading.title.font.size = 35,
                  heading.subtitle.font.size = 20) %>%
      tab_source_note(source_note = md("**Table**: @CFBNumbers | **Data**: @CFB_Data with @cfbfastR")) %>%
      gt_theme_538() %>% 
      opt_align_table_header(align = "center")
  })
}

shinyApp(ui, server)




