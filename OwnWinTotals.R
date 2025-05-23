library(shiny)
library(gt)
library(tidyverse) 
library(cfbplotR)
library(thematic)
library(renv)
library(bslib)
library(gtExtras)
library(glue)
thematic::thematic_shiny(font = "auto")
options(warn = -1)

df <- read.csv("https://raw.githubusercontent.com/CFBNumbers/createyourownwintotals/refs/heads/main/games25.csv")

ui <- fluidPage(
  theme = bs_theme(
    bg = "#FFFFFF", fg = "black",
    bootswatch = "spacelab", 
    primary = "#FCC780", 
    base_font = font_google("Space Mono"),
    code_font = font_google("Space Mono")
  ),
  
  navbarPage("@CFBNumbers",
             tabPanel("Create Your Own Win Total!",
                      fluidRow(
                        column(2, align = "center",
                               tags$h5(" "),
                               tags$style(type="text/css",
                                          ".shiny-output-error { visibility: hidden; }",
                                          ".shiny-output-error:before { visibility: hidden; }"
                               )
                        )
                      ),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(
                            inputId = "team_select",
                            label = "Choose a team:",
                            choices = unique(df$team),
                            selected = "Florida State"
                          ),
                          textAreaInput(
                            inputId = "custom_footnote",
                            label = "Add Name Or Socials To Table:",
                            value = "",
                            rows = 1
                          ),
                          tags$hr(),
                          tags$a(
                            href = "https://buymeacoffee.com/cfbnumbers",
                            target = "_blank",
                            tags$img(
                              src = "https://miro.medium.com/v2/resize:fit:1090/0*lHgOW3tB_MfDAlBf.png",
                              style = "max-width: 100%; height: auto;",
                              alt = "Buy me a coffee"
                            )
                          )
                        ),
                        mainPanel(
                          tags$h4("Select Your % Odds To Win Each Game", style = "font-size: 20px;"),
                          uiOutput("dynamic_sliders"),
                          gt_output("results_table")
                        )
                      )
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
    vals_num <- unlist(vals)
    if (is.null(vals_num) || length(vals_num) != nrow(this_df) || length(vals_num) == 0) return(gt(data.frame()))
    table_data <- this_df %>%
      select(Week, team, opp, school2) %>%
      mutate(
        odds = vals_num / 100,
        school2 = ifelse(school2 == "East Texas A&M", "Texas A&M-Commerce", school2)
      )
    sum_val <- sum(vals/100, na.rm = TRUE)
    gt(table_data) %>%
      cols_align(align = "center") %>% 
      tab_header(title = html(glue::glue("<strong>{unique(this_df$team)} 2025 Season")),
                 subtitle = html(glue::glue("<em>My Projected Wins: {sum_val}"))) %>%
      gt_fmt_cfb_logo(columns = c(team, school2), height = 50) %>%
      cols_label(Week = md("**Week**"), 
                 team = "",
                 opp = md("**Game**"),
                 school2 = "",
                 odds = md("**My Odds To Win**")) %>%
      fmt_percent(
        columns = c(odds),
        decimals = 0
      ) %>%
      tab_options(heading.title.font.size = 30,
                  heading.subtitle.font.size = 20) %>%
      gt_theme_538() %>% 
      opt_align_table_header(align = "center") %>%
      {
        if(nzchar(input$custom_footnote)) {
          tab_source_note(., source_note = md(paste("**Odds By:**", input$custom_footnote)))
        } else .
      } %>%
      tab_source_note(source_note = md("**Table**: @CFBNumbers | **Data**: @CFB_Data with @cfbfastR"))
  })
}

shinyApp(ui, server)

