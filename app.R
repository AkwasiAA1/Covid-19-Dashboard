# ===============================
# Loading Packages
# ===============================
library(shiny)
library(tidyverse)
library(lubridate)
library(ggplot2)

# ===============================
# Data Import (cleaned data)
# ===============================
covid <- read_csv("covid.csv")

# National summary (precomputed)
national_summary <- covid |>
  group_by(date) |>
  summarise(
    total_confirmed = sum(accum_cases, na.rm = TRUE),
    total_recovered = sum(recovered, na.rm = TRUE),
    total_active_cases = sum(active_cases, na.rm = TRUE),
    total_new_cases = sum(new_case, na.rm = TRUE),
    .groups = "drop"
  )

# ===============================
# UI
# ===============================
ui <- fluidPage(
  
  titlePanel("COVID-19 Dashboard – Ghana (June–August 2020)"),
  
  sidebarLayout(
    sidebarPanel(
      dateRangeInput(
        inputId = "date_range",
        label = "Select Date Range:",
        start = min(covid$date),
        end = max(covid$date)
      ),
      
      selectInput(
        inputId = "region",
        label = "Select Region:",
        choices = c("National", sort(unique(covid$region))),
        selected = "National"
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Cumulative Cases",
                 plotOutput("cumulative_plot", height = "400px")
        ),
        
        tabPanel("Daily New Cases",
                 plotOutput("new_cases_plot", height = "400px")
        ),
        
        tabPanel("Top 5 Regions",
                 plotOutput("top5_plot", height = "400px")
        ),
        
        tabPanel("Recovery Rate",
                 plotOutput("recovery_plot", height = "400px")
        )
      )
    )
  )
)

# ===============================
# Server
# ===============================
server <- function(input, output) {
  
  # Filter data by date
  filtered_data <- reactive({
    covid |>
      filter(date >= input$date_range[1],
             date <= input$date_range[2])
  })
  
  # ---- Plot 1: Cumulative Cases ----
  output$cumulative_plot <- renderPlot({
    
    if (input$region == "National") {
      national_summary |>
        filter(date >= input$date_range[1],
               date <= input$date_range[2]) |>
        ggplot(aes(date, total_confirmed)) +
        geom_line(color = "red", linewidth = 1) +
        labs(
          title = "National Cumulative COVID-19 Cases",
          x = "Date", y = "Cumulative Confirmed Cases"
        ) +
        theme_bw()
      
    } else {
      filtered_data() |>
        filter(region == input$region) |>
        ggplot(aes(date, accum_cases)) +
        geom_line(color = "blue", linewidth = 1) +
        labs(
          title = paste("Cumulative Cases –", input$region),
          x = "Date", y = "Cumulative Cases"
        ) +
        theme_bw()
    }
  })
  
  # ---- Plot 2: Daily New Cases ----
  output$new_cases_plot <- renderPlot({
    
    if (input$region == "National") {
      national_summary |>
        filter(date >= input$date_range[1],
               date <= input$date_range[2]) |>
        ggplot(aes(date, total_new_cases)) +
        geom_col(fill = "orange") +
        labs(
          title = "Daily New Cases (National)",
          x = "Date", y = "New Cases"
        ) +
        theme_bw()
      
    } else {
      filtered_data() |>
        filter(region == input$region) |>
        ggplot(aes(date, new_case)) +
        geom_col(fill = "steelblue") +
        labs(
          title = paste("Daily New Cases –", input$region),
          x = "Date", y = "New Cases"
        ) +
        theme_bw()
    }
  })
  
  # ---- Plot 3: Top 5 Regions ----
  output$top5_plot <- renderPlot({
    
    latest_update <- filtered_data() |>
      filter(date == max(date))
    
    top_5_regions <- latest_update |>
      arrange(desc(accum_cases)) |>
      slice_head(n = 5) |>
      pull(region)
    
    filtered_data() |>
      filter(region %in% top_5_regions) |>
      ggplot(aes(date, accum_cases, color = region)) +
      geom_line(linewidth = 1) +
      labs(
        title = "Cumulative Cases: Top 5 Regions",
        x = "Date", y = "Cumulative Cases"
      ) +
      scale_color_brewer(palette = "Set1") +
      theme_bw()
  })
  
  # ---- Plot 4: Recovery Rate ----
  output$recovery_plot <- renderPlot({
    
    latest_update <- filtered_data() |>
      filter(date == max(date)) |>
      mutate(recovery_rate = recovered / accum_cases * 100)
    
    latest_update |>
      arrange(desc(recovery_rate)) |>
      slice_head(n = 10) |>
      ggplot(aes(reorder(region, recovery_rate), recovery_rate)) +
      geom_col(fill = "forestgreen") +
      coord_flip() +
      labs(
        title = "Recovery Rate by Region (Latest Date)",
        y = "% Recovered", x = NULL
      ) +
      theme_minimal()
  })
}

# ===============================
# Run App
# ===============================
shinyApp(ui = ui, server = server)