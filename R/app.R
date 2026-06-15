# app.R
library(shiny)
library(tidyverse)
library(lubridate)
library(readxl)

source("monte_carlo_forecast.R")
model_bundle <- readRDS("model_bundle.rds")

ui <- fluidPage(
  titlePanel("ISONE DA LMP Futures Settlement Forecast"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput(
        "report_file",
        "Upload ICE Report (.xlsx)",
        accept = ".xlsx"
      ),
      dateInput(
        "report_date",
        "Report Date",
        value = Sys.Date(),
        format = "mm/dd/yyyy"
      ),
      actionButton(
        "run",
        "Run Forecast",
        class = "btn-primary",
        width = "100%"
      ),
      br(), br(),
      uiOutput("contract_selector")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Forecast Results", br(), tableOutput("results_table")),
        tabPanel("Distributions",    br(), plotOutput("dist_plot", height = "500px"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  results <- eventReactive(input$run, {
    req(input$report_file)
    
    contracts <- read_xlsx(input$report_file$datapath, skip = 1) |>
      select(`CONTRACT MONTH`, PRICE) |>
      rename(
        contract_month = `CONTRACT MONTH`,
        futures_price  = PRICE
      ) |>
      mutate(
        delivery_month     = substr(contract_month, 1, 3),
        delivery_year      = as.integer(paste0("20", substr(contract_month, 4, 5))),
        report_month       = month(as.Date(input$report_date)),
        report_year        = year(as.Date(input$report_date)),
        delivery_month_num = match(delivery_month, month.abb),
        months_out         = as.integer(
          (delivery_year - report_year) * 12 +
            (delivery_month_num - report_month)
        ),
        contract_label     = contract_month
      ) |>
      filter(months_out > 0) |>
      select(contract_label, delivery_month, delivery_year, months_out, futures_price)
    
    contracts |>
      rowwise() |>
      mutate(
        sim = list(forecast_prices(
          model_bundle   = model_bundle,
          delivery_month = delivery_month,
          months_out     = months_out,
          futures_price  = futures_price
        ))
      ) |>
      ungroup() |>
      mutate(
        summary = map(sim, summarize_forecast),
        label   = paste0(delivery_month, " ", delivery_year)
      )
  })
  
  # Dynamically render contract selector — none selected by default
  output$contract_selector <- renderUI({
    req(results())
    checkboxGroupInput(
      "selected_contracts",
      "Select Contracts to Plot",
      choices  = setNames(results()$label, results()$contract_label),
      selected = character(0)  # none selected by default
    )
  })
  
  output$results_table <- renderTable({
    req(results())
    results() |>
      mutate(summary = map2(summary, contract_label, ~ mutate(.x, contract = .y))) |>
      select(summary) |>
      unnest(summary) |>
      mutate(
        months_out            = as.integer(months_out),
        futures_price         = paste0("$", formatC(futures_price,         format = "f", digits = 2)),
        settlement_volatility = paste0("$", formatC(settlement_volatility, format = "f", digits = 2)),
        p05_settlement        = paste0("$", formatC(p05_settlement,        format = "f", digits = 2)),
        p50_settlement        = paste0("$", formatC(p50_settlement,        format = "f", digits = 2)),
        p95_settlement        = paste0("$", formatC(p95_settlement,        format = "f", digits = 2))
      ) |>
      select(contract, months_out, futures_price,
             settlement_volatility, p05_settlement,
             p50_settlement, p95_settlement) |>
      rename(
        "Contract"           = contract,
        "Months Out"         = months_out,
        "Futures Price"      = futures_price,
        "Std. Deviation"     = settlement_volatility,
        "Best Case (p05)"    = p05_settlement,
        "Base Case (p50)"    = p50_settlement,
        "Worst Case (p95)"   = p95_settlement
      )
  })
  
  output$dist_plot <- renderPlot({
    req(results(), input$selected_contracts)
    validate(need(length(input$selected_contracts) > 0, "Select at least one contract to plot."))
    
    plot_data <- results() |>
      filter(label %in% input$selected_contracts) |>
      mutate(draws = map(sim, ~ tibble(price = .x$forecast))) |>
      select(contract_label, draws) |>
      unnest(draws)
    
    # Find local maxima for each contract
    peaks <- plot_data |>
      group_by(contract_label) |>
      summarise(
        dens = list(density(price)),
        .groups = "drop"
      ) |>
      mutate(
        peak_data = map(dens, ~ {
          x <- .x$x
          y <- .x$y
          # Find local maxima — points higher than both neighbors
          is_peak <- c(FALSE, y[-1] > y[-length(y)], FALSE) &
            c(y[-length(y)] > y[-1], FALSE, FALSE)
          # Flip to correct logic
          is_peak <- (y > dplyr::lag(y, default = 0)) &
            (y > dplyr::lead(y, default = 0))
          peak_x <- x[is_peak]
          peak_y <- y[is_peak]
          # Keep only top 2 peaks by height
          top2 <- order(peak_y, decreasing = TRUE)[1:min(2, sum(is_peak))]
          tibble(x = peak_x[top2], y = peak_y[top2])
        })
      ) |>
      select(contract_label, peak_data) |>
      unnest(peak_data)
    
    plot_data |>
      ggplot(aes(x = price, fill = contract_label, color = contract_label)) +
      geom_density(alpha = 0.3, linewidth = 0.7) +
      geom_text(
        data = peaks,
        aes(x = x, y = y, label = paste0("$", round(x, 1)), color = contract_label),
        vjust = -0.5,
        size  = 3.5,
        show.legend = FALSE
      ) +
      labs(
        title    = "Simulated Settlement Price Distributions",
        subtitle = "Each curve represents the predicted distribution for one contract",
        x        = "Simulated Settlement Price ($/MWh)",
        y        = "Density",
        fill     = NULL,
        color    = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title       = element_text(size = 14, face = "bold"),
        plot.subtitle    = element_text(size = 11, color = "gray40"),
        panel.grid.minor = element_blank(),
        legend.position  = "bottom"
      )
  })
}

shinyApp(ui, server)