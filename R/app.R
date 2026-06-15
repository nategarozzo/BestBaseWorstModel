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
      ),
      br(),
      fluidRow(
        column(6, downloadButton("download_table", "Download Results (.xlsx)",
                                 style = "width: 100%")),
        column(6, downloadButton("download_plot",  "Download Distribution (.png)",
                                 style = "width: 100%"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  results <- eventReactive(input$run, {
    req(input$report_file)
    
    contracts <- read_xlsx(input$report_file$datapath, skip = 1) |>
      select(NAME, `CONTRACT MONTH`, PRICE) |>
      rename(
        contract_month = `CONTRACT MONTH`,
        futures_price  = PRICE
      ) |>
      mutate(
        futures_price      = as.numeric(futures_price),
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
      filter(months_out > 0, months_out <= 30, !is.na(futures_price)) |>
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
  
  output$contract_selector <- renderUI({
    req(results())
    checkboxGroupInput(
      "selected_contracts",
      "Select Contracts to Plot",
      choices  = setNames(results()$label, results()$contract_label),
      selected = character(0)
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
        "Contract"          = contract,
        "Months Out"        = months_out,
        "Futures Price"     = futures_price,
        "Std. Deviation"    = settlement_volatility,
        "Best Case (p05)"   = p05_settlement,
        "Base Case (p50)"   = p50_settlement,
        "Worst Case (p95)"  = p95_settlement
      )
  })
  
  # Shared plot builder
  build_plot <- function(selected) {
    plot_data <- results() |>
      filter(label %in% selected) |>
      mutate(draws = map(sim, ~ tibble(price = .x$forecast))) |>
      select(contract_label, sim, draws) |>
      unnest(draws)
    
    quantile_labels <- results() |>
      filter(label %in% selected) |>
      mutate(
        p05 = map_dbl(sim, ~ quantile(.x$forecast, 0.05)),
        p50 = map_dbl(sim, ~ quantile(.x$forecast, 0.50)),
        p95 = map_dbl(sim, ~ quantile(.x$forecast, 0.95)),
        dens_at_p05 = map2_dbl(sim, p05, ~ approx(density(.x$forecast)$x,
                                                  density(.x$forecast)$y, .y)$y),
        dens_at_p50 = map2_dbl(sim, p50, ~ approx(density(.x$forecast)$x,
                                                  density(.x$forecast)$y, .y)$y),
        dens_at_p95 = map2_dbl(sim, p95, ~ approx(density(.x$forecast)$x,
                                                  density(.x$forecast)$y, .y)$y)
      ) |>
      select(contract_label, p05, p50, p95,
             dens_at_p05, dens_at_p50, dens_at_p95) |>
      pivot_longer(
        cols      = c(p05, p50, p95),
        names_to  = "percentile",
        values_to = "price"
      ) |>
      mutate(
        density_val = case_when(
          percentile == "p05" ~ dens_at_p05,
          percentile == "p50" ~ dens_at_p50,
          percentile == "p95" ~ dens_at_p95
        ),
        label_text = paste0("$", round(price, 1))
      )
    
    # Dynamic x axis breaks every $10
    x_min <- floor(min(plot_data$price) / 10) * 10
    x_max <- ceiling(max(plot_data$price) / 10) * 10
    
    plot_data |>
      ggplot(aes(x = price, fill = contract_label, color = contract_label)) +
      geom_density(alpha = 0.3, linewidth = 0.7) +
      geom_vline(
        data     = quantile_labels,
        aes(xintercept = price, color = contract_label),
        linetype = "dashed", linewidth = 0.4, alpha = 0.7
      ) +
      geom_label(
        data        = quantile_labels,
        aes(x = price, y = density_val, label = label_text, color = contract_label),
        vjust       = -0.3, size = 2.8, show.legend = FALSE,
        fill        = "white", label.size = 0, alpha = 0.8
      ) +
      scale_x_continuous(breaks = seq(x_min, x_max, by = 10)) +
      labs(
        title    = "Simulated Settlement Price Distributions",
        subtitle = "Dashed lines indicate best (p05), base (p50), and worst (p95) case estimates",
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
  }
  
  output$dist_plot <- renderPlot({
    req(results(), input$selected_contracts)
    validate(need(length(input$selected_contracts) > 0, "Select at least one contract to plot."))
    build_plot(input$selected_contracts)
  })
  
  output$download_table <- downloadHandler(
    filename = function() paste0("isone_forecast_", Sys.Date(), ".xlsx"),
    content = function(file) {
      results() |>
        mutate(summary = map2(summary, contract_label, ~ mutate(.x, contract = .y))) |>
        select(summary) |>
        unnest(summary) |>
        mutate(across(where(is.numeric), ~ round(.x, 2)),
               months_out = as.integer(months_out)) |>
        select(contract, months_out, futures_price,
               settlement_volatility, p05_settlement,
               p50_settlement, p95_settlement) |>
        rename(
          "Contract"        = contract,
          "Months Out"      = months_out,
          "Futures Price"   = futures_price,
          "Std. Deviation"  = settlement_volatility,
          "Best Case (p05)" = p05_settlement,
          "Base Case (p50)" = p50_settlement,
          "Worst Case (p95)" = p95_settlement
        ) |>
        writexl::write_xlsx(file)
    }
  )
  
  output$download_plot <- downloadHandler(
    filename = function() paste0("isone_distribution_", Sys.Date(), ".png"),
    content = function(file) {
      req(input$selected_contracts)
      ggsave(file, plot = build_plot(input$selected_contracts),
             width = 10, height = 6, dpi = 300)
    }
  )
}

shinyApp(ui, server)