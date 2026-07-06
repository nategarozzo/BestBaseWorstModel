# app.R
library(shiny)
library(tidyverse)
library(lubridate)
library(pdftools)

source("monte_carlo_forecast.R")
source("process_pdfs.R")
model_bundle <- readRDS("model_bundle.rds")

CONTRACT_COLORS <- c(
  "#2C4F7C", "#1A7A5E", "#7C3D2C",
  "#4A2C7C", "#1A5F7A", "#7A5E1A"
)
MAX_CONTRACTS <- length(CONTRACT_COLORS)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background-color: #f8f9fa; font-family: 'Georgia', serif; }
      .well { background-color: #ffffff; border: 1px solid #e0e0e0; border-radius: 6px; box-shadow: 0 1px 3px rgba(0,0,0,0.06); }
      .nav-tabs > li > a { font-family: 'Georgia', serif; color: #444; font-size: 14px; }
      .nav-tabs > li.active > a { color: #2C4F7C; font-weight: bold; border-top: 2px solid #2C4F7C; }
      .btn-primary { background-color: #2C4F7C; border-color: #2C4F7C; font-family: 'Georgia', serif; width: 100%; }
      .btn-primary:hover { background-color: #1e3a5f; border-color: #1e3a5f; }
      .btn-default { font-family: 'Georgia', serif; font-size: 13px; }
      h2.title { color: #2C4F7C; font-size: 20px; font-weight: bold; border-bottom: 2px solid #2C4F7C; padding-bottom: 8px; margin-bottom: 16px; }
      .table { font-family: 'Georgia', serif; font-size: 13px; }
      .table th { background-color: #2C4F7C; color: white; font-weight: normal; }
      .table-striped > tbody > tr:nth-of-type(odd) { background-color: #f4f7fb; }
      label { font-size: 13px; color: #444; font-family: 'Georgia', serif; }
      .shiny-input-container { margin-bottom: 12px; }
      .checkbox label { font-size: 12px; }
      #max_warning { color: #7C3D2C; font-size: 12px; margin-top: 4px; font-style: italic; }
      .sidebar-panel-title { font-size: 13px; font-weight: bold; color: #2C4F7C; margin-bottom: 8px; text-transform: uppercase; letter-spacing: 0.5px; }
    "))
  ),
  
  titlePanel(
    div(
      h2("ISONE DA LMP Futures Settlement Forecast", class = "title"),
      p("Bridge Energy Services · Model v1.0",
        style = "font-size:12px; color:#888; margin-top:-8px; font-family:'Georgia',serif;")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(class = "sidebar-panel-title", "Data Upload"),
      fileInput(
        "peak_file",
        "Peak ICE Report (.pdf)",
        accept = ".pdf",
        placeholder = "No file selected"
      ),
      fileInput(
        "offpeak_file",
        "Off-Peak ICE Report (.pdf)",
        accept = ".pdf",
        placeholder = "No file selected"
      ),
      dateInput(
        "report_date",
        "Report Date",
        value  = Sys.Date(),
        format = "mm/dd/yyyy"
      ),
      actionButton(
        "run",
        "Run Forecast",
        class = "btn-primary",
        icon  = icon("play")
      ),
      hr(style = "border-color: #e0e0e0; margin: 16px 0;"),
      div(class = "sidebar-panel-title", "Plot Contracts"),
      uiOutput("contract_selector"),
      uiOutput("max_warning")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        type = "tabs",
        tabPanel("Forecast Results", br(), tableOutput("results_table")),
        tabPanel("Distributions",    br(), plotOutput("dist_plot", height = "480px"))
      ),
      br(),
      fluidRow(
        column(4, downloadButton("download_table", "Download Results (.xlsx)",
                                 class = "btn-default", style = "width:100%")),
        column(4, downloadButton("download_plot",  "Download Chart (.png)",
                                 class = "btn-default", style = "width:100%"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  results <- eventReactive(input$run, {
    req(input$peak_file, input$offpeak_file)
    
    # Parse both PDFs
    peak_data    <- parse_ice_rec_pdf(input$peak_file$datapath)
    offpeak_data <- parse_ice_rec_pdf(input$offpeak_file$datapath)
    
    # Average peak and off-peak settle prices
    contracts <- peak_data |>
      inner_join(
        offpeak_data |> select(contract_month, settle_price),
        by = "contract_month",
        suffix = c("_peak", "_offpeak")
      ) |>
      mutate(
        futures_price      = (settle_price_peak + settle_price_offpeak) / 2,
        delivery_month     = substr(contract_month, 1, 3),
        delivery_year      = as.integer(paste0("20", substr(contract_month, 4, 5))),
        report_month       = month(as.Date(input$report_date)),
        report_year        = year(as.Date(input$report_date)),
        delivery_month_num = match(delivery_month, month.abb),
        months_out         = as.integer(
          (delivery_year - report_year) * 12 +
            (delivery_month_num - report_month)
        ),
        contract_label = contract_month
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
      NULL,
      choices  = setNames(results()$label, results()$contract_label),
      selected = character(0)
    )
  })
  
  output$max_warning <- renderUI({
    req(input$selected_contracts)
    if (length(input$selected_contracts) >= MAX_CONTRACTS) {
      div(id = "max_warning",
          paste0("Maximum of ", MAX_CONTRACTS, " contracts selected"))
    }
  })
  
  observe({
    req(input$selected_contracts)
    if (length(input$selected_contracts) > MAX_CONTRACTS) {
      updateCheckboxGroupInput(
        session,
        "selected_contracts",
        selected = head(input$selected_contracts, MAX_CONTRACTS)
      )
    }
  })
  
  output$results_table <- renderTable(
    striped = TRUE,
    hover   = TRUE,
    {
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
          p90_settlement        = paste0("$", formatC(p90_settlement,        format = "f", digits = 2))
        ) |>
        select(contract, months_out, futures_price,
               settlement_volatility, p05_settlement,
               p50_settlement, p90_settlement) |>
        rename(
          "Contract"         = contract,
          "Months Out"       = months_out,
          "Futures Price"    = futures_price,
          "Std. Deviation"   = settlement_volatility,
          "Best Case (p05)"  = p05_settlement,
          "Base Case (p50)"  = p50_settlement,
          "Worst Case (p90)" = p90_settlement
        )
    }
  )
  
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
        p90 = map_dbl(sim, ~ quantile(.x$forecast, 0.90)),
        dens_at_p05 = map2_dbl(sim, p05, ~ approx(density(.x$forecast)$x,
                                                  density(.x$forecast)$y, .y)$y),
        dens_at_p50 = map2_dbl(sim, p50, ~ approx(density(.x$forecast)$x,
                                                  density(.x$forecast)$y, .y)$y),
        dens_at_p90 = map2_dbl(sim, p90, ~ approx(density(.x$forecast)$x,
                                                  density(.x$forecast)$y, .y)$y)
      ) |>
      select(contract_label, p05, p50, p90,
             dens_at_p05, dens_at_p50, dens_at_p90) |>
      pivot_longer(
        cols      = c(p05, p50, p90),
        names_to  = "percentile",
        values_to = "price"
      ) |>
      mutate(
        density_val = case_when(
          percentile == "p05" ~ dens_at_p05,
          percentile == "p50" ~ dens_at_p50,
          percentile == "p90" ~ dens_at_p90
        ),
        label_text = paste0("$", round(price, 0))
      )
    
    contract_order  <- results() |> filter(label %in% selected) |> pull(contract_label)
    contract_colors <- CONTRACT_COLORS[seq_along(contract_order)]
    names(contract_colors) <- contract_order
    
    x_clip_min <- quantile(plot_data$price, 0.005)
    x_clip_max <- quantile(plot_data$price, 0.995)
    
    plot_data |>
      ggplot(aes(x = price, fill = contract_label, color = contract_label)) +
      geom_density(alpha = 0.15, linewidth = 0.9) +
      geom_vline(
        data      = quantile_labels,
        aes(xintercept = price, color = contract_label),
        linetype  = "dashed", linewidth = 0.5, alpha = 0.85
      ) +
      geom_label(
        data          = quantile_labels,
        aes(x = price, y = density_val, label = label_text, color = contract_label),
        vjust         = -0.3, size = 3.2, fontface = "bold",
        show.legend   = FALSE, fill = "white",
        label.size    = 0, label.padding = unit(0.2, "lines"), alpha = 0.9
      ) +
      scale_fill_manual(values  = contract_colors) +
      scale_color_manual(values = contract_colors) +
      scale_x_continuous(
        breaks = scales::pretty_breaks(n = 8),
        labels = scales::dollar_format(accuracy = 1)
      ) +
      scale_y_continuous(labels = NULL) +
      coord_cartesian(xlim = c(x_clip_min, x_clip_max)) +
      labs(
        title    = "Simulated Settlement Price Distributions",
        subtitle = "Dashed lines indicate best (p05), base (p50), and worst case (p90) estimates",
        x        = "Simulated Settlement Price ($/MWh)",
        y        = NULL, fill = NULL, color = NULL
      ) +
      theme_minimal(base_size = 14, base_family = "Georgia") +
      theme(
        plot.title         = element_text(size = 16, face = "bold", margin = margin(b = 4)),
        plot.subtitle      = element_text(size = 12, color = "gray50", margin = margin(b = 12)),
        axis.title.x       = element_text(size = 12, margin = margin(t = 8)),
        axis.text.x        = element_text(size = 11),
        axis.text.y        = element_blank(),
        axis.ticks.y       = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_line(color = "gray92", linewidth = 0.4),
        legend.position    = "bottom",
        legend.text        = element_text(size = 11),
        plot.background    = element_rect(fill = "white", color = NA),
        panel.background   = element_rect(fill = "white", color = NA),
        plot.margin        = margin(16, 24, 16, 16)
      )
  }
  
  output$dist_plot <- renderPlot({
    req(results(), input$selected_contracts)
    validate(need(length(input$selected_contracts) > 0, "Select at least one contract to plot."))
    build_plot(input$selected_contracts)
  })
  
  output$download_table <- downloadHandler(
    filename = function() paste0("isone_forecast_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      results() |>
        mutate(summary = map2(summary, contract_label, ~ mutate(.x, contract = .y))) |>
        select(summary) |>
        unnest(summary) |>
        mutate(across(where(is.numeric), ~ round(.x, 2)),
               months_out = as.integer(months_out)) |>
        select(contract, months_out, futures_price,
               settlement_volatility, p05_settlement,
               p50_settlement, p90_settlement) |>
        rename(
          "Contract"         = contract,
          "Months Out"       = months_out,
          "Futures Price"    = futures_price,
          "Std. Deviation"   = settlement_volatility,
          "Best Case (p05)"  = p05_settlement,
          "Base Case (p50)"  = p50_settlement,
          "Worst Case (p90)" = p90_settlement
        ) |>
        writexl::write_xlsx(file)
    }
  )
  
  output$download_plot <- downloadHandler(
    filename = function() paste0("isone_distribution_", Sys.Date(), ".png"),
    content  = function(file) {
      req(input$selected_contracts)
      ggsave(file, plot = build_plot(input$selected_contracts),
             width = 10, height = 6, dpi = 300, bg = "white")
    }
  )
}

shinyApp(ui, server)