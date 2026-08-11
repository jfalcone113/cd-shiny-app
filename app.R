
library(shiny)
library(bslib)
library(fpp3)
library(tidyverse)
library(lubridate)
library(scales)
library(plotly)

# -----------------------------
# 1. Data preparation
# -----------------------------

read_fred_cd <- function(series_id, term_months) {
  
  file_name <- paste0(series_id, ".csv")
  
  data <- read.csv(file_name) |>
    as_tibble()
  
  # Handles either "observation_date" or "DATE"
  if ("observation_date" %in% names(data)) {
    date_column <- data$observation_date
  } else if ("DATE" %in% names(data)) {
    date_column <- data$DATE
  } else if ("date" %in% names(data)) {
    date_column <- data$date
  } else {
    stop(
      "No date column found in ",
      file_name,
      ". Expected observation_date, DATE, or date."
    )
  }
  
  # Handles either the original FRED series column or a renamed rate column
  if (series_id %in% names(data)) {
    rate_column <- data[[series_id]]
  } else if ("rate" %in% names(data)) {
    rate_column <- data$rate
  } else {
    stop(
      "No rate column found in ",
      file_name,
      ". Expected ",
      series_id,
      " or rate."
    )
  }
  
  tibble(
    date = yearmonth(date_column),
    term = term_months,
    rate = as.numeric(rate_column)
  ) |>
    filter(
      date >= yearmonth("2021 Apr"),
      date <= yearmonth("2026 Jun"),
      !is.na(rate)
    )
}

cd <- bind_rows(
  read_fred_cd("NDR12MCD", 12),
  read_fred_cd("NDR24MCD", 24),
  read_fred_cd("NDR48MCD", 48)
) |>
  mutate(
    term = factor(
      term,
      levels = c(12, 24, 48),
      labels = c("12 Month", "24 Month", "48 Month")
    )
  ) |>
  arrange(term, date) |>
  as_tsibble(
    index = date,
    key = term
  )

min_date <- min(cd$date)
max_date <- max(cd$date)
max_future_date <- max_date + 48

term_choices <- c(
  "12 Month",
  "24 Month",
  "48 Month"
)

model_choices <- c(
  "Mean",
  "ARIMA",
  "TSLM",
  "Naive",
  "Ensemble"
)

# -----------------------------
# 2. Helper functions
# -----------------------------

months_between <- function(start_ym, end_ym) {
  start_date <- as.Date(start_ym)
  end_date <- as.Date(end_ym)
  
  12 * (year(end_date) - year(start_date)) +
    (month(end_date) - month(start_date))
}


fit_forecast_models <- function(data, selected_term, holdout_size, horizon) {
  one_series <- data |>
    filter(term == selected_term) |>
    as_tibble() |>
    dplyr::select(date, rate) |>
    arrange(date) |>
    as_tsibble(index = date)
  
  req(holdout_size < nrow(one_series))
  
  train_n <- nrow(one_series) - holdout_size
  
  train <- one_series |>
    slice_head(n = train_n)
  
  test <- one_series |>
    slice_tail(n = holdout_size)
  
  fits <- train |>
    model(
      Mean = MEAN(rate),
      Naive = NAIVE(rate),
      ARIMA = ARIMA(rate),
      TSLM = TSLM(rate ~ trend() + season())
    )
  
  test_fc_base <- fits |>
    forecast(h = holdout_size) |>
    as_tibble()
  
  future_fc_base <- fits |>
    forecast(h = horizon) |>
    as_tibble()
  
  test_fc_ensemble <- test_fc_base |>
    group_by(date) |>
    summarise(
      .model = "Ensemble",
      .mean = mean(.mean, na.rm = TRUE),
      .groups = "drop"
    )
  
  future_fc_ensemble <- future_fc_base |>
    group_by(date) |>
    summarise(
      .model = "Ensemble",
      .mean = mean(.mean, na.rm = TRUE),
      .groups = "drop"
    )
  
  test_fc <- bind_rows(
    test_fc_base |> dplyr::select(.model, date, .mean),
    test_fc_ensemble
  )
  
  future_fc <- bind_rows(
    future_fc_base |> dplyr::select(.model, date, .mean),
    future_fc_ensemble
  )
  
  acc <- test_fc |>
    left_join(test |> as_tibble(), by = "date") |>
    group_by(.model) |>
    summarise(
      ME = mean(rate - .mean, na.rm = TRUE),
      RMSE = sqrt(mean((rate - .mean)^2, na.rm = TRUE)),
      MAE = mean(abs(rate - .mean), na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(RMSE)
  
  list(
    train = train,
    test = test,
    fits = fits,
    test_fc = test_fc,
    future_fc = future_fc,
    accuracy = acc
  )
}

get_simulation_rates <- function(data, invest_ym, selected_model) {

  last_observed_month <- max(data$date)

  # Historical dates use the observed rates.
  if (invest_ym <= last_observed_month) {
    return(
      data |>
        filter(date == invest_ym) |>
        as_tibble() |>
        mutate(
          rate_source = "Observed historical rate"
        )
    )
  }

  h_needed <- as.integer(
    months_between(
      last_observed_month,
      invest_ym
    )
  )

  validate(
    need(
      h_needed >= 1,
      "The selected future month could not be converted into a forecast horizon."
    )
  )

  terms <- levels(data$term)

  future_rates <- purrr::map_dfr(
    terms,
    function(selected_term) {

      rate_values <- data |>
        filter(term == selected_term) |>
        as_tibble() |>
        arrange(date) |>
        pull(rate) |>
        as.numeric()

      rate_values <- rate_values[is.finite(rate_values)]

      validate(
        need(
          length(rate_values) >= 24,
          paste(
            "Not enough valid observations to forecast the",
            selected_term,
            "series."
          )
        )
      )

      # Use the fpp3/fable ARIMA implementation already used by the app.
      # This avoids introducing the separate forecast package as a dependency.
      one_series <- data |>
        filter(term == selected_term) |>
        as_tibble() |>
        dplyr::select(date, rate) |>
        arrange(date) |>
        as_tsibble(index = date)

      fitted_model <- one_series |>
        model(ARIMA = ARIMA(rate))

      forecast_result <- fitted_model |>
        forecast(h = h_needed) |>
        as_tibble()

      forecast_rate <- as.numeric(
        forecast_result$.mean[h_needed]
      )

      # If automatic ARIMA cannot produce a finite forecast, fall back to
      # the historical mean. This uses only the packages already loaded by
      # the app and avoids introducing another forecasting dependency.
      if (!is.finite(forecast_rate)) {
        forecast_rate <- mean(rate_values, na.rm = TRUE)
        source_label <- "Historical mean fallback"
      } else {
        source_label <- "fpp3 ARIMA forecast"
      }

      tibble(
        date = invest_ym,
        term = selected_term,
        rate = forecast_rate,
        rate_source = source_label
      )
    }
  ) |>
    mutate(
      term = factor(
        term,
        levels = terms
      )
    )

  future_rates
}


plot_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11, color = "#6b7280"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#e5e7eb"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "top",
    legend.title = element_blank()
  )

# -----------------------------
# 3. UI
# -----------------------------

ui <- navbarPage(
  title = "CD Rates Analysis Tool",
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#ff8200",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter")
  ),
  
  header = tags$head(
    tags$style(HTML("
      body {
        background:
          radial-gradient(circle at top left, rgba(255,130,0,0.12), transparent 28%),
          linear-gradient(180deg, #ff8200 0%, #eef2f7 100%);
        color: #4B4B4B;
        letter-spacing: -0.2px;
      }

      .navbar {
        background: rgba(255,255,255,0.88) !important;
        backdrop-filter: blur(14px);
        box-shadow: 0 2px 18px rgba(0,0,0,0.08);
        border-bottom: 1px solid rgba(255,255,255,0.65);
      }

      .navbar-brand {
        font-weight: 800;
        letter-spacing: -0.4px;
        color: #2f2f2f !important;
      }

      .navbar-nav .nav-link {
        margin: 0 5px;
        border-radius: 10px;
        color: #4B4B4B !important;
        font-weight: 600;
        transition: all 0.2s ease;
        position: relative;
      }

      .navbar-nav .nav-link:hover {
        color: #ff8200 !important;
        background-color: rgba(255,130,0,0.10) !important;
      }

      .navbar-nav .nav-link.active {
        color: #ff8200 !important;
        background: transparent !important;
      }

      .navbar-nav .nav-link.active::after {
        content: '';
        position: absolute;
        left: 14%;
        right: 14%;
        bottom: 4px;
        height: 3px;
        border-radius: 999px;
        background: #ff8200;
      }

      .container-fluid {
        padding-top: 24px;
      }

      .well {
        background: rgba(255,255,255,0.76);
        backdrop-filter: blur(14px);
        border: 1px solid rgba(255,255,255,0.72);
        border-radius: 18px;
        box-shadow: 0 8px 28px rgba(15,23,42,0.07);
      }

      .main-panel-card {
        background: rgba(255,255,255,0.78);
        backdrop-filter: blur(14px);
        border: 1px solid rgba(255,255,255,0.75);
        border-radius: 22px;
        padding: 24px;
        margin-bottom: 26px;
        box-shadow: 0 8px 30px rgba(15,23,42,0.07);
        transition: all 0.25s ease;
      }

      .main-panel-card:hover {
        transform: translateY(-3px);
        box-shadow: 0 12px 38px rgba(15,23,42,0.10);
      }

      .project-overview-card {
        border-left: 6px solid #ff8200;
        background: linear-gradient(135deg, rgba(255,255,255,0.92), rgba(255,247,237,0.92));
      }

      .project-overview-card h2 {
        margin-top: 0;
      }

      h2, h3, h4 {
        font-weight: 800;
        color: #2f2f2f;
        letter-spacing: -0.4px;
      }

      .main-panel-card h3 {
        border-bottom: 1px solid #e5e7eb;
        padding-bottom: 10px;
        margin-bottom: 18px;
      }

      p {
        line-height: 1.68;
        font-size: 15px;
        color: #4b5563;
      }

      .shiny-plot-output {
        background: #ffffff;
        border-radius: 16px;
        padding: 8px;
      }

      pre {
        white-space: pre-wrap;
        word-wrap: break-word;
        background: #f8fafc;
        border: 1px solid #e5e7eb;
        border-radius: 14px;
        padding: 16px;
        font-size: 14px;
        color: #374151;
      }

      table {
        background: white;
        border-radius: 14px;
        overflow: hidden;
      }

      .form-group {
        margin-bottom: 18px;
      }

      label {
        font-weight: 700;
        color: #374151;
      }

      .form-control, .selectize-input {
        border-radius: 12px !important;
        border: 1px solid #e5e7eb !important;
        box-shadow: none !important;
        transition: all 0.2s ease;
      }

      .form-control:focus, .selectize-input.focus {
        border-color: #ff8200 !important;
        box-shadow: 0 0 0 3px rgba(255,130,0,0.16) !important;
      }

      .irs--shiny .irs-bar,
      .irs--shiny .irs-single {
        background: #ff8200 !important;
        border-color: #ff8200 !important;
      }

      .irs--shiny .irs-handle {
        border-color: #ff8200 !important;
      }

      input[type='checkbox'],
      input[type='radio'] {
        accent-color: #ff8200;
      }

      .btn, .btn-default {
        border-radius: 12px;
      }

      .summary-card {
        background: linear-gradient(135deg, #ffffff 0%, #fff7ed 100%);
        border: 1px solid rgba(255,130,0,0.18);
        border-radius: 18px;
        padding: 18px;
        margin-bottom: 18px;
        box-shadow: 0 6px 20px rgba(15,23,42,0.06);
      }

      .summary-label {
        font-size: 13px;
        font-weight: 700;
        color: #6b7280;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }

      .summary-value {
        font-size: 26px;
        font-weight: 850;
        color: #ff8200;
        margin-top: 4px;
      }
      .sidebar-text-box {
        background: rgba(255, 255, 255, 0.82);
        border: 1px solid rgba(255, 130, 0, 0.22);
        border-left: 5px solid #ff8200;
        border-radius: 14px;
        padding: 16px;
        margin-top: 22px;
        box-shadow: 0 5px 18px rgba(15, 23, 42, 0.06);
      }
      
      .sidebar-text-box h4 {
        margin-top: 0;
        margin-bottom: 10px;
        font-size: 17px;
      }
      
      .sidebar-text-box p {
        margin-bottom: 0;
        line-height: 1.55;
      }
    "))
  ),
  
  tabPanel(
    "Overview",
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(
          "explore_terms",
          "Selected CD terms",
          choices = term_choices,
          selected = term_choices
        ),
        sliderInput(
          "date_range",
          "Date range:",
          min = as.Date(min_date),
          max = as.Date(max_date),
          value = c(as.Date(min_date), as.Date(max_date)),
          timeFormat = "%b %Y"
        )
      ),
      mainPanel(
        div(
          class = "main-panel-card project-overview-card",
          h2("Picking a CD: When to Invest and Which Term to Choose"),
          p(
            "This project uses Federal Reserve Economic Data (FRED) to explain how national average Certificate of Deposit (CD) rates have changed over time. The five tabs move from historical rate patterns, to the spread between short- and long-term CDs, to month-to-month changes, forecasting, and a practical investment simulator."
          )
        ),
        
        fluidRow(
          column(
            4,
            div(
              class = "summary-card",
              div(class = "summary-label", "Latest Date"),
              div(class = "summary-value", textOutput("latest_date_summary", inline = TRUE))
            )
          ),
          column(
            4,
            div(
              class = "summary-card",
              div(class = "summary-label", "Highest Latest Rate"),
              div(class = "summary-value", textOutput("highest_latest_rate_summary", inline = TRUE))
            )
          ),
          column(
            4,
            div(
              class = "summary-card",
              div(class = "summary-label", "Current Direction"),
              div(class = "summary-value", textOutput("direction_summary", inline = TRUE))
            )
          )
        ),
        
        div(
          class = "main-panel-card",
          h3("Core Time Series Plot"),
          plotOutput("core_plot", height = "420px"),
          p(
            "\n CD rates increased sharply during 2022 and 2023 as interest rates rose, then gradually declined after reaching their peak. The chart allows users to compare how different CD maturities have behaved over time."
          )
        )
      )
    )
  ),
  
  tabPanel(
    "Term Spread",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Date range:"),
        sliderInput(
          "spread_date_range",
          label = NULL,
          min = as.Date(min_date),
          max = as.Date(max_date),
          value = c(as.Date(min_date), as.Date(max_date)),
          timeFormat = "%b %Y"
        )
      ),
      mainPanel(
        width = 9,
        div(
          class = "main-panel-card",
          h3("Spread Between 48-Month and 12-Month CD Rates"),
          plotlyOutput("spread_plot", height = "620px"),
          p(
            strong("Interpretation guide:"),
            "\n Positive values indicate that the 48-month CD pays more than the 12-month CD. Negative values indicate an inverted term structure in which the shorter-term CD pays more. Hover over the line for the exact spread and date."
          )
        )
      )
    )
  ),
  
  tabPanel(
    "Month-to-Month Change",
    
    fluidRow(
      
      column(
        width = 4,
        
        # First separate box: checkboxes
        div(
          class = "well",
          
          checkboxGroupInput(
            "monthly_change_terms",
            "Select CD terms:",
            choices = term_choices,
            selected = term_choices
          )
        ),
        
        # Second separate box: explanatory text
        div(
          class = "sidebar-text-box",
          
          h4("Y-axis Interpretation"),
          
          p(
            "An important distinction to make here is that the y-axis does not represent
            the overall interest rate for each month for the CD mjtype. The y-axis represents
            how much the rate has increased or decreased for one month when compared to the previous month."
          )
        )
      ),
      
      column(
        width = 8,
        
        div(
          class = "main-panel-card",
          
          h3("Monthly Interest Rate Changes"),
          
          plotlyOutput(
            "monthly_change_plot",
            height = "620px"
          ),
          
          p(
            strong("Interpretation guide:"),
            " Positive bars indicate an increase from the previous month, while negative bars indicate a decrease. Hover over a bar to view the date, rate, and exact monthly change."
          )
        )
      )
    )
  ),
  
  tabPanel(
    "Forecast",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "forecast_term",
          "Select CD term:",
          choices = term_choices,
          selected = "12 Month"
        ),
        sliderInput(
          "horizon",
          "Forecast horizon, in months:",
          min = 3,
          max = 24,
          value = 12,
          step = 1
        ),
        sliderInput(
          "holdout",
          "Holdout test size, in months:",
          min = 6,
          max = 18,
          value = 12,
          step = 1
        ),
        radioButtons(
          "forecast_model",
          "Model to emphasize:",
          choices = model_choices,
          selected = "Ensemble"
        )
      ),
      mainPanel(
        fluidRow(
          column(
            4,
            div(
              class = "summary-card",
              div(class = "summary-label", "Selected Model"),
              div(class = "summary-value", textOutput("selected_model_summary", inline = TRUE))
            )
          ),
          column(
            4,
            div(
              class = "summary-card",
              div(class = "summary-label", "Best RMSE Model"),
              div(class = "summary-value", textOutput("best_model_summary", inline = TRUE))
            )
          ),
          column(
            4,
            div(
              class = "summary-card",
              div(class = "summary-label", "Forecast Horizon"),
              div(class = "summary-value", textOutput("horizon_summary", inline = TRUE))
            )
          )
        ),
        
        div(
          class = "main-panel-card",
          h3("Forecast Plot"),
          plotOutput("forecast_plot", height = "420px")
        ),
        
        div(
          class = "main-panel-card",
          h3("Accuracy Table on Holdout Set"),
          tableOutput("accuracy_table")
        ),
        
        div(
          class = "main-panel-card",
          h3("Recommendation"),
          verbatimTextOutput("forecast_recommendation")
        )
      )
    )
  ),
  
  tabPanel(
    "Simulator",
    sidebarLayout(
      sidebarPanel(
        dateInput(
          "invest_date",
          "Date invested:",
          value = as.Date(max_date),
          min = as.Date(min_date),
          max = as.Date(max_future_date)
        ),
        radioButtons(
          "sim_model",
          "Forecast model for future dates:",
          choices = "ARIMA",
          selected = "ARIMA"
        ),
        numericInput(
          "initial_amount",
          "Initial amount invested:",
          value = 1000,
          min = 1,
          step = 100
        )
      ),
      mainPanel(
        div(
          class = "main-panel-card",
          h3("Estimated CD Value at Maturity"),
          plotOutput("sim_plot", height = "500px")
        ),
        
        div(
          class = "main-panel-card",
          h3("Simulation Table"),
          tableOutput("sim_table"),
          p(
            "Due to the illiquidity of CDs, the rate at which a CD is opened cannot change. Calculating its value at maturity is done by using the rate observed at the date it is opened. If the investment date selected is outside the scope of the data, the corresponding rate is determined from the forecasting model selected."
          )
        )
      )
    )
  )
)

# -----------------------------
# 4. Server
# -----------------------------

server <- function(input, output, session) {
  
  explore_base <- reactive({
    req(input$explore_terms)
    
    cd |>
      filter(
        term %in% input$explore_terms,
        date >= yearmonth(input$date_range[1]),
        date <= yearmonth(input$date_range[2])
      )
  })
  
  output$latest_date_summary <- renderText({
    max(explore_base()$date) |> as.character()
  })
  
  output$highest_latest_rate_summary <- renderText({
    data <- explore_base()
    
    latest <- data |>
      as_tibble() |>
      filter(date == max(date)) |>
      arrange(desc(rate)) |>
      slice(1)
    
    paste0(round(latest$rate, 2), "%")
  })
  
  output$direction_summary <- renderText({
    data <- explore_base()
    
    latest_two <- data |>
      as_tibble() |>
      filter(term == "12 Month") |>
      arrange(date) |>
      slice_tail(n = 2)
    
    if (nrow(latest_two) < 2) {
      return("N/A")
    }
    
    if (latest_two$rate[2] > latest_two$rate[1]) {
      "Rising"
    } else if (latest_two$rate[2] < latest_two$rate[1]) {
      "Falling"
    } else {
      "Flat"
    }
  })
  
  output$core_plot <- renderPlot({
    explore_base() |>
      autoplot(rate, linewidth = 1.05) +
      labs(
        title = "National Average CD Rates",
        subtitle = "Historical National Average CD Rates from FRED",
        x = "Date",
        y = "Rate (%)",
        color = "Term"
      ) +
      scale_color_manual(values = c("#ff8200", "#4b5563", "#9ca3af")) +
      plot_theme
  })
  
  spread_data_full <- cd |>
    as_tibble() |>
    mutate(date = as.Date(date)) |>
    dplyr::select(date, term, rate) |>
    pivot_wider(names_from = term, values_from = rate) |>
    transmute(
      date,
      spread_48_12 = round(`48 Month` - `12 Month`, 3),
      regime = if_else(
        spread_48_12 >= 0,
        "Normal (long-term pays more)",
        "Inverted (short-term pays more)"
      ),
      hover_color = if_else(spread_48_12 >= 0, "#0073e6", "#e63946"),
      pos_spread = pmax(spread_48_12, 0),
      neg_spread = pmin(spread_48_12, 0)
    ) |>
    drop_na(spread_48_12)

  # Compute the true first inversion from the complete dataset, not the selected window.
  true_inversion_date <- spread_data_full |>
    filter(spread_48_12 < 0) |>
    slice_min(date, n = 1, with_ties = FALSE) |>
    pull(date)

  output$spread_plot <- renderPlotly({
    spread_data <- spread_data_full |>
      filter(
        date >= as.Date(input$spread_date_range[1]),
        date <= as.Date(input$spread_date_range[2])
      )

    req(nrow(spread_data) > 0)

    show_inversion <- length(true_inversion_date) > 0 &&
      true_inversion_date >= as.Date(input$spread_date_range[1]) &&
      true_inversion_date <= as.Date(input$spread_date_range[2])

    y_min <- min(spread_data$spread_48_12)
    y_max <- max(spread_data$spread_48_12)

    spread_shapes <- list(
      list(
        type = "line",
        x0 = min(spread_data$date), x1 = max(spread_data$date),
        y0 = 0, y1 = 0,
        line = list(color = "#4b5563", dash = "dash", width = 1)
      )
    )
    spread_annotations <- list()

    if (show_inversion) {
      spread_shapes[[2]] <- list(
        type = "line",
        x0 = true_inversion_date, x1 = true_inversion_date,
        y0 = y_min * 1.05, y1 = y_max * 1.20,
        line = list(color = "#e63946", dash = "dot", width = 1.5)
      )
      spread_annotations[[1]] <- list(
        x = true_inversion_date, y = y_max * 1.15,
        text = paste0("Inversion begins:<br>", format(true_inversion_date, "%b %Y")),
        showarrow = FALSE, font = list(color = "#e63946", size = 13),
        xanchor = "left"
      )
    }

    plot_ly(spread_data, x = ~date) |>
      add_ribbons(
        ymin = 0, ymax = ~pos_spread,
        name = "Normal (long-term pays more)",
        fillcolor = "rgba(0, 115, 230, 0.45)",
        line = list(color = "transparent"), hoverinfo = "skip"
      ) |>
      add_ribbons(
        ymin = ~neg_spread, ymax = 0,
        name = "Inverted (short-term pays more)",
        fillcolor = "rgba(230, 57, 70, 0.45)",
        line = list(color = "transparent"), hoverinfo = "skip"
      ) |>
      add_trace(
        y = ~spread_48_12, type = "scatter", mode = "lines",
        line = list(color = "black", width = 2),
        name = "Spread", showlegend = FALSE,
        hovertemplate = "%{y:+.3f} pts<br>%{x|%B %Y}<extra></extra>",
        hoverlabel = list(bgcolor = ~hover_color, font = list(color = "white", size = 13))
      ) |>
      layout(
        xaxis = list(
          title = "", type = "date", tickformat = "%b %Y", dtick = "M3",
          tickangle = -45, showspikes = TRUE, spikemode = "across",
          spikesnap = "cursor", spikedash = "dot", spikecolor = "#888888"
        ),
        yaxis = list(
          title = "48-month rate minus 12-month rate (percentage points)",
          zeroline = FALSE, showspikes = TRUE, spikemode = "across",
          spikedash = "dot", spikecolor = "#888888"
        ),
        shapes = spread_shapes, annotations = spread_annotations,
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.04, yanchor = "bottom"),
        hovermode = "x", margin = list(t = 90)
      )
  })

  monthly_changes <- cd |>
    as_tibble() |>
    mutate(date = as.Date(date)) |>
    group_by(term) |>
    arrange(date, .by_group = TRUE) |>
    mutate(monthly_change = rate - lag(rate)) |>
    ungroup() |>
    drop_na(monthly_change)
  
  output$monthly_change_plot <- renderPlotly({
    req(length(input$monthly_change_terms) > 0)
    
    plot_data <- monthly_changes |>
      filter(term %in% input$monthly_change_terms) |>
      mutate(
        term = factor(term, levels = input$monthly_change_terms),
        change_direction = if_else(
          monthly_change >= 0,
          "Increase",
          "Decrease"
        ),
        hover_text = paste0(
          "<b>", term, "</b>",
          "<br>Date: ", format(date, "%B %Y"),
          "<br>Rate: ", round(rate, 3), "%",
          "<br>Monthly change: ",
          sprintf("%+.3f", monthly_change),
          " percentage points"
        )
      )
    
    monthly_change_ggplot <- ggplot(
      plot_data,
      aes(
        x = date,
        y = monthly_change,
        fill = change_direction,
        text = hover_text
      )
    ) +
      geom_col(width = 25, show.legend = FALSE) +
      geom_hline(yintercept = 0, linewidth = 0.4) +
      facet_wrap(~term, ncol = 1, scales = "free_y") +
      scale_fill_manual(
        values = c(
          "Increase" = "#00BFC4",
          "Decrease" = "#F8766D"
        )
      ) +
      scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
      labs(x = NULL, y = "Monthly change (percentage points)") +
      theme_classic() +
      theme(
        strip.text = element_text(face = "bold", size = 12),
        strip.background = element_rect(fill = "white", color = "black"),
        panel.spacing = grid::unit(1, "lines")
      )
    
    ggplotly(monthly_change_ggplot, tooltip = "text") |>
      layout(hovermode = "closest")
  })
  
  forecast_results <- reactive({
    fit_forecast_models(
      cd,
      input$forecast_term,
      input$holdout,
      input$horizon
    )
  })
  
  output$selected_model_summary <- renderText({
    input$forecast_model
  })
  
  output$best_model_summary <- renderText({
    forecast_results()$accuracy$.model[1]
  })
  
  output$horizon_summary <- renderText({
    paste0(input$horizon, " mo.")
  })
  
  output$forecast_plot <- renderPlot({
    res <- forecast_results()
    
    model_to_show <- input$forecast_model
    
    fc <- res$future_fc |>
      filter(.model == model_to_show)
    
    history <- cd |>
      filter(term == input$forecast_term) |>
      as_tibble() |>
      dplyr::select(date, rate) |>
      arrange(date)
    
    ggplot() +
      geom_line(
        data = history,
        aes(x = date, y = rate),
        linewidth = 1.05,
        color = "#4b5563"
      ) +
      geom_line(
        data = fc,
        aes(x = date, y = .mean),
        linewidth = 1.15,
        color = "#ff8200"
      ) +
      geom_point(
        data = fc,
        aes(x = date, y = .mean),
        size = 2,
        color = "#ff8200"
      ) +
      labs(
        title = paste(
          model_to_show,
          "forecast for",
          input$forecast_term,
          "CD rates"
        ),
        x = "Date",
        y = "Rate (%)"
      ) +
      plot_theme
  })
  
  output$accuracy_table <- renderTable({
    forecast_results()$accuracy |>
      as_tibble() |>
      dplyr::select(.model, ME, RMSE, MAE) |>
      mutate(across(where(is.numeric), ~ round(.x, 4)))
  })
  
  output$forecast_recommendation <- renderText({
    acc <- forecast_results()$accuracy
    best <- acc$.model[1]
    
    paste0(
      "Recommended model based on current holdout: ", best, "\n\n",
      "Reason: it has the lowest RMSE on the selected holdout period. RMSE is useful here because CD rates are measured in percentage points and larger errors are penalized more heavily. The Ensemble model averages the Mean, Naive, ARIMA, and TSLM forecasts, which can make the forecast more stable by reducing dependence on a single model assumption. However, because CD rates are highly persistent, ARIMA and Naive may still outperform the Ensemble in some holdout windows.","\n\n","Tip: The Ensemble Model is a combination of Mean, Naive, ARIMA, and TSLM models by averaging their predicted values. Rather than relying on just one method, this model increases stability and limits the forecast's dependence on a singular model's accuracy."
    )
  })
  
  sim_data <- reactive({
    
    invest_ym <- yearmonth(input$invest_date)
    
    sim_rates <- get_simulation_rates(
      data = cd,
      invest_ym = invest_ym,
      selected_model = input$sim_model
    )
    
    validate(
      need(
        nrow(sim_rates) == 3,
        "Forecast rates could not be produced for all three CD terms."
      ),
      need(
        all(is.finite(sim_rates$rate)),
        "The ARIMA model returned invalid forecast rates."
      )
    )
    
    sim_rates |>
      mutate(
        term_months = as.numeric(
          str_extract(as.character(term), "\\d+")
        ),
        
        initial_amount = input$initial_amount,
        
        maturity_value =
          initial_amount *
          (1 + rate / 100 / 12) ^ term_months,
        
        interest_earned =
          maturity_value - initial_amount,
        
        percent_gain =
          (interest_earned / initial_amount) * 100,
        
        annualized_gain =
          (
            (maturity_value / initial_amount) ^
              (12 / term_months) - 1
          ) * 100
      )
    
  })
  
  
  output$sim_plot <- renderPlot({
    
    ggplot(
      sim_data(),
      aes(
        x = term,
        y = maturity_value,
        fill = term
      )
    ) +
      
      geom_col(
        show.legend = FALSE,
        width = 0.65
      ) +
      
      geom_text(
        aes(
          label = paste0(
            dollar(round(maturity_value, 2)),
            "\n+",
            round(percent_gain, 1),
            "% total",
            "\n",
            round(annualized_gain, 1),
            "% annualized"
          )
        ),
        vjust = -0.3,
        fontface = "bold",
        lineheight = 1.1
      ) +
      
      labs(
        title = paste(
          "Projected Value at CD Maturity",
          ifelse(
            yearmonth(input$invest_date) > max_date,
            paste(
              "using",
              input$sim_model,
              "forecasted rates"
            ),
            "using observed historical rates"
          )
        ),
        subtitle =
          "Total gain reflects different holding periods; annualized return allows comparison across terms",
        x = "CD Term",
        y = "Maturity Value"
      ) +
      
      scale_fill_manual(
        values = c(
          "#ff8200",
          "#4b5563",
          "#9ca3af"
        )
      ) +
      
      scale_y_continuous(
        labels = dollar_format(),
        expand = expansion(
          mult = c(0, 0.20)
        )
      ) +
      
      plot_theme
  })
  
  
  output$sim_table <- renderTable({
    
    sim_data() |>
      transmute(
        Term = term,
        `Investment Month` = as.character(date),
        `Rate Source` = rate_source,
        `Rate (%)` = round(rate, 3),
        `Initial Amount` = dollar(initial_amount),
        `Estimated Maturity Value` =
          dollar(round(maturity_value, 2)),
        `Estimated Interest Earned` =
          dollar(round(interest_earned, 2)),
        `Percent Gain` =
          paste0(round(percent_gain, 1), "%"),
        `Annualized Gain` =
          paste0(round(annualized_gain, 1), "%")
      )
  })
}

shinyApp(ui = ui, server = server)

