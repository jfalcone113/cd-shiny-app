library(shiny)
library(plotly)
library(tidyverse)

cd_full <- read_csv("cd_full.csv") |>
  mutate(date = ymd(date))

cd_spread_full <- cd_full |>
  mutate(
    spread_48_12 = round(term48 - term12, 3),
    regime = if_else(spread_48_12 >= 0,
                     "Normal (long-term pays more)",
                     "Inverted (short-term pays more)"),
    hover_color = if_else(spread_48_12 >= 0, "#0073e6", "#e63946"),
    pos_spread = pmax(spread_48_12, 0),
    neg_spread = pmin(spread_48_12, 0)
  )

# Compute the TRUE inversion date once, from the full dataset (not the filtered one)
true_inversion_date <- cd_spread_full |>
  filter(spread_48_12 < 0) |>
  slice_min(date) |>
  pull(date)

ui <- fluidPage(
  titlePanel("Spread Between 48-Month and 12-Month CD Rates"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Date range:"),
      sliderInput(
        inputId = "date_range",
        label = NULL,
        min = min(cd_spread_full$date),
        max = max(cd_spread_full$date),
        value = c(min(cd_spread_full$date), max(cd_spread_full$date)),
        timeFormat = "%b %Y"
      )
    ),
    mainPanel(
      width = 9,
      plotlyOutput("spread_plot", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  
  cd_spread_filtered <- reactive({
    cd_spread_full |>
      filter(date >= input$date_range[1], date <= input$date_range[2])
  })
  
  output$spread_plot <- renderPlotly({
    df <- cd_spread_filtered()
    req(nrow(df) > 0)
    
    # Only show the inversion marker if the TRUE inversion date
    # falls inside the currently selected date range
    show_inversion <- true_inversion_date >= input$date_range[1] &&
      true_inversion_date <= input$date_range[2]
    
    y_min <- min(df$spread_48_12)
    y_max <- max(df$spread_48_12)
    
    shapes_list <- list(
      list(type = "line", x0 = min(df$date), x1 = max(df$date),
           y0 = 0, y1 = 0, line = list(color = "gray30", dash = "dash", width = 1))
    )
    annotations_list <- list()
    
    if (show_inversion) {
      shapes_list[[2]] <- list(
        type = "line", x0 = true_inversion_date, x1 = true_inversion_date,
        y0 = y_min * 1.05, y1 = y_max * 1.2,
        line = list(color = "red", dash = "dot", width = 1.5)
      )
      annotations_list[[1]] <- list(
        x = true_inversion_date, y = y_max * 1.15,
        text = paste0("Inversion begins:<br>", format(true_inversion_date, "%b %Y")),
        showarrow = FALSE, font = list(color = "red", size = 13),
        xanchor = "left"
      )
    }
    
    plot_ly(df, x = ~date) |>
      add_ribbons(
        ymin = 0, ymax = ~pos_spread,
        name = "Normal (long-term pays more)",
        fillcolor = "rgba(0, 115, 230, 0.45)",
        line = list(color = "transparent"),
        hoverinfo = "skip"
      ) |>
      add_ribbons(
        ymin = ~neg_spread, ymax = 0,
        name = "Inverted (short-term pays more)",
        fillcolor = "rgba(230, 57, 70, 0.45)",
        line = list(color = "transparent"),
        hoverinfo = "skip"
      ) |>
      add_trace(
        y = ~spread_48_12,
        type = "scatter", mode = "lines",
        line = list(color = "black", width = 2),
        name = "Spread", showlegend = FALSE,
        hovertemplate = "%{y:+.3f} pts<br>%{x|%B %Y}<extra></extra>",
        hoverlabel = list(
          bgcolor = ~hover_color,
          font = list(color = "white", size = 13)
        )
      ) |>
      layout(
        title = list(text = "Spread Between 48-Month and 12-Month CD Rates",
                     x = 0.02, y = 1.25, xanchor = "left", yanchor = "top",
                     font = list(size = 18)),
        xaxis = list(
          title = "", type = "date",
          tickformat = "%b %Y", tickangle = -45,
          showspikes = TRUE, spikemode = "across", spikesnap = "cursor",
          spikedash = "dot", spikethickness = 1, spikecolor = "#888888"
        ),
        yaxis = list(
          title = "48-Month Rate minus 12-Month Rate (percentage points)",
          zeroline = FALSE,
          showspikes = TRUE, spikemode = "across",
          spikedash = "dot", spikethickness = 1, spikecolor = "#888888"
        ),
        shapes = shapes_list,
        annotations = annotations_list,
        legend = list(orientation = "h", x = 0.5, xanchor = "center",
                      y = 1.08, yanchor = "bottom"),
        hovermode = "x",
        margin = list(t = 150)
      )
  })
}

shinyApp(ui, server)