

cd.12 <- read.csv('https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23ebf3fb&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1320&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=NDR12MCD&scale=left&cosd=2021-04-01&coed=2026-06-01&line_color=%230073e6&link_values=false&line_style=solid&mark_type=none&mw=3&lw=3&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2021-04-01&line_index=1&transformation=lin&vintage_date=2026-07-19&revision_date=2026-07-19&nd=2021-04-01')

cd.24 <- read.csv('https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23ebf3fb&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1320&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=NDR24MCD&scale=left&cosd=2021-04-01&coed=2026-06-01&line_color=%230073e6&link_values=false&line_style=solid&mark_type=none&mw=3&lw=3&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2021-04-01&line_index=1&transformation=lin&vintage_date=2026-07-19&revision_date=2026-07-19&nd=2021-04-01')

cd.48 <- read.csv('https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23ebf3fb&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1320&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=NDR48MCD&scale=left&cosd=2021-04-01&coed=2026-06-01&line_color=%230073e6&link_values=false&line_style=solid&mark_type=none&mw=3&lw=3&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2021-04-01&line_index=1&transformation=lin&vintage_date=2026-07-19&revision_date=2026-07-19&nd=2021-04-01')

cd.full <- left_join(cd.12,cd.24) |>
  left_join(cd.48) |>
  mutate(observation_date = ymd(observation_date)) |>
  rename(date = observation_date,
         term12 = NDR12MCD,
         term24 = NDR24MCD,
         term48 = NDR48MCD)

rm(cd.12, cd.24, cd.48)


library(shiny)
library(tidyverse)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(

  tabPanel(
    "Monthly Interest Rate Changes",
    
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(
          inputId = "monthly_change_terms",
          label = "Select CD terms:",
          choices = c(
            "12-month CD",
            "24-month CD",
            "48-month CD"
          ),
          selected = c(
            "12-month CD",
            "48-month CD"
          )
        )
      ),
      
      mainPanel(
        div(
          class = "main-panel-card",
          
          plotlyOutput(
            outputId = "monthly_change_plot",
            height = "620px"
          ),
          
          p(
            strong("Interpretation guide:"),
            paste(
              "Positive bars indicate an increase from the previous month,",
              "while negative bars indicate a decrease. Hover over a bar",
              "to view the date and exact monthly change."
            )
          )
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  cd.changes <- cd.full |>
    arrange(date) |>
    pivot_longer(
      cols = c(term12, term24, term48),
      names_to = "term",
      values_to = "rate"
    ) |>
    group_by(term) |>
    mutate(
      monthly_change = rate - lag(rate)
    ) |>
    ungroup() |>
    drop_na(monthly_change) |>
    mutate(
      term = recode(
        term,
        term12 = "12-month CD",
        term24 = "24-month CD",
        term48 = "48-month CD"
      )
    )
  
  output$monthly_change_plot <- renderPlotly({
    
    # Require at least one selected CD term
    req(input$monthly_change_terms)
    
    plot_data <- cd.changes |>
      filter(term %in% input$monthly_change_terms) |>
      mutate(
        # Keep facets in the same order as the user's selections
        term = factor(
          term,
          levels = input$monthly_change_terms
        ),
        
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
      geom_col(
        width = 25,
        show.legend = FALSE
      ) +
      geom_hline(
        yintercept = 0,
        linewidth = 0.4
      ) +
      facet_wrap(
        ~ term,
        ncol = 1
      ) +
      scale_fill_manual(
        values = c(
          "Increase" = "#00BFC4",
          "Decrease" = "#F8766D"
        )
      ) +
      scale_x_date(
        date_breaks = "6 months",
        date_labels = "%b\n%Y"
      ) +
      labs(
        x = NULL,
        y = "Monthly change in percentage points"
      ) +
      theme_classic() +
      theme(
        strip.text = element_text(
          face = "bold",
          size = 12
        ),
        strip.background = element_rect(
          fill = "white",
          color = "black"
        ),
        panel.spacing = unit(1, "lines")
      )
    
    ggplotly(
      monthly_change_ggplot,
      tooltip = "text"
    ) |>
      layout(
        hovermode = "closest"
      )
  })
}  
# Run the application 
shinyApp(ui = ui, server = server)
