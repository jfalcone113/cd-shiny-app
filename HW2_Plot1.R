library(tidyverse)

cd_full <- read_csv("cd_full.csv") |>
  mutate(date = ymd(date))

cd_spread <- cd_full |>
  mutate(
    spread_48_12 = term48 - term12,
    regime = if_else(spread_48_12 >= 0, 
                     "Normal (long-term pays more)", 
                     "Inverted (short-term pays more)")
  )

inversion_date <- cd_spread |>
  filter(spread_48_12 < 0) |>
  slice_min(date) |>
  pull(date)

ggplot(cd_spread, aes(x = date, y = spread_48_12)) +
  geom_area(aes(fill = regime), alpha = 0.6) +
  geom_line(linewidth = 1, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  geom_vline(xintercept = inversion_date, linetype = "dotted", color = "red") +
  annotate("text", x = inversion_date, y = 0.15,
           label = paste("Inversion begins:\n", format(inversion_date, "%b %Y")),
           hjust = -0.05, size = 4, color = "red", fontface = "bold") +
  scale_fill_manual(values = c(
    "Normal (long-term pays more)" = "#0073e6",
    "Inverted (short-term pays more)" = "#e63946"
  )) +
  labs(
    x = NULL,
    y = "48-Month Rate minus 12-Month Rate (percentage points)",
    fill = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top", plot.title = element_text(face = "bold"))






library(tidyverse)
library(plotly)

cd_full <- read_csv("cd_full.csv") |>
  mutate(date = ymd(date))

cd_spread <- cd_full |>
  mutate(
    spread_48_12 = round(term48 - term12, 3),   # <- FIX 1: round to 3 decimals
    regime = if_else(spread_48_12 >= 0,
                     "Normal (long-term pays more)",
                     "Inverted (short-term pays more)"),
    hover_color = if_else(spread_48_12 >= 0, "#0073e6", "#e63946"),  # <- FIX 2 setup
    pos_spread = pmax(spread_48_12, 0),
    neg_spread = pmin(spread_48_12, 0)
  )

inversion_date <- cd_spread |>
  filter(spread_48_12 < 0) |>
  slice_min(date) |>
  pull(date)

p <- plot_ly(cd_spread, x = ~date) |>
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
    hoverlabel = list(                          # <- FIX 2: colored hover box
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
      tickformat = "%b %Y", dtick = "M3", tickangle = -45,
      showspikes = TRUE, spikemode = "across", spikesnap = "cursor",
      spikedash = "dot", spikethickness = 1, spikecolor = "#888888"
    ),
    yaxis = list(
      title = "48-Month Rate minus 12-Month Rate (percentage points)",
      zeroline = FALSE,
      showspikes = TRUE, spikemode = "across",
      spikedash = "dot", spikethickness = 1, spikecolor = "#888888"
    ),
    shapes = list(
      list(type = "line", x0 = min(cd_spread$date), x1 = max(cd_spread$date),
           y0 = 0, y1 = 0, line = list(color = "gray30", dash = "dash", width = 1)),
      list(type = "line", x0 = inversion_date, x1 = inversion_date,
           y0 = min(cd_spread$spread_48_12) * 1.05,
           y1 = max(cd_spread$spread_48_12) * 1.2,
           line = list(color = "red", dash = "dot", width = 1.5))
    ),
    annotations = list(
      list(x = inversion_date, y = max(cd_spread$spread_48_12) * 1.15,
           text = paste0("Inversion begins:<br>", format(inversion_date, "%b %Y")),
           showarrow = FALSE, font = list(color = "red", size = 13),
           xanchor = "left")
    ),
    legend = list(orientation = "h", x = 0.5, xanchor = "center",
                  y = 1.08, yanchor = "bottom"),   # <- FIX 3: pulled down from title
    hovermode = "x",
    margin = list(t = 150)                          # <- FIX 3: more headroom overall
  )

p
