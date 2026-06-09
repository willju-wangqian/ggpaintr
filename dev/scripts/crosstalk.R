library(plotly)
library(crosstalk)
library(DT)

# 1. Wrap your dataset inside a SharedData object
# This acts as a centralized event bus to link your HTML widgets together
shared_mtcars <- SharedData$new(mtcars)

# 2. Structure your layout and components using bscols
p <- bscols(
  widths = c(3, 9), # Controls column widths on a 12-point grid bootstrap scale

  # Left Column: Interactive HTML Filter Controls
  list(
    filter_checkbox("cyl", "Cylinders", shared_mtcars, ~cyl, inline = TRUE),
    filter_slider("hp", "Horsepower", shared_mtcars, ~hp, width = "100%"),
    filter_select("gear", "Gears", shared_mtcars, ~gear, multiple = TRUE)
  ),

  # Right Column: Linked Plotly Visualizations and Interactive Data Table
  list(
    # Interlinked Plotly Scatter Plot
    plot_ly(shared_mtcars, x = ~wt, y = ~mpg, color = ~factor(cyl),
            type = "scatter", mode = "markers", size = I(100)) %>%
      layout(
        title = "Weight vs MPG (Linked Brushing Active)",
        dragmode = "select" # Activates box/lasso tool selection by default
      ) %>%
      highlight(on = "plotly_selected", off = "plotly_deselect"),

    tags$br(), # Visual HTML line break

    # Interlinked Interactive Data Table
    datatable(shared_mtcars, style = "bootstrap", width = "100%",
              options = list(pageLength = 5))
  )
)

htmltools::save_html(p, "/tmp/crosstalk.html"); browseURL("/tmp/crosstalk.html")
