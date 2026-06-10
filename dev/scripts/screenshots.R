library(ggpaintr)
library(rlang)        # supplies %||% (base R provides it only from 4.4)
library(shiny)
library(ggpcp)
library(dplyr)
library(palmerpenguins)

# custom consumer placeholder: pick several columns at once
ppVars <- ptr_define_placeholder_consumer(
  keyword = "ppVars",
  build_ui = function(node, cols, data, label = NULL, selected = NULL, ...) {
    retained <- intersect(selected %||% character(0), cols)
    selectizeInput(node$id, label = label %||% "Columns",
                   choices = union(retained, cols), selected = retained, multiple = TRUE,
                   options = list(plugins = list("drag_drop")))

  },
  resolve_expr = function(value, node, ...) {
    if (length(value) == 0L) return(NULL)
    rlang::call2("c", !!!as.list(value))   # c(col1, col2, ...) for pcp_select
  },
  parse_positional_arg = ptr_arg_string(vector = TRUE),
  ui_text_defaults = list(label = "Columns for {param}")
)

# custom value placeholder: choose a scaling method
ppScaleMethod <- ptr_define_placeholder_value(
  keyword = "ppScaleMethod",
  build_ui = function(node, label = NULL, selected = NULL, ...) {
    choices <- c(
      "Raw (no scaling)"                  = "raw",
      "Standardize (mean / sd)"           = "std",
      "Robust (median / MAD)"             = "robust",
      "Uni min-max (per variable 0..1)"   = "uniminmax",
      "Global min-max (across vars 0..1)" = "globalminmax"
    )
    sel <- if (length(selected) == 1L && nzchar(selected)) selected else "raw"
    selectInput(node$id, label = label %||% "Scaling method",
                choices = choices, selected = sel)
  },
  resolve_expr = function(value, node, ...) {
    if (length(value) != 1L || is.na(value) || !nzchar(value)) return(NULL)
    as.character(value)
  },
  parse_positional_arg = ptr_arg_string(),
  ui_text_defaults = list(label = "Scaling method for {param}")
)

ptr_app(
  ppUpload(penguins) |>
    ppVerbSwitch(filter(!is.na(sex)), TRUE,
                 label = "Drop rows with missing sex") |>
    pcp_select(
      ppVars(c("bill_depth_mm", "bill_length_mm",
               "flipper_length_mm", "body_mass_g",
               "sex", "species"))) |>
    pcp_scale(method = ppScaleMethod("uniminmax")) |>
    pcp_arrange() |>
    ggplot(aes_pcp()) +
    geom_pcp_axes() +
    geom_pcp(aes(colour = ppVar(species)),
             alpha = ppNum(0.8), overplot = "none") +
    geom_pcp_labels()
)


library(shiny)
library(ggpaintr)
library(plotly)
library(palmerpenguins)

plots <- list(
  expr(ggplot(penguins, aes(x = ppVar(flipper_length_mm),
                            y = ppVar(body_mass_g),
                            color = ppVar(species, shared = "grp"))) +
         geom_point(alpha = 0.7)),
  expr(ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm,
                            color = ppVar(species, shared = "grp"))) +
         geom_point(alpha = 0.7))
)

obj <- ptr_shared(plots)

ui <- fluidPage(
  titlePanel("Penguins, two views"),
  ptr_shared_panel(obj),
  fluidRow(
    column(6, ptr_ui(plots[[1]], "body", shared = obj)),
    column(6, ptr_ui_controls(plots[[2]], "bill", shared = obj),
           plotly::plotlyOutput(NS("bill")("scatter")))
  )
)

server <- function(input, output, session) {
  sh <- ptr_shared_server(obj)
  ptr_server(plots[[1]], "body", shared_state = sh)
  state <- ptr_server(plots[[2]], "bill", shared_state = sh)

  output[[NS("bill")("scatter")]] <- plotly::renderPlotly({
    res <- state$runtime()
    shiny::req(isTRUE(res$ok), res$plot)
    plotly::ggplotly(res$plot)
  })
}

shinyApp(ui, server)


ggplot(
  data = penguins |>
    filter(!is.na(sex)) |>
    pcp_select(
      c("species", "island", "flipper_length_mm", "body_mass_g", "sex")
    ) |>
    pcp_scale(method = "std") |>
    pcp_arrange(),
  aes_pcp()
) +
  geom_pcp_axes() +
  geom_pcp(aes(colour = species), alpha = 0.8, overplot = "none") +
  geom_pcp_labels()



library(ggpaintr)

ptr_app(
  ggplot(iris, aes(x = ppVar(Sepal.Length),
                   y = ppVar(Sepal.Width),
                   color = ppVar(Species))) +
    geom_point()
)

library(ggpaintr)

ptr_app(
  iris |>
    group_by(ppVar) |>
    summarise(
      mean_val = mean(ppVar),
    ) |>
    ggplot(aes(x = ppVar, y = ppVar)) +
    geom_col()
)




