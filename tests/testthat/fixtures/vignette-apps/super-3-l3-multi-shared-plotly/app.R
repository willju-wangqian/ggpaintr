# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. Body below is the super-app-3 (ADR 0013 §App-3) pressure
# fixture: hand-rolled L3 with two pre-quoted formulas, a shared coordinator
# spanning both cells, and a plotly custom output on cell B. Not paired with
# any vignette chunk -- this fixture exists only to pressure-test the matrix.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)

# Local custom value placeholder (ADR 0013 §App-3; shape matches App-1's).
# Registration is local to this child app process (project memory
# `shinytest2-appdir-pkgload`), so no cross-test contamination.
# `ppRange <-` binding required for Path-B evaluability (ADR-0016).
ppRange <- ptr_define_placeholder_value(
  keyword     = "ppRange",
  default_arg = ptr_default_numeric_vector(length = 2),
  build_ui    = function(node, label = NULL, selected = NULL, ...) {
    val <- if (!is.null(selected) && length(selected) == 2L) selected else c(0, 100)
    # `step = 0.1` keeps the returned values as doubles (not integers); a
    # whole-number step would coerce to `int`, and `deparse(c(12L, 38L))`
    # diverges from the test's expected literal `c(12, 38)`.
    shiny::sliderInput(node$id, label = label %||% "Range",
                       min = -100, max = 100, value = val, step = 0.1)
  },
  resolve_expr = function(value, node, ...) {
    if (is.null(value) || length(value) != 2L) return(NULL)
    # Force double so the rendered code emits `c(12, 38)` not `c(12L, 38L)`.
    # Shiny's sliderInput can return integers when slider min/max/step are
    # all integer-typed; the placeholder contract is "any numeric pair".
    value <- as.double(value)
    rlang::expr(c(!!value[1], !!value[2]))
  }
)

# G4 row: both formulas live as local `rlang::expr()` bindings. `ptr_server`
# captures them via `enexpr` + sys.frames-fallback resolution (G6). A string
# formula would degrade to G1 and defeat the App-3 pressure surface.
formula_a <- rlang::expr(
  ggplot(mtcars, aes(x = ppVar(mpg), y = ppVar(wt),
                     color = ppVar(cyl, shared = "linked"))) +
    geom_point(size = ppNum(2), alpha = ppNum(0.7)) +
    scale_x_continuous(limits = ppRange(c(10, 40))) +
    labs(title = ppText("Cell A: ggplot"))
)
formula_b <- rlang::expr(
  ggplot(mtcars, aes(x = ppVar(hp), y = ppVar(qsec),
                     color = ppVar(cyl, shared = "linked"))) +
    geom_point(size = ppNum(2)) +
    geom_smooth(method = ppText("lm"), linewidth = ppNum(1)) +
    labs(title = ppText("Cell B: plotly"))
)
# `ptr_ui_*` and `ptr_shared` take string-only formulas (they call
# `ptr_translate` directly, no `enexpr` capture). Provide deparsed strings
# for them. `ptr_server` BELOW still receives the pre-quoted `rlang::expr`
# bindings (its `enexpr` path is the G4 + G6 capture pressure surface).
formula_a_str <- paste(deparse(formula_a), collapse = "\n")
formula_b_str <- paste(deparse(formula_b), collapse = "\n")

# Shared coordinator carrying the cross-cell `"linked"` key. Reaches both
# cells (default-ggplot output and plotly custom output) -- the cross-output-
# backend propagation surface (ADR 0013 §App-3). `ptr_shared()` reads
# formulas only to discover shared keys + partition them; it requires string
# form (`shared_translate_formulas()` in R/paintr-shared-ui.R asserts
# `is_string`). Deparse the pre-quoted exprs here -- `ptr_server` below still
# receives the unparsed `rlang::expr` bindings, preserving the G4 capture
# pressure surface.
# `shared_ui` is no longer supported (see ?ptr_shared). The `"linked"` key is
# `color = ppVar(cyl, shared = "linked")` -- a `var` consumer. With no override
# it auto-renders from `ppVar`'s own `build_ui` (the reactive column picker),
# which honours the formula default `cyl` at boot -- exactly the seeding the
# old custom `selectInput(names(mtcars))` override discarded.
shared <- ptr_shared(
  formulas  = list(formula_a_str, formula_b_str)
)

ui <- ptr_ui_page(
  ptr_shared_panel(shared),
  shiny::fluidRow(
    # Cell A: default ggpaintr-managed plot output + L3 controls + code panel.
    shiny::column(
      6,
      ptr_ui_controls(formula_a_str, "plot1"),
      ptr_ui_plot("plot1"),
      ptr_ui_code("plot1")
    ),
    # Cell B: plotly custom output wrapped by the </> code-toggle, paired
    # with its own code panel.
    shiny::column(
      6,
      ptr_ui_controls(formula_b_str, "plot2"),
      plotly::plotlyOutput(shiny::NS("plot2")("custom_plot"), height = "500px") |>
        ptr_ui_toggle_code(ptr_ui_code("plot2"))
    )
  )
)

server <- function(input, output, session) {
  shared_state <- ptr_shared_server(shared)
  state_a <- ptr_server(formula_a, "plot1", shared_state = shared_state)
  state_b <- ptr_server(formula_b, "plot2", shared_state = shared_state)
  # F4 + F7 + F8: read `state$runtime()$plot` inside renderPlotly. `state$tree()`
  # is also exposed for downstream consumers (not asserted here, but the
  # surface stays available).
  output[[shiny::NS("plot2")("custom_plot")]] <- plotly::renderPlotly({
    res <- state_b$runtime()
    shiny::req(isTRUE(res$ok), res$plot)
    plotly::ggplotly(res$plot)
  })
}

shiny::shinyApp(ui, server)
