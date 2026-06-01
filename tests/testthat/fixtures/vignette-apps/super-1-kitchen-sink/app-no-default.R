pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
# super-1-kitchen-sink (no-default variant): same formula structure as
# app.R but every `pp*(default)` call is stripped of its positional default.
# Path-A pressure-test variant per ADR-0016 (the with-default `app.R` is the
# canonical Path-B-evaluable form; this peer file is Path-A only and
# exercises the parser's no-positional-default arrival paths into each
# build_ui hook).

ppRange <- ptr_define_placeholder_value(
  keyword = "ppRange",
  build_ui = function(node, label = "Range", selected = NULL, ...) {
    v <- if (is.numeric(selected) && length(selected) == 2L) {
      as.numeric(selected)
    } else c(0, 1)
    shiny::sliderInput(node$id, label, min = 0, max = 100, value = v)
  },
  resolve_expr = function(value, ...) value,
  parse_positional_arg = ptr_arg_numeric(vector = TRUE, length = 2)
)

color_var <- expr(cyl)

formula1 <- expr(
  ggplot(
    mtcars |>
      dplyr::filter(ppExpr()) |>
      dplyr::mutate(adj = ppExpr()) |>
      ppVerbSwitch(dplyr::slice_max(mpg, n = 15), TRUE, label = "Top 15 by mpg"),
    aes(x = ppVar(), y = ppVar(), color = ppVar(shared = "grp"))
  ) +
    geom_point(size = ppNum(), alpha = ppNum()) +
    geom_smooth(method = ppText(), linewidth = ppNum(shared = "lw")) +
    ppLayerOff(geom_line(linewidth = ppNum(shared = "lw")), TRUE) +
    facet_wrap(vars(ppVar(shared = "grp"))) +
    scale_y_continuous(limits = ppRange()) +
    labs(title = ppText(), subtitle = ppText())
)

ptr_app(
  !!formula1,
  ui_text = list(
    defaults = list(
      ppNum  = list(label = "{param}"),
      ppText = list(label = "{param} text")
    ),
    params = list(
      linewidth = list(ppNum = list(label = "Line thickness"))
    )
  )
)
