# gallery-examples.R
# Standalone ggplot2 (and extension-package) graphics extracted from the retired
# vignette archive/retired_vignettes/ggpaintr-gallery.Rmd. These are the plain
# "original" plots each gallery section pairs with a ptr_app() formula — runnable
# as-is at the R prompt. Install the extension package named in each section's
# header if you do not have it.

# ppNum, ppText, ppExpr, ppVar, ppUpload

ppRange <- ptr_define_placeholder_value(
  keyword = "ppRange",
  build_ui = function(node, label = NULL, selected = NULL, ...) {
    v <- suppressWarnings(as.numeric(selected))
    initial <- if (length(v) == 2L && all(is.finite(v))) v else c(0, 50)
    shiny::sliderInput(node$id, label = label %||% "Range",
                       min = -100, max = 100, value = initial, step = 1)
  },
  resolve_expr = function(value, node, ...) {
    if (is.null(value) || length(value) != 2L) return(NULL)
    rlang::expr(c(!!value[1], !!value[2]))
  },
  ui_text_defaults = list(label = "Range for {param}"),
  positional_arg = ptr_arg_numeric_vector()
)

library(ggplot2)

## 3. A realistic ggplot2 graphic ------------------------------------------
# mpg dataset (ships with ggplot2)
{
  ggplot(mpg, aes(ppVar(displ), ppVar(hwy), color = ppVar(class))) +
    geom_point(alpha = 0.6, size = 2) +
    geom_smooth(method = ppText("loess"), se = FALSE, span = 0.75) +
    ppLayerOff(facet_wrap(~ ppVar(drv))) +
    scale_color_brewer(palette = "Set2") +
    labs(title = "Highway MPG vs engine displacement",
         x = ppText("Engine displacement (L)"), y = ppText("Highway MPG")) +
    coord_cartesian(xlim = ppExpr(c(1, 7)), ylim = ppRange(c(10, 45)))
} |>
  ptr_app()

## 4.1 Filter, group, and plot --------------------------------------------
library(dplyr)
ptr_app(
  mpg |>
    dplyr::filter(ppVar(displ) > ppNum(1.5)) |>
    dplyr::group_by(ppVar(class)) |>
    dplyr::filter(ppExpr(dplyr::n() > 5)) |>
    dplyr::ungroup() |>
    ggplot(aes(ppVar(displ), ppVar(hwy), color = ppVar(class))) +
    geom_point(alpha = 0.5)

)

## 4.2 PCA scores with 95% confidence ellipses ----------------------------
library(dplyr)
library(broom)

ptr_arg_string_vector <- function() {
  function(arg_expr) {
    val <- ggpaintr:::ptr_constant_fold(arg_expr)
    if (!is.character(val) || anyNA(val)) {
      rlang::abort("Default must be a character vector of column names.")
    }
    val
  }
}

ppVars <- ptr_define_placeholder_consumer(
  keyword = "ppVars",
  build_ui = function(node, cols, data, label = NULL, selected = NULL, ...) {
    retained <- intersect(selected %||% character(0), cols)
    shiny::selectInput(node$id, label = label %||% "Columns",
                       choices = cols, selected = retained,
                       multiple = TRUE)
  },
  resolve_expr = function(value, node, ...) {
    if (length(value) == 0L) return(NULL)
    rlang::call2("c", !!!as.list(value))   # c(col1, col2, ...) as bare symbols
  },
  validate_session_input = function(value, ctx) {
    bad <- setdiff(value, ctx$upstream_cols)
    if (length(bad) == 0L) TRUE
    else paste0("Not in upstream data: ", paste(bad, collapse = ", "))
  },
  ui_text_defaults = list(label = "Columns for {param}"),
  parse_positional_arg = ptr_arg_string(vector = TRUE)
)

do_pca <- function(d, cols) {
  broom::augment(prcomp(d[, cols], scale. = TRUE), d)
}

{ppUpload(iris) |>
    ppVerbSwitch(drop_na(), FALSE) |>
  do_pca(ppVars(c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"))) |>
  ggplot(aes(ppVar(.fittedPC1), ppVar(.fittedPC2), color = ppVar(Species))) +
    stat_ellipse(level = 0.95, linewidth = 0.8) +
    geom_point(alpha = 0.8, size = 2) +
    scale_color_brewer(palette = "Dark2") +
    labs(title = "Iris in PC space, with 95% ellipses",
         x = "PC1", y = "PC2") +
    theme_minimal() +
    theme(legend.position = "bottom")} |>
  ptr_app()

## 4.3 K-means elbow plot -------------------------------------------------
library(dplyr)
library(tidyr)
library(purrr)
library(broom)

tibble(k = 1:9) |>
  mutate(
    kmeans  = map(k, function(.k) kmeans(
      scale(iris[, c("Sepal.Length", "Sepal.Width",
                     "Petal.Length", "Petal.Width")]),
      centers = .k, nstart = 25)),
    glanced = map(kmeans, broom::glance)
  ) |>
  unnest(glanced) |>
  ggplot(aes(k, tot.withinss)) +
    geom_line(color = "gray50") +
    geom_point(size = 3, color = "#0072B2") +
    scale_x_continuous(breaks = 1:9) +
    labs(title = "Elbow plot for k-means on iris",
         x = "Number of clusters", y = "Total within-cluster SS") +
    theme_minimal()

## 4.4 Regression diagnostics ---------------------------------------------
library(broom)

{broom::augment(lm(mpg ~ wt + cyl + hp, data = ppUpload(mtcars))) |>
  ggplot(aes(.fitted, .resid)) +
    geom_hline(yintercept = 0, color = "gray60", linetype = 2) +
    geom_point(alpha = 0.7, size = 2.5, color = "#D55E00") +
    geom_smooth(method = ppText("loess"), se = FALSE, span = 0.75) +
    labs(title = "Residuals vs fitted (mpg ~ wt + cyl + hp)",
         x = "Fitted", y = "Residual") +
    theme_minimal()} |>
  ptr_app()


{broom::augment(lm(ppExpr(Sepal.Length ~ Sepal.Width + Petal.Length), data = ppUpload(iris))) |>
  ggplot(aes(.fitted, .resid)) +
  geom_hline(yintercept = ppExpr(0), color = "gray60", linetype = 2) +
  geom_point(alpha = 0.7, size = 2.5, color = "#D55E00") +
  geom_smooth(method = ppText("loess"), se = FALSE, span = 0.75) +
  labs(title = "Residuals vs fitted (mpg ~ wt + cyl + hp)",
       x = "Fitted", y = "Residual") +
  theme_minimal()} |>
  ptr_app()

## 4.5 Rolling time-series ------------------------------------------------
library(dplyr)

roll_mean <- function(x, n) {
  as.numeric(stats::filter(x, rep(1 / n, n), sides = 1))
}

{economics |>
  mutate(roll = roll_mean(ppVar(unemploy), ppNum(12))) |>
  ggplot(aes(date, ppVar(unemploy))) +
    geom_line(color = "gray70") +
    geom_line(aes(y = roll), color = "#0072B2", linewidth = 1) +
    labs(title = "US unemployment with 12-month rolling mean",
         x = NULL, y = "Persons (thousands)") +
    theme_minimal()} |>
  ptr_app()

## 4.6 Group-wise regression coefficients ---------------------------------
library(palmerpenguins)
library(dplyr)
library(tidyr)
library(purrr)
library(broom)

penguins |>
  tidyr::drop_na(bill_depth_mm, bill_length_mm) |>
  group_by(species) |>
  tidyr::nest() |>
  mutate(
    fit  = map(data, function(.d) lm(bill_depth_mm ~ bill_length_mm, data = .d)),
    tidy = map(fit,  function(.f) broom::tidy(.f, conf.int = TRUE))
  ) |>
  unnest(tidy) |>
  filter(term == "bill_length_mm") |>
  ggplot(aes(estimate, species, color = species)) +
    geom_vline(xintercept = 0, color = "gray60", linetype = 2) +
    geom_pointrange(aes(xmin = conf.low, xmax = conf.high), size = 0.8) +
    scale_color_brewer(palette = "Dark2") +
    labs(title = "Slope of bill depth ~ bill length, by species",
         x = "Estimate (95% CI)", y = NULL) +
    theme_minimal() +
    theme(legend.position = "none")

## 5.1 plotly tooltips ----------------------------------------------------
library(plotly)

p <- ggplot(mpg, aes(displ, hwy, color = class,
                     text = paste(manufacturer, model, sep = " "))) +
       geom_point(size = 3, alpha = 0.7) +
       coord_cartesian(xlim = c(1, 7), ylim = c(10, 45))

plotly::ggplotly(p, tooltip = "text")

## 5.2 ggiraph tooltips and click handling --------------------------------
library(ggiraph)

ggplot(mpg, aes(displ, hwy)) +
  geom_point_interactive(aes(tooltip = model), size = 3, color = "#e63946")

## 6.1 ggpcp — parallel coordinates ---------------------------------------
library(ggpcp)
data(flea, package = "GGally")

{ppUpload(flea) |>
  pcp_select(ppVars(c("species", "tars1", "tars2", "head", "aede1", "aede2", "aede3"))) |>
  pcp_scale(method = "uniminmax") |>
  pcp_arrange() |>
  ggplot(aes_pcp()) +
    geom_pcp_axes() +
    geom_pcp(aes(colour = ppVar(species)))} |>
  ptr_app()

ptr_arg_bare_names <- function() {
  function(arg_expr) {
    items <- if (rlang::is_call(arg_expr, "c")) as.list(arg_expr[-1L]) else list(arg_expr)
    vapply(items, function(e) {
      if (rlang::is_symbol(e))      rlang::as_string(e)   # bare: species
      else if (rlang::is_string(e)) e                     # string: "species"
      else rlang::abort("ppVars() default must be bare column names or strings.")
    }, character(1))
  }
}

ppVars2 <- ptr_define_placeholder_consumer(
  keyword = "ppVars2",
  build_ui = function(node, cols, data, label = NULL, selected = NULL, ...) {
    retained <- intersect(selected %||% character(0), cols)
    shiny::selectInput(node$id, label = label %||% "Columns",
                       choices = cols, selected = retained, multiple = TRUE)
  },
  # draw-time: turn the SELECTED column names back into c(species, tars1)
  resolve_expr = function(value, node, ...) {
    if (length(value) == 0L) return(NULL)
    rlang::call2("c", !!!rlang::syms(value))
  },
  positional_arg = ptr_arg_bare_names(),
  ui_text_defaults = list(label = "Columns for {param}"),
  runtime = function(x, ...) {
    expr <- rlang::enexpr(x)
    if (rlang::is_call(expr, "c"))    vapply(as.list(expr[-1L]), rlang::as_string, character(1))
    else if (rlang::is_symbol(expr))  rlang::as_string(expr)
    else if (rlang::is_string(expr))  expr
    else eval(expr, parent.frame())            # fallback for other selection exprs
  }
)

ppUpload(flea) |>
    pcp_select(ppVars2(c(species, tars1, tars2, head, aede1, aede2, aede3))) |>
    pcp_scale(method = "uniminmax") |>
    pcp_arrange() |>
    ggplot(aes_pcp()) +
    geom_pcp_axes() +
    geom_pcp(aes(colour = ppVar(species)))

## 6.2 ggridges — ridge densities -----------------------------------------
library(ggridges)

ggplot(lincoln_weather,
       aes(x = `Mean Temperature [F]`, y = Month, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01,
                               bandwidth = 3) +
  scale_fill_viridis_c(option = "C")

## 6.3 ggrepel — labelled scatter -----------------------------------------
library(ggrepel)

mt <- data.frame(mtcars, model = rownames(mtcars))

ggplot(mt, aes(wt, mpg, label = model)) +
  geom_point(size = 3, color = "steelblue") +
  geom_text_repel(size = 3.5, max.overlaps = 12, box.padding = 0.3)

## 6.4 ggalluvial — flow diagrams -----------------------------------------
library(ggalluvial)

ggplot(vaccinations,
       aes(x = survey, stratum = response, alluvium = subject,
           y = freq, fill = response)) +
  geom_flow(alpha = 0.7) +
  geom_stratum() +
  scale_x_discrete(expand = c(0.1, 0.1))

## 6.5 ggdist — distribution visualizations -------------------------------
library(ggdist)

ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
  stat_halfeye(adjust = 0.5, justification = -0.2,
               .width = 0, point_colour = NA, slab_alpha = 0.6) +
  geom_boxplot(width = 0.15, outlier.shape = NA) +
  coord_flip()


colvars <- ptr_define_placeholder_consumer(
  keyword = "colvars",

  build_ui = function(node, cols = character(), data = NULL,
                      label = NULL, selected = character(0), ...) {
    shiny::selectInput(
      node$id, label = label %||% "Columns",
      choices  = cols,
      selected = intersect(selected, cols),  # keep only still-valid picks
      multiple = TRUE
    )
  },

  resolve_expr = function(value, node, ...) {
    if (length(value) == 0L) return(NULL)
    rlang::call2("c", !!!as.list(value))   # c(col1, col2, ...)
  },

  positional_arg   = ptr_arg_symbol_or_string(),
  ui_text_defaults = list(label = "Columns for {param}")
  # validate_input / named_args / runtime: same shape as 2.1, omitted here.
)

mtcars |>
  dplyr::select(colvars(mpg, hp)) |>
  ggplot(aes(x = ppVar(mpg), y = ppVar(hp))) + geom_point()

ppPercent <- ptr_define_placeholder_value(
  keyword = "ppPercent",

  build_ui = function(node, label = NULL, selected = NULL,
                      named_args = list(), ...) {
    step <- named_args$step %||% 1
    shiny::sliderInput(
      node$id, label = label %||% "Percent",
      min = 0, max = 100,
      value = selected %||% node$default %||% 50,
      step = step
    )
  },

  resolve_expr = function(value, node, ...) {
    if (is.null(value)) return(NULL)
    as.numeric(value) / 100
  },

  validate_session_input = function(value, ctx) {
    v <- suppressWarnings(as.numeric(value))
    if (length(v) != 1L || is.na(v) || v < 0 || v > 100) {
      rlang::abort("Percent must be a single number between 0 and 100.")
    }
    TRUE
  },

  parse_positional_arg = ptr_arg_numeric(),
  parse_named_args = list(step = ptr_arg_numeric()),
  embellish_eval   = function(x, ...) as.numeric(x) / 100,
  ui_text_defaults = list(label = "Percent for {param}")
)
ptr_app(
  ggplot(mtcars, aes(x = ppVar("wt"), y = ppVar("mpg"))) +
    geom_point(alpha = ppPercent(40, step = 5))
)

