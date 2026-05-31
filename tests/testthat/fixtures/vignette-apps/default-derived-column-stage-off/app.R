# Boot scaffolding (NOT vignette code): load the in-development package so the
# child app process exercises dev source. Pinned regression fixture (2026-05-30):
# a DERIVED-column FORMULA DEFAULT (no spec seed for the picker or its upstream
# producer) must still seed at boot when the spec disables a pipeline stage. The
# only spec entry is the stage toggle; x/y pickers and the ppExpr producers all
# ride on formula defaults. No vignette counterpart.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
library(rlang)

formula <- expr(ggplot(
  data = mtcars |>
    dplyr::filter(ppExpr(hp >= 75)) |>
    dplyr::mutate(adj = ppExpr(mpg / wt)),
  aes(x = ppVar(mpg), y = ppVar(adj))
) + geom_point())

# Only the stage toggle is specified; the derived-column y picker (and the
# ppExpr that creates `adj`) have NO spec seed -- they default from the formula.
spec <- list(
  `ggplot_2_stage_enabled` = FALSE
)

ptr_app(!!formula, spec = spec)
