# Boot scaffolding (NOT vignette code): load the in-development package so the
# child app process exercises dev source. Pinned regression fixture for the
# derived-column spec-seed bug WHEN a pipeline stage is also toggled off at boot
# (2026-05-30) -- the stage toggle fires an extra consumer redraw in the window
# between the seed being emitted and the browser committing it, which used to
# wipe the seeded selection. No vignette counterpart.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
library(rlang)

# `adj` is a derived column (from `mutate(adj = ppExpr(...))`), so it is absent
# from the y picker's column list on the first render until the upstream ppExpr
# commits. The spec disables the FILTER stage (`ggplot_2_stage_enabled = FALSE`),
# which triggers a second consumer redraw at boot -- the race that erased the
# seeded `adj`.
formula <- expr(ggplot(
  data = mtcars |>
    dplyr::filter(ppExpr(hp >= 75)) |>
    dplyr::mutate(adj = ppExpr(mpg / wt)),
  aes(x = ppVar(mpg), y = ppVar(adj))
) + geom_point())

spec <- list(
  `ggplot_2_1_ppExpr_NA`   = "hp >= 75",
  `ggplot_3_1_ppExpr_NA`   = "mpg / wt",
  `ggplot_1_1_ppVar_NA`    = "mpg",
  `ggplot_1_2_ppVar_NA`    = "adj",
  `ggplot_2_stage_enabled` = FALSE
)

ptr_app(!!formula, spec = spec)
