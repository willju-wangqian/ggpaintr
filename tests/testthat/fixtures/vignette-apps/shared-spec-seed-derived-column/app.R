# Boot scaffolding (NOT vignette code): load the in-development package so the
# child app process exercises dev source. Behavior-PIN fixture (2026-05-30), NOT
# a bug repro: a SHARED consumer (`var(shared = "grp")`, used by both `color` and
# `facet`) over a column DERIVED by a placeholder-carrying stage
# (`mutate(adj = ppExpr(...))`). By design the shared resolver truncates the
# upstream at the first placeholder-carrying stage, so the shared picker is fed
# `names(mtcars)` and never offers `adj` (R/paintr-shared.R:65-127). The test
# pins that documented limitation. No vignette counterpart.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
library(rlang)

formula <- expr(ggplot(
  data = mtcars |>
    dplyr::filter(ppExpr(hp >= 75)) |>
    dplyr::mutate(adj = ppExpr(mpg / wt)),
  aes(x = ppVar(mpg), color = factor(ppVar(adj, shared = "grp")))
) + geom_point() + facet_wrap(vars(ppVar(adj, shared = "grp"))))

spec <- list(
  `ggplot_2_1_ppExpr_NA`   = "hp >= 75",
  `ggplot_3_1_ppExpr_NA`   = "mpg / wt",
  `ggplot_1_1_ppVar_NA`    = "mpg",
  `shared_grp`             = "adj",
  `ggplot_2_stage_enabled` = FALSE
)

ptr_app(!!formula, spec = spec)
