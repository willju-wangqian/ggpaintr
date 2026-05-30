# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. Pinned regression fixture for the derived-column spec-seed
# bug (2026-05-29 #2) -- there is no vignette counterpart.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
library(rlang)

# A consumer (`aes(y = ppVar(adj))`) seeded over a DERIVED column: `adj` is
# created by an upstream `mutate(adj = ppExpr(...))`, so it is absent from the
# picker's `cols` on the first render (the ppExpr input has not bound yet) and
# only appears once the ppExpr echoes. The spec seeds `adj` for the y picker;
# before the fix the seed was dropped on the empty first render and the latch
# flipped, so the picker booted EMPTY instead of selecting `adj`.
formula <- expr(ggplot(
  data = mtcars |>
    dplyr::mutate(adj = ppExpr(mpg / wt)),
  aes(x = ppVar(mpg), y = ppVar(adj))
) + geom_point())

spec <- list(
  `ggplot_2_1_ppExpr_NA` = "mpg / wt",
  `ggplot_1_1_ppVar_NA`  = "mpg",
  `ggplot_1_2_ppVar_NA`  = "adj"
)

ptr_app(!!formula, spec = spec)
