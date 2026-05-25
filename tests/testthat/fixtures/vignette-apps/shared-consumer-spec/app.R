pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
# EXCEPT: this fixture is NOT a vignette mirror — it is the regression
# fixture for the shared-consumer spec-seed bug. Before the fix,
# `ptr_bind_shared_consumer_uis()` seeded its picker's `selected` from
# `input[[ns(rep_node$id)]]` only and ignored `state$spec_seed[[rep_node$id]]`,
# so a `spec = list(shared_<key> = ...)` entry for a `ppVar(shared = ...)`
# placeholder wrote into `state$spec_seed` (via `apply_spec_at_boot`) but
# never reached the rendered picker — the widget booted blank.
#
# The companion shared `ppNum(shared = "lw")` entry (`shared_lw = 2`)
# exercises the shared-value path (`ptr_setup_value_uis()`'s shared loop)
# which already honored the seed, so we expect that widget to boot at 2.
ptr_app(
  ggplot(
    mtcars,
    aes(x = ppVar(), y = ppVar(), color = ppVar(shared = "grp"))
  ) +
    geom_point(size = ppNum()) +
    geom_smooth(linewidth = ppNum(shared = "lw")) +
    geom_line(linewidth = ppNum(shared = "lw")) +
    facet_wrap(vars(ppVar(shared = "grp"))),
  spec = list(
    `ggplot_1_1_ppVar_NA` = "mpg",
    `ggplot_1_2_ppVar_NA` = "hp",
    `shared_grp`          = "cyl",
    `geom_point_1_ppNum_NA` = 2L,
    `shared_lw`           = 2L
  )
)
