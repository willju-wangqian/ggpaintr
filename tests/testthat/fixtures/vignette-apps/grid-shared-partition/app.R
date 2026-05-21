# Boot scaffolding: load the in-development package into this child app
# process so the e2e test exercises dev source, not a stale system install.
#
# NOT a verbatim vignette chunk -- this is a constructed W1 (#B1/#B1b)
# regression fixture: `ptr_app_grid()` has the SAME host shape as the embed
# path (`ptr_shared_server()` + per-plot `ptr_server()`), so the
# binder-less formula-local shared-consumer bug must be exercised on it too.
# `ax1`/`ax2` are formula-local `ppVar(shared=)` consumer keys (one consuming
# formula each -> bound by each `ptr_server()` under its namespace);
# `sz` is a cross-formula panel value key (both formulas -> standalone panel).
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)

ptr_app_grid(
  plots = list(
    "ggplot(iris, aes(x = ppVar(shared = 'ax1'), y = ppVar(shared = 'ax1'),
                      color = Species)) + geom_point(size = ppNum(shared = 'sz'))",
    "ggplot(iris, aes(x = ppVar(shared = 'ax2'), y = Sepal.Width,
                      color = Species)) + geom_point(size = ppNum(shared = 'sz'))"
  ),
  shared_ui = list(sz = function(id) sliderInput(id, "Size", 1, 6, 3))
)
