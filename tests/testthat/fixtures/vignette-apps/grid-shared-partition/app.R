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
#
# shared_ui removal (2026-05-28): `shared_ui` is no longer supported (see
# ?ptr_shared). `sz`'s 1-6 slider look is now carried by a custom value
# placeholder `ppSize` whose `build_ui` is the slider -- used via
# `ppSize(shared = 'sz')`, replacing the removed `shared_ui` override.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)

# Custom value placeholder: a 1-6 slider (the look the removed `shared_ui`
# override used to supply). Process-local registration.
ppSize <- ptr_define_placeholder_value(
  keyword      = "ppSize",
  default_arg  = ptr_arg_numeric(),
  build_ui     = function(node, label = NULL, selected = NULL, ...) {
    val <- if (is.null(selected) || length(selected) == 0L) 3 else
      suppressWarnings(as.numeric(selected[[1L]]))
    if (length(val) != 1L || is.na(val)) val <- 3
    sliderInput(node$id, label = label %||% "Size", min = 1, max = 6, value = val)
  },
  resolve_expr = function(value, node, ...) {
    out <- suppressWarnings(as.numeric(value))
    if (length(out) != 1L || is.na(out)) return(NULL)
    out
  }
)

ptr_app_grid(
  plots = list(
    "ggplot(iris, aes(x = ppVar(shared = 'ax1'), y = ppVar(shared = 'ax1'),
                      color = Species)) + geom_point(size = ppSize(shared = 'sz'))",
    "ggplot(iris, aes(x = ppVar(shared = 'ax2'), y = Sepal.Width,
                      color = Species)) + geom_point(size = ppSize(shared = 'sz'))"
  )
)
