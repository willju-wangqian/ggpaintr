# ggpaintr 0.1.0

- repositioned the package around the maintained `ggpaintr` workflow
- archived the legacy package implementation under `archive/legacy-package/`
- introduced a focused public API: `ggpaintr_app()`, `paintr_formula()`,
  `paintr_build_runtime()`, `paintr_get_plot()`, and `generate_shiny()`
- replaced legacy package docs, vignette, and pkgdown content with
  `ggpaintr`-first documentation
- added roxygen2-based package documentation for the active implementation
- prepared the package structure for `R CMD check` and CRAN-oriented cleanup
