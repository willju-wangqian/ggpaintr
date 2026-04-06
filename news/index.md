# Changelog

## ggpaintr 0.1.0

- repositioned the package around the maintained `ggpaintr` workflow
- archived the legacy package implementation under
  `archive/legacy-package/`
- introduced a focused public API:
  [`ggpaintr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ggpaintr_app.md),
  [`paintr_formula()`](https://willju-wangqian.github.io/ggpaintr/reference/paintr_formula.md),
  [`paintr_build_runtime()`](https://willju-wangqian.github.io/ggpaintr/reference/paintr_build_runtime.md),
  [`paintr_get_plot()`](https://willju-wangqian.github.io/ggpaintr/reference/paintr_get_plot.md),
  and
  [`generate_shiny()`](https://willju-wangqian.github.io/ggpaintr/reference/generate_shiny.md)
- replaced legacy package docs, vignette, and pkgdown content with
  `ggpaintr`-first documentation
- added roxygen2-based package documentation for the active
  implementation
- prepared the package structure for `R CMD check` and CRAN-oriented
  cleanup
