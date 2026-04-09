# ggpaintr 0.1.0

- clarified the maintained public API boundary in the README and pkgdown-facing
  docs so the beginner path stays centered on the wrapper, integration, export,
  placeholder, and intentionally exported runtime helpers
- removed generated documentation topics for package-internal helper functions
  so internal implementation details are no longer presented as part of the
  public community-facing surface
- updated `generate_shiny()` so the maintained public call path is now
  `generate_shiny(ggpaintr_obj, output_file, ...)`, while deprecated legacy
  `var_ui` calls still work with a warning
- repositioned the package around the maintained `ggpaintr` workflow
- archived the legacy package implementation under `archive/legacy-package/`
- introduced a focused public API: `ggpaintr_app()`, `ggpaintr_formula()`,
  `ggpaintr_build_runtime()`, `ggpaintr_get_plot()`, and `generate_shiny()`
- replaced legacy package docs, vignette, and pkgdown content with
  `ggpaintr`-first documentation
- added roxygen2-based package documentation for the active implementation
- prepared the package structure for `R CMD check` and CRAN-oriented cleanup
