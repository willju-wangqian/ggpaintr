# Changelog

## ggpaintr 0.9.0

### New features

- [`ptr_gg_extra()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_gg_extra.md)
  — advanced helper for embedded apps that own their own
  `renderPlot({...})` block. Captures ggplot components (themes, scales,
  coords, …) added on top of
  [`ptr_extract_plot()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_extract_plot.md)
  and stores them on a new `ptr_state$extras` reactiveVal, so the
  default code binder
  ([`ptr_register_code()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_register_code.md))
  appends them to the generated-code pane. Extras are suppressed
  automatically when the underlying runtime reports a failure, so stale
  extras from a prior successful draw never surface during an error
  state. See
  [`vignette("ggpaintr-extensibility")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-extensibility.md),
  Recipe 3.

### Breaking changes

- Removed the Shiny app export feature: `ptr_generate_shiny()`,
  `ptr_register_export()`, the download button, and `ids$export_button`
  are gone. Use the public API
  ([`ptr_server_state()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server_state.md),
  `ptr_register_*()`,
  [`ptr_input_ui()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_input_ui.md),
  [`ptr_output_ui()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_output_ui.md),
  [`ptr_app_bslib()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app_bslib.md))
  to compose and distribute custom apps.
- The placeholder distribution parameters `source_file`,
  `source_package`, `source_function`, and `on_missing` are removed from
  [`ptr_define_placeholder()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder.md).
  They only existed to support the exporter. Hook functions should be
  defined inline.
- The last commit before these removals is tagged
  `v0-pre-export-removal`.

## ggpaintr 0.1.0

- repositioned the package around the maintained `ggpaintr` workflow
- shortened API prefix: all exported functions now use the `ptr_*`
  prefix for conciseness (previously `ggpaintr_*`)
- improved semantic clarity of function names across the public API
  (e.g., `bind_*` → `register_*`, `*_value` → `extract_*`)
- renamed `copy_rules` parameter/system to `ui_text` throughout
- migrated error signaling from
  [`base::stop()`](https://rdrr.io/r/base/stop.html) to
  [`rlang::abort()`](https://rlang.r-lib.org/reference/abort.html)
- added `@examples` to all exported functions
- clarified the maintained public API boundary in the README and
  pkgdown-facing docs so the beginner path stays centered on the
  wrapper, integration, export, placeholder, and intentionally exported
  runtime helpers
- removed generated documentation topics for package-internal helper
  functions so internal implementation details are no longer presented
  as part of the public community-facing surface
- updated `ptr_generate_shiny()` so the maintained public call path is
  now `ptr_generate_shiny(ptr_obj, output_file, ...)`
- archived the legacy package implementation under
  `archive/legacy-package/`
- replaced legacy package docs, vignette, and pkgdown content with
  `ggpaintr`-first documentation
- added roxygen2-based package documentation for the active
  implementation
- prepared the package structure for `R CMD check` and CRAN-oriented
  cleanup
