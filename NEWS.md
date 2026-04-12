# ggpaintr 0.9.0

## Breaking changes

- Removed the Shiny app export feature: `ptr_generate_shiny()`,
  `ptr_register_export()`, the download button, and `ids$export_button` are
  gone. Use the public API (`ptr_server_state()`, `ptr_register_*()`,
  `ptr_input_ui()`, `ptr_output_ui()`, `ptr_app_bslib()`) to compose and
  distribute custom apps.
- The placeholder distribution parameters `source_file`, `source_package`,
  `source_function`, and `on_missing` are removed from
  `ptr_define_placeholder()`. They only existed to support the exporter. Hook
  functions should be defined inline.
- The last commit before these removals is tagged `v0-pre-export-removal`.

# ggpaintr 0.1.0

- repositioned the package around the maintained `ggpaintr` workflow
- shortened API prefix: all exported functions now use the `ptr_*` prefix for
  conciseness (previously `ggpaintr_*`)
- improved semantic clarity of function names across the public API
  (e.g., `bind_*` → `register_*`, `*_value` → `extract_*`)
- renamed `copy_rules` parameter/system to `ui_text` throughout
- migrated error signaling from `base::stop()` to `rlang::abort()`
- added `@examples` to all exported functions
- clarified the maintained public API boundary in the README and pkgdown-facing
  docs so the beginner path stays centered on the wrapper, integration, export,
  placeholder, and intentionally exported runtime helpers
- removed generated documentation topics for package-internal helper functions
  so internal implementation details are no longer presented as part of the
  public community-facing surface
- updated `ptr_generate_shiny()` so the maintained public call path is now
  `ptr_generate_shiny(ptr_obj, output_file, ...)`
- archived the legacy package implementation under `archive/legacy-package/`
- replaced legacy package docs, vignette, and pkgdown content with
  `ggpaintr`-first documentation
- added roxygen2-based package documentation for the active implementation
- prepared the package structure for `R CMD check` and CRAN-oriented cleanup
