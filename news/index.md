# Changelog

## ggpaintr (development version)

### New features

- **L3 — own every piece of the UI.** Every piece of ggpaintr’s public
  UI now has its own exported builder:
  [`ptr_ui_header()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_header.md),
  [`ptr_ui_controls()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_controls.md),
  [`ptr_ui_plot()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_plot.md),
  [`ptr_ui_error()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_error.md),
  [`ptr_ui_code()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_code.md),
  `ptr_ui_code_toggle()`, and
  [`ptr_ui_assets()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_assets.md).
  Compose exactly the pieces you want, place each anywhere in your own
  layout, and wire them with the existing server API
  ([`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)
  / `ptr_module_server()`). The bundled
  [`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
  / `ptr_module_ui()` / `ptr_outputs_ui()` / `ptr_controls_ui()` paths
  are reimplemented as thin composites of these pieces — same `<body>`
  DOM, no behavior or performance change. The pieces are deliberately
  *bare* (no assets, no `.ptr-app` wrapper); the new
  **[`ptr_ui_page()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_page.md)**
  shell wraps them in a Bootstrap page + the single `.ptr-app` theme
  scope + the (deduped) asset bundle in one call, so that is all an L3
  user has to remember. Swap the page builder with `page =` (`fluidPage`
  default, also `fixedPage`/`fillPage`/`bootstrapPage`/`basicPage`); for
  a `navbarPage` or bslib root use the documented decomposition recipe.
  See
  [`vignette("ggpaintr-use-cases")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-use-cases.md)
  § “L3 — Own every piece of the UI”.
- [`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)
  now accepts a
  [`ptr_shared_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_server.md)
  bundle directly via `shared_state =`, mirroring `ptr_module_server()`.
  Wiring a page-level `ptr_shared_ui()` panel to a single embedded or
  custom-rendered plot no longer requires spreading the four bundle
  slots through `...`.
- The custom-renderer pattern (reading `state$runtime()` for your own
  [`renderPlotly()`](https://rdrr.io/pkg/plotly/man/plotly-shiny.html) /
  `renderGirafe()`) is now documented as an L2 capability — both
  [`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)
  and `ptr_module_server()` return the `ptr_state`, so it needs nothing
  beyond embedding.

### Behavior changes

- **`ptr_register_plot()` / `ptr_register_error()` /
  `ptr_register_code()` are no longer exported.** Post-rewrite they only
  ever ran inside
  [`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)
  (their sole caller, via the internal runtime setup) and rendered
  nothing without it, so the “compose outputs manually” use case they
  advertised was impossible. They remain as internal helpers; no
  replacement is needed (use the L3 pieces +
  [`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md),
  or read `state$runtime()` for a custom renderer). No deprecation cycle
  — this dev cycle’s own surface, single pre-1.0 user.
- **ggpaintr’s CSS/JS now ship as deduped
  [`htmltools::htmlDependency()`](https://rstudio.github.io/htmltools/reference/htmlDependency.html)
  bundles.** `ggpaintr.css`, the code-window JS, and the `ptr_set_class`
  handler + stage CSS are emitted as two dependencies (`ggpaintr`,
  `ggpaintr-layer`) instead of inline `<style>`/`<script>`/`<link>`
  tags. htmltools collapses each to a single `<head>` injection no
  matter how many shells/pieces a page nests, so the old
  `window.__ptr_*_registered` JS guards are gone. Rendered `<body>` is
  unchanged; only `<head>` asset emission differs. `htmltools` moves
  from Suggests to Imports.
- **Custom placeholder registration is now process-global.** The legacy
  `placeholders = ptr_merge_placeholders(...)` argument has been removed
  from every public entry point
  ([`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md),
  [`ptr_app_bslib()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app_bslib.md),
  [`ptr_app_grid()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app_grid.md),
  `ptr_module_ui()`, `ptr_module_server()`,
  [`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md),
  `ptr_translate()`, `ptr_render()`). Custom placeholders are now
  defined once per R session via
  `ptr_define_placeholder_value() / _consumer() / _source()`, which
  write directly into an internal global registry. All instances in that
  session see the same registry.
  - The hook signatures changed too: `build_ui(node, label = NULL, ...)`
    for value/source,
    `build_ui(node, cols, label = NULL, selected = character(0), ...)`
    for consumer. The id is on `node$id` (and the source companion id on
    `node$companion_id`); there is no separate `id` argument.
    `resolve_expr(value, node, ...)` returning `NULL` is the new “drop
    this argument” signal (replacing `ptr_missing_expr()`).
  - Distribution: package authors should ship a setup function that
    calls `ptr_define_placeholder_*()` (caller-driven), or auto-register
    from `.onLoad` (drop-in). Either pattern works; pick by whether you
    want the registration to be visible at the call site.
  - Multi-instance limitation: two `ptr_module_server()` instances in
    the same Shiny app cannot have *different* implementations of the
    same keyword. The second registration overwrites the first (with a
    [`cli::cli_warn`](https://cli.r-lib.org/reference/cli_abort.html)
    notice). If you need divergent widgets per instance, prefix custom
    keywords with a package-specific tag (`mypkg_pct` rather than
    `pct`). A scoped per-instance registry override is on the table for
    a future release.
  - Test isolation: tests that register custom placeholders should
    restore state with `ptr_registry_clear()` +
    `ptr_register_builtins()` (currently internal) or run in a fresh R
    session.
- Extended the curated empty-call cleanup list with 14 more ggplot2
  names:
  [`annotation_custom()`](https://ggplot2.tidyverse.org/reference/annotation_custom.html),
  [`annotation_map()`](https://ggplot2.tidyverse.org/reference/annotation_map.html),
  [`annotation_raster()`](https://ggplot2.tidyverse.org/reference/annotation_raster.html)
  (their required args make zero-arg calls error in stock ggplot2 —
  dropping prevents the crash);
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html),
  [`aes_()`](https://ggplot2.tidyverse.org/reference/aes_.html),
  [`aes_q()`](https://ggplot2.tidyverse.org/reference/aes_.html),
  [`aes_string()`](https://ggplot2.tidyverse.org/reference/aes_.html),
  [`vars()`](https://ggplot2.tidyverse.org/reference/vars.html) (empty
  mapping helpers are render-identical to “absent” via `inherit.aes`);
  and
  [`element_text()`](https://ggplot2.tidyverse.org/reference/element.html),
  [`element_line()`](https://ggplot2.tidyverse.org/reference/element.html),
  [`element_rect()`](https://ggplot2.tidyverse.org/reference/element.html),
  [`element_point()`](https://ggplot2.tidyverse.org/reference/element.html),
  [`element_polygon()`](https://ggplot2.tidyverse.org/reference/element.html),
  [`element_geom()`](https://ggplot2.tidyverse.org/reference/element.html)
  (empty form inherits from the active theme element). This means
  e.g. `geom_point(aes(colour = var))` with `var` missing now reduces to
  `+ geom_point()` (clean) instead of `+ geom_point(aes())`.
  [`element_blank()`](https://ggplot2.tidyverse.org/reference/element.html)
  is intentionally excluded — empty form is a meaningful “suppress”
  directive, not a no-op.
- Replaced the old name-prefix heuristic for pruning empty calls. The
  new rule is: a zero-argument call is dropped iff its bare function
  name is in a curated ggplot2 cleanup list
  ([`theme()`](https://ggplot2.tidyverse.org/reference/theme.html),
  [`labs()`](https://ggplot2.tidyverse.org/reference/labs.html),
  `xlab`/`ylab`/`ggtitle`, `facet_wrap`/`facet_grid`/`facet_null`,
  `xlim`/`ylim`/`lims`, `expand_limits`, `guides`, `annotate`) or in the
  new `safe_to_remove` argument on
  [`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md),
  [`ptr_app_bslib()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app_bslib.md),
  `ptr_app_components()`,
  [`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md),
  `ptr_module_server()`, `ptr_server_state()`, `ptr_complete_expr()`,
  and `ptr_exec()`. Empty calls whose name is not in the set
  (e.g. third-party helpers like `pcp_theme()`, `pcp_arrange()`, or
  user-authored `aes_pcp()`) are preserved by default — being absent
  from the set is the “removal safety unknown” signal. Pass
  `safe_to_remove = c("pcp_theme")` to opt a specific name into the
  cleanup pass.
- An `expr` placeholder, when the user supplies an expression, always
  wins over the cleanup pass: whatever the user typed into an `expr`
  input is honoured verbatim, even if its top-level name is in
  `safe_to_remove`. The intent (“I want this here”) overrides the
  curated list.
- `geom_*()` and `stat_*()` layers are kept empty regardless of
  `safe_to_remove`, since they inherit aesthetics from
  [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html). So
  `geom_point(colour = var)` with `var` missing still renders as
  [`geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html).
- Behavior change vs. the prior implementation: user-authored literal
  `+ labs()`, `+ theme()`, `+ guides()` calls are now dropped (no
  semantic change — they’re no-ops in stock ggplot2). The previous build
  kept them via a “diff guard” that has been removed in favour of the
  cleaner curated-list rule.

## ggpaintr 0.9.1

### Documentation and exports

- `ptr_resolve_layer_data()` and `ptr_ns_id()` are now exported with
  full docs. Custom data-aware placeholders should call these helpers
  from a `bind_ui()` callback instead of replicating the source-symbol
  walk or the namespace-function dance.
- `?ptr_define_placeholder` now documents the `bind_ui` contract, the
  contents of the placeholder `context`, and the `meta$id` namespacing
  rule. A second runnable example demonstrates a data-aware
  “numeric-columns-only” placeholder using the public helpers.

## ggpaintr 0.9.0

### New features

- [`ptr_gg_extra()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_gg_extra.md)
  — advanced helper for embedded apps that own their own
  `renderPlot({...})` block. Captures ggplot components (themes, scales,
  coords, …) added on top of
  [`ptr_extract_plot()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_extract.md)
  and stores them on a new `ptr_state$extras` reactiveVal, so the
  default code binder (`ptr_register_code()`) appends them to the
  generated-code pane. Extras are suppressed automatically when the
  underlying runtime reports a failure, so stale extras from a prior
  successful draw never surface during an error state. See
  `vignette("ggpaintr-extensibility")`, Recipe 3.
- [`ptr_llm_primer()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_llm_primer.md),
  [`ptr_llm_topic()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_llm_topic.md),
  [`ptr_llm_topics()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_llm_topics.md),
  [`ptr_llm_register()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_llm_register.md)
  — ellmer-facing helpers that expose the bundled `inst/llm/` primer and
  topic files as plain strings or as registered ellmer tools, so coding
  assistants can fetch just-in-time ggpaintr guidance. See
  [`vignette("ggpaintr-llm")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-llm.md).
- `upload` placeholder now accepts `.tsv`, `.xlsx`, `.xls`, and `.json`
  in addition to `.csv` and `.rds`. JSON uploads must be an array of
  records; nested objects are flattened, nested arrays error out. Excel
  and JSON readers require the new suggested packages `readxl` and
  `jsonlite`. The “show me the code” pane reflects the
  format-appropriate reader (`read.delim`,
  [`readxl::read_excel`](https://readxl.tidyverse.org/reference/read_excel.html),
  [`jsonlite::fromJSON`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html)).

### Breaking changes

- Removed the Shiny app export feature: `ptr_generate_shiny()`,
  `ptr_register_export()`, the download button, and `ids$export_button`
  are gone. Use the public Shiny integration surface
  (`ptr_server_state()`, `ptr_register_*()`, `ptr_input_ui()`,
  `ptr_output_ui()`,
  [`ptr_app_bslib()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app_bslib.md))
  to compose and distribute custom apps.
- The placeholder distribution parameters `source_file`,
  `source_package`, `source_function`, and `on_missing` are removed from
  `ptr_define_placeholder()`. They only existed to support the exporter.
  Hook functions should be defined inline.
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
  now `ptr_generate_shiny(ptr_obj, output_file, ...)` *Note:
  `ptr_generate_shiny()` was removed in 0.9.0 — see breaking changes
  above.*
- archived the legacy package implementation under
  `archive/legacy-package/`
- replaced legacy package docs, vignette, and pkgdown content with
  `ggpaintr`-first documentation
- added roxygen2-based package documentation for the active
  implementation
- prepared the package structure for `R CMD check` and CRAN-oriented
  cleanup
