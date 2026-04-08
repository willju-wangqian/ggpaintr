# Current Status

## Snapshot

- `ggpaintr` is maintained as a single package surface. Archived legacy package
  content remains under `archive/legacy-package/`.
- Durable architecture, supported boundaries, and export design live in
  `dev/knowledge/project-overview.md`. Use
  `dev/knowledge/index.md` for the normal note-routing entry point.

## Latest verification

- `Rscript -e 'devtools::document()'` completed cleanly in the latest recorded
  session run and synchronized `NAMESPACE` plus the generated `man/` files with
  the current roxygen comments.
- `Rscript -e 'testthat::test_dir("tests/testthat")'` passes with 388 tests in
  the latest recorded session run.
- `Rscript -e 'devtools::check(document = FALSE, manual = FALSE, args = c("--as-cran", "--no-manual"))'`
  completed cleanly with 0 errors, 0 warnings, and 0 notes in the latest
  recorded session run.
- `Rscript -e 'pkgdown::build_site_github_pages(new_process = FALSE, install = TRUE)'`
  completed cleanly in the latest recorded session run.
- `Rscript -e 'devtools::load_all("."); rmarkdown::render("README.Rmd", envir = globalenv())'`
  completed cleanly in the latest recorded session run and regenerated
  `README.md`.
- `cran-comments.md` now matches the latest clean
  `--as-cran --no-manual` result.

## Completed recently

- implemented phase-2 extensibility through the per-app placeholder registry,
  including `ggpaintr_placeholder()`,
  `ggpaintr_effective_placeholders()`, and `ggpaintr_missing_expr()`
- refactored parsing, UI dispatch, runtime completion, upload eval-env
  preparation, and copy-rule validation so built-in and custom placeholders
  share the same registry path
- removed the unused legacy placeholder replacement helpers from
  `R/paintr-runtime.R`, dropped the orphaned `detect_keywords()` helper, and
  regenerated `man/` and `docs/` so the dead internal help pages are gone
- extended export generation so custom-placeholder apps serialize only compact
  `custom_placeholders` definitions and rebuild the effective registry in the
  generated app
- added `ggpaintr_normalize_column_names()` plus internal tabular-data
  normalization helpers
- tightened plot assembly so `paintr_get_plot()` handles single-expression
  plots and empty-layer input more deliberately
- synchronized the repo cleanup pass across roxygen comments, generated docs,
  pkgdown output, and README artifacts
- expanded README, pkgdown, manual docs, and vignettes to cover custom
  placeholders and the newer Shiny integration workflows
- added `ggpaintr_runtime_input_spec()` as the supported low-level runtime
  input-discovery helper

## Current focus

- decide whether the tracked historical `preconsideration/` tree should stay as
  explicit history or move into a clearer archive path
- decide whether phase 3 should focus first on module-layer composition,
  deeper placeholder exportability guarantees, or developer ergonomics for
  common custom-placeholder patterns
- keep the verified source, generated docs, README, and maintained notes in
  sync as the next feature work lands

## Current risks or blockers

- exported custom placeholders must currently define hook functions inline
  inside `ggpaintr_placeholder()` calls to remain exportable as standalone apps.
  Reference: `R/paintr-placeholders.R:457-485`
- the repo still contains tracked historical exploratory material under
  `preconsideration/`; it is excluded from package/build paths, but it remains
  repo-noise until a separate archival decision is made.
  Reference: `.gitignore:44-46`, `.Rbuildignore:5-7`

## Quick re-entry points

Default startup path:

1. `dev/knowledge/index.md`
2. `dev/knowledge/current-status.md`
3. task-relevant source files, tests, and docs only
