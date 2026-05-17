# vignette-apps fixtures

One subdir per covered vignette example: `<slug>/app.R`. Each `app.R` body
(everything below the `# >>>` marker) is **verbatim-equivalent** to the named
vignette chunk — keep it diffable against the `.Rmd`. The lines above the
marker are boot scaffolding only: `pkgload::load_all()` so the shinytest2
child process runs **dev** source, not the stale system-installed ggpaintr.

Booted by `tests/testthat/test-e2e-vignette-examples-shinytest2.R` via
`tests/testthat/helper-vignette-apps.R`.

Before adding or changing a fixture, read the **"Browser e2e (shinytest2) —
hard-won gotchas"** section in `.claude/rules/testing.md`. The non-obvious
ones: app-dir + `pkgload::load_all` (never a serialized appobj), no
`get_values()`, `set_inputs(wait_ = FALSE)`, and the source/consumer
`*_subtab = "Controls"` step to bind suspended `var` pickers.
