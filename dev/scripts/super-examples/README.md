# Playable super-app examples

These are copies of the five super-app pressure fixtures from `tests/testthat/fixtures/vignette-apps/super-*/`. The only difference is the boot scaffolding: the original first line `pkgload::load_all(Sys.getenv("GGP_PKG"), ...)` (used by shinytest2 to load the in-development package into the child R process) is replaced by a one-line comment. Interactive play assumes ggpaintr is already loaded.

The fixtures themselves stay in `tests/testthat/fixtures/vignette-apps/super-*/` so the gate keeps testing them. Edits here are for your own play — they do not affect tests.

## Run

From the repo root:

```r
devtools::load_all(".")
shiny::runApp("dev/scripts/super-examples/super-1-kitchen-sink")
# super-2a / super-2b / super-3 / super-4 work the same way:
shiny::runApp("dev/scripts/super-examples/super-2a-upload-registry")
```

To run the no-default variant of any super:

```r
shiny::runApp(shiny::shinyAppFile(
  "dev/scripts/super-examples/super-1-kitchen-sink/app-no-default.R"))
```

`super-3` is L3 multi-cell (`ptr_ui` + `ptr_server`, not `ptr_app_grid`). It needs `plotly` installed.

## What's in each directory

- `app.R` — the with-positional-defaults variant (canonical super formula).
- `app-no-default.R` — same surface, every `pp*(default)` stripped to `pp*()` (ADR-0016 parallel coverage).
- `reference.R` — Path-B reference: the formula evaluated as plain ggplot, without `ptr_app()`. Useful for sanity-checking what the apps "should" render.
- `sample_*.csv` (super-2a, super-2b) — fixture CSVs the apps' `ppUpload` widgets read.
- `user.css` (super-4) — the user-supplied stylesheet that styles the `ppColor` wrapper.

## Eyeball + `spec=` round-trip checklist

See `dev/notes/2026-05-24-super-eyeball-and-spec-roundtrip-checklist.html` for the per-super eyeball checks and the six-step copy-paste-back recipe for testing the `ptr_spec` round-trip — neither is covered by the automated gate.

## Coverage one-liner

Built-ins covered across the five: ppText, ppNum, ppExpr, ppVar, ppUpload (all five). Custom keywords exercised: ppRange (super-1, super-3), ppMultiVar / ppPower (super-2a), ppCoef / ppFactor / ppSample (super-2b), ppColor (super-4).
