## Submission type

Resubmission (new submission, version unchanged at 0.11.1). This
resubmission addresses all three remarks from the CRAN review of the
first submission:

- Removed the single quotes around the file formats in the Description
  field (now: .csv, .tsv, .rds, .xlsx, .xls, and .json).
- Removed the examples from the help pages of unexported functions
  (`ptr_app_bslib`, `ptr_app_grid`, and the documentation-only `ptr_css`
  page). The placeholder help topic formerly named `pp_placeholders` (a
  topic name with no object behind it) is now named after the exported
  `ppVar`, with `pp_placeholders` kept as an alias, so every help page
  with examples documents an exported object.
- Replaced the commented-out example lines with executable code. Calls
  that launch a 'shiny' app are gated by `if (interactive())`; example
  lines using the suggested package 'dplyr' are gated by
  `requireNamespace("dplyr", quietly = TRUE)`. Every example, including
  the interactive app examples, was verified to run without error (the
  app examples in a headless browser via 'shinytest2').

## Test environments

- Local: macOS Sequoia 15.6.1 (Darwin 24.6.0, aarch64-apple-darwin20), R 4.5.3 — OK
- win-builder (R-devel and R-release) — OK
- R-hub:
  - linux (R-devel) — OK
  - windows (R-devel) — OK
  - macOS (R-devel) — not run; the R-hub macOS runner did not pick up the
    job (no available runner). macOS is covered instead by the macOS builder
    below.
- macOS builder (https://mac.r-project.org/macbuilder/): R 4.6.0 Patched
  (r89963), aarch64-apple-darwin23 (macOS Tahoe 26.5.1) — OK

## R CMD check results

On every environment above the check completed with:

- 0 errors
- 0 warnings
- 0 notes

On CRAN's incoming checks a single NOTE is expected: the standard
"New submission" notice for a first-time submission.

## Downstream dependencies

There are no reverse dependencies (this is a new package).
