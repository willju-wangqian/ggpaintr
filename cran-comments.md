## Submission type

New submission. This is the first submission of `ggpaintr` to CRAN.

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
