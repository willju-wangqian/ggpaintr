## Test environments

- Local Ubuntu 22.04.5 LTS development environment (`R 4.3.3`, `aarch64`)

## R CMD check results

- `R CMD check --as-cran --no-manual ggpaintr_0.1.0.tar.gz`
  completed with:
  - 0 errors
  - 1 warning
  - 2 notes
- The remaining warning/note items were:
  - `‘qpdf’ is needed for checks on size reduction of PDFs`
  - this is an environment dependency warning from the local check machine
  - `unable to verify current time`
  - this appears to be an environment/timestamp check note rather than a
    package code or metadata issue

## Resubmission

- This release repositions `ggpaintr` around the maintained `ggpaintr`
  workflow and archives the older package implementation in-repo without
  shipping it in the active package.
