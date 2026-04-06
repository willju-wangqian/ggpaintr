## Test environments

- Local macOS development environment

## R CMD check results

- `devtools::check(document = FALSE, manual = FALSE, args = c("--no-manual"))`
  completed with:
  - 0 errors
  - 0 warnings
  - 1 note
- The remaining note was:
  - `unable to verify current time`
  - this appears to be an environment/timestamp check note rather than a
    package code or metadata issue

## Resubmission

- This release repositions `ggpaintr` around the maintained `ggpaintr`
  workflow and archives the older package implementation in-repo without
  shipping it in the active package.
