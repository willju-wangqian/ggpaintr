# Suggested Commands — ggpaintr

## Development
```r
devtools::load_all(".")                   # load package interactively
devtools::document()                      # regenerate NAMESPACE + man/ from roxygen
```

## Testing
```r
devtools::test()                          # run all tests
devtools::test(filter = "parse-formula")  # run one test file (matches test-<filter>.R)
```
Shell equivalent: `Rscript -e "devtools::test()"`

## Checking
```r
devtools::check(document = FALSE, manual = FALSE, args = c("--as-cran", "--no-manual"))
```
Shell: `Rscript -e "devtools::check()"`
Requirement: 0 errors, 0 warnings.

## Docs
```r
source("dev/build-pkgdown.R"); build_pkgdown_clean()  # build pkgdown site (excludes CLAUDE.md)
devtools::load_all("."); rmarkdown::render("README.Rmd", envir = globalenv())  # rebuild README.md
```

## Workflows (via harness)
```
/ewh:doit add-feature
/ewh:doit refine-feature
/ewh:doit fact-check
/ewh:doit knowledge-update
/ewh:doit clean-up
```
