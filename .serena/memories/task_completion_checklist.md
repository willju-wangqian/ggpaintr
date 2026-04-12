# Task Completion Checklist — ggpaintr

After implementing any change:

1. Run tests: `devtools::test()`
2. If roxygen comments changed: `devtools::document()`
3. Run R CMD check: `devtools::check(document = FALSE, manual = FALSE, args = c("--as-cran", "--no-manual"))` — must pass with 0 errors, 0 warnings
4. For UI/Shiny changes: launch app and verify in browser
5. For new exported functions: ensure roxygen docs exist and NAMESPACE is updated
6. For README changes: edit `README.Rmd`, re-knit to `README.md`

## Never
- Edit NAMESPACE or man/ by hand
- Skip `devtools::document()` after roxygen changes
- Edit `README.md` directly
