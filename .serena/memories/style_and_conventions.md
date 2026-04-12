# Style & Conventions — ggpaintr

## Naming
- Public API functions: `ptr_` prefix, exported via roxygen `@export`
- Internal helpers: no prefix, unexported
- snake_case throughout; 2-space indent

## Error & Messaging
- `rlang::abort()` for errors (not `stop()`)
- `rlang::warn()` for warnings (not `warning()`)
- `assertthat::assert_that()` for argument validation
- `cli::cli_inform()` / `cli::cli_warn()` for user-facing messages

## Docs
- Roxygen2 for all exported functions
- Run `devtools::document()` after changing roxygen comments
- NAMESPACE and man/ are auto-generated — never edit by hand

## Tests
- testthat edition 3
- `expect_equal()`, `expect_error()`, `expect_true()`, `expect_s3_class()`
- `withr::local_*()` for temporary state
- Group by feature/behavior, not by source file

## README
- Edit `README.Rmd`, then re-knit to produce `README.md`
- Never edit `README.md` directly

## Other
- `dev/developer-notes.md` is human-maintained only
- Do not create ad-hoc markdown files in `.claude/specs/`
- Source of truth: source code + current tests (not notes/docs)
