# Level 3 — headless plot from a formula

Use when: render a plot from a formula without launching Shiny — batch
reports, test fixtures, programmatic code generation, pre-computing
artifacts for a static site.

## Pipeline

1. `ptr_parse_formula(formula, placeholders = NULL)` → `ptr_obj`.
2. `ptr_runtime_input_spec(ptr_obj)` → data frame, one row per Shiny
   input id the app *would* need. Columns include `input_id`, `role`,
   `layer_name`, `param_key`.
3. Build a named list mirroring the spec — one entry per `input_id`.
4. `ptr_exec(ptr_obj, inputs)` → runtime result (never raises).
5. `ptr_extract_plot(result)` → `ggplot` object (or `NULL`).

## Example

```r
library(ggpaintr)

obj <- ptr_parse_formula(
  "ggplot(data = mtcars, aes(x = var, y = var)) +
     geom_point() +
     labs(title = text)"
)

spec <- ptr_runtime_input_spec(obj)
spec  # inspect input_ids, roles (placeholder / layer_checkbox / upload_name)

inputs <- setNames(vector("list", nrow(spec)), spec$input_id)

# Every layer checkbox starts TRUE (= include that layer).
checkbox_rows <- spec$role == "layer_checkbox"
inputs[checkbox_rows] <- rep(list(TRUE), sum(checkbox_rows))

# Fill one placeholder value per row.
pick <- function(layer_name, param_key) {
  spec$input_id[spec$layer_name == layer_name & spec$param_key == param_key]
}
inputs[[pick("ggplot", "x")]]     <- "wt"
inputs[[pick("ggplot", "y")]]     <- "mpg"
inputs[[pick("labs",   "title")]] <- "Weight vs MPG"

result <- ptr_exec(obj, inputs)
result$ok                         # TRUE on success
p <- ptr_extract_plot(result)     # ordinary ggplot object
print(p)                           # or ggsave(), knitr, etc.
```

## Code extraction

```r
cat(ptr_extract_code(result))
```

`ptr_extract_code()` accepts an optional `extras =` list of
`rlang::quosure`s that are appended to the code text when the runtime
succeeded:

```r
extras <- list(rlang::quo(ggplot2::theme_minimal()))
cat(ptr_extract_code(result, extras = extras))
```

## Failure handling — never raises

```r
broken <- inputs
broken[[pick("ggplot", "x")]] <- "not_a_column"
bad <- ptr_exec(obj, broken)
bad$ok                        # FALSE
bad$message                   # error string
ptr_extract_plot(bad)         # NULL
cat(ptr_extract_code(bad))    # still returns the attempted call
                              # extras are suppressed on failure
```

All three extract helpers return `NULL` / the attempted value on
abnormal state — safe to call from any reactive context without `req()`.
