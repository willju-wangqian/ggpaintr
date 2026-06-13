#' ggpaintr: Formula-Driven ggplot Shiny Apps
#'
#' `ggpaintr` turns a single ggplot-like formula string into a small Shiny app
#' with generated controls, rendered plots, generated code, inline runtime
#' feedback, upload support for `.csv`, `.tsv`, `.rds`, `.xlsx`, `.xls`, and `.json` files, copy customization, and
#' per-app custom placeholder registries. It also provides lower-level helpers
#' for runtime inspection and column-name normalization for data used with the
#' built-in `ppVar` placeholder.
#'
#' @import ggplot2
#' @importFrom rlang "%||%"
#' @keywords internal
"_PACKAGE"

# `:=` is used unqualified for dynamic argument names in desugar_pipe_to_call()
# (`rlang::call2(rhs, !!nm := lhs)`, R/paintr-eval.R). `.ptr_row` is the bare
# per-row key symbol injected by `aes(key = .ptr_row)` in mint_plotly_key()
# (R/paintr-plotly.R) — written unqualified on purpose (the frozen plotly
# contract observable), so it resolves against the plot's own data at runtime.
# Register both so R CMD check's codetools does not flag them as undefined
# globals (no behaviour change).
utils::globalVariables(c(":=", ".ptr_row"))
