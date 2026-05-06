# Placeholder registry (rewrite). Three constructors replace the legacy
# `ptr_define_placeholder`. Stored in an isolated env so the legacy registry
# in paintr-placeholders.R is undisturbed during cohabitation.

.ptr_registry_v2 <- new.env(parent = emptyenv())
.ptr_registry_v2_initialized <- new.env(parent = emptyenv())
.ptr_registry_v2_initialized$done <- FALSE

ensure_registry_v2_initialized <- function() {
  if (isTRUE(.ptr_registry_v2_initialized$done)) return(invisible(NULL))
  .ptr_registry_v2_initialized$done <- TRUE
  if (length(ls(.ptr_registry_v2)) > 0L) return(invisible(NULL))
  if (exists("ptr_register_builtins_v2", mode = "function")) {
    ptr_register_builtins_v2()
  }
  invisible(NULL)
}

ptr_registry_v2_clear <- function() {
  rm(list = ls(.ptr_registry_v2), envir = .ptr_registry_v2)
  .ptr_registry_v2_initialized$done <- FALSE
  invisible(NULL)
}

ptr_registry_v2_lookup <- function(keyword) {
  ensure_registry_v2_initialized()
  if (!exists(keyword, envir = .ptr_registry_v2, inherits = FALSE)) {
    return(NULL)
  }
  get(keyword, envir = .ptr_registry_v2, inherits = FALSE)
}

ptr_registry_v2_keywords <- function() {
  ensure_registry_v2_initialized()
  ls(.ptr_registry_v2)
}

ptr_registry_v2_data_aware_keywords <- function() {
  out <- character()
  for (kw in ptr_registry_v2_keywords()) {
    entry <- ptr_registry_v2_lookup(kw)
    if (isTRUE(entry$data_aware)) out <- c(out, kw)
  }
  out
}

# R reserved words; cannot be a keyword.
.ptr_reserved <- c(
  "if", "else", "repeat", "while", "function", "for", "in", "next", "break",
  "TRUE", "FALSE", "NULL", "Inf", "NaN", "NA",
  "NA_integer_", "NA_real_", "NA_complex_", "NA_character_", "..."
)

validate_keyword <- function(keyword) {
  assertthat::assert_that(
    is.character(keyword), length(keyword) == 1L, !is.na(keyword),
    nzchar(keyword)
  )
  if (keyword %in% .ptr_reserved) {
    rlang::abort(paste0("`keyword` cannot be a reserved word: ", keyword))
  }
  if (make.names(keyword) != keyword) {
    rlang::abort(paste0("`keyword` is not a syntactically valid R name: ", keyword))
  }
  invisible(TRUE)
}

validate_hook <- function(fn, hook_name, required_args = character()) {
  if (!is.function(fn)) {
    rlang::abort(paste0("`", hook_name, "` must be a function."))
  }
  fmls <- names(formals(fn))
  if (length(fmls) == 1L && identical(fmls, "...")) {
    cli::cli_warn(c(
      "{.arg {hook_name}} only declares {.code ...}.",
      i = "Required args: {.val {required_args}}."
    ))
    return(invisible(TRUE))
  }
  has_dots <- "..." %in% fmls
  missing_args <- setdiff(required_args, fmls)
  if (length(missing_args) > 0L && !has_dots) {
    rlang::abort(paste0(
      "`", hook_name, "` must accept argument(s): ",
      paste(missing_args, collapse = ", "),
      " (or `...`)."
    ))
  }
  invisible(TRUE)
}

# Permissive validation: a named list of length-1 character values. Field
# whitelist (label/tooltip/placeholder/help/...) tightens at P6 wiring.
validate_copy_defaults <- function(copy_defaults) {
  if (is.null(copy_defaults)) return(invisible(TRUE))
  if (!is.list(copy_defaults)) {
    rlang::abort("`copy_defaults` must be a named list.")
  }
  nms <- names(copy_defaults)
  if (is.null(nms) || any(!nzchar(nms)) || anyDuplicated(nms)) {
    rlang::abort("`copy_defaults` must have unique, non-empty names.")
  }
  for (i in seq_along(copy_defaults)) {
    v <- copy_defaults[[i]]
    if (!is.character(v) || length(v) != 1L || is.na(v)) {
      rlang::abort(paste0(
        "`copy_defaults$", nms[[i]], "` must be a single non-NA string."
      ))
    }
  }
  invisible(TRUE)
}

ptr_registry_v2_register <- function(entry) {
  if (exists(entry$keyword, envir = .ptr_registry_v2, inherits = FALSE)) {
    cli::cli_warn("Overwriting placeholder registry entry: {.val {entry$keyword}}.")
  }
  assign(entry$keyword, entry, envir = .ptr_registry_v2)
  invisible(entry)
}

#' Define a value placeholder
#'
#' @param keyword Single non-empty string; valid R name; not reserved.
#' @param build_ui Function `function(node, ...)` returning a Shiny tag.
#' @param resolve_expr Function `function(value, node, ...)` returning the
#'   substituted expression. Allowed return types: numeric, character, logical,
#'   integer, NULL, language, symbol, expression.
#' @param copy_defaults Named list of single-string default copy entries.
#' @export
ptr_define_placeholder_value <- function(keyword, build_ui, resolve_expr,
                                       copy_defaults = list(
                                         label = "Enter a value for {param}"
                                       )) {
  validate_keyword(keyword)
  validate_hook(build_ui, "build_ui", c("node"))
  validate_hook(resolve_expr, "resolve_expr", c("value", "node"))
  validate_copy_defaults(copy_defaults)
  entry <- list(
    keyword = keyword, role = "value", data_aware = FALSE,
    build_ui = build_ui, resolve_expr = resolve_expr,
    copy_defaults = copy_defaults
  )
  ptr_registry_v2_register(entry)
}

#' Define a data-consumer placeholder (e.g. column picker)
#'
#' @param keyword,build_ui,resolve_expr,copy_defaults See
#'   [ptr_define_placeholder_value()]. `build_ui` receives `cols` (resolved
#'   upstream column names).
#' @param validate_input Optional `function(value, upstream_cols)` returning
#'   `TRUE` or an error message string.
#' @export
ptr_define_placeholder_consumer <- function(keyword, build_ui, resolve_expr,
                                          validate_input = NULL,
                                          copy_defaults = list(
                                            label = "Pick a column for {param}"
                                          )) {
  validate_keyword(keyword)
  validate_hook(build_ui, "build_ui", c("node", "cols"))
  validate_hook(resolve_expr, "resolve_expr", c("value", "node"))
  if (!is.null(validate_input)) {
    validate_hook(validate_input, "validate_input", c("value", "upstream_cols"))
  }
  validate_copy_defaults(copy_defaults)
  entry <- list(
    keyword = keyword, role = "consumer", data_aware = TRUE,
    build_ui = build_ui, resolve_expr = resolve_expr,
    validate_input = validate_input,
    copy_defaults = copy_defaults
  )
  ptr_registry_v2_register(entry)
}

#' Define a data-source placeholder (e.g. upload, database table)
#'
#' @param keyword,build_ui,copy_defaults See [ptr_define_placeholder_value()].
#' @param resolve_data Function `function(value, node, ...)` returning a
#'   data.frame.
#' @param resolve_expr Optional override; default returns `rlang::sym(value)`.
#' @param companion_id_fn Optional `function(id) -> companion_id` for paired
#'   widgets (e.g. upload-name).
#' @export
ptr_define_placeholder_source <- function(keyword, build_ui, resolve_data,
                                        resolve_expr = NULL,
                                        companion_id_fn = NULL,
                                        copy_defaults = list(
                                          label = "Provide a data source for {param}"
                                        )) {
  validate_keyword(keyword)
  validate_hook(build_ui, "build_ui", c("node"))
  validate_hook(resolve_data, "resolve_data", c("value", "node"))
  if (is.null(resolve_expr)) {
    resolve_expr <- function(value, node, ...) rlang::sym(value)
  } else {
    validate_hook(resolve_expr, "resolve_expr", c("value", "node"))
  }
  if (!is.null(companion_id_fn)) {
    validate_hook(companion_id_fn, "companion_id_fn", c("id"))
  }
  validate_copy_defaults(copy_defaults)
  entry <- list(
    keyword = keyword, role = "source", data_aware = TRUE,
    build_ui = build_ui, resolve_expr = resolve_expr,
    resolve_data = resolve_data, companion_id_fn = companion_id_fn,
    copy_defaults = copy_defaults
  )
  ptr_registry_v2_register(entry)
}
