# Placeholder registry (rewrite). Three constructors replace the legacy
# `ptr_define_placeholder`. Stored in an isolated env so the legacy registry
# in paintr-placeholders.R is undisturbed during cohabitation.

.ptr_registry <- new.env(parent = emptyenv())
.ptr_registry_initialized <- new.env(parent = emptyenv())
.ptr_registry_initialized$done <- FALSE

ensure_registry_initialized <- function() {
  if (isTRUE(.ptr_registry_initialized$done)) return(invisible(NULL))
  .ptr_registry_initialized$done <- TRUE
  if (length(ls(.ptr_registry)) > 0L) return(invisible(NULL))
  if (exists("ptr_register_builtins", mode = "function")) {
    ptr_register_builtins()
  }
  invisible(NULL)
}

ptr_registry_clear <- function() {
  rm(list = ls(.ptr_registry), envir = .ptr_registry)
  .ptr_registry_initialized$done <- FALSE
  invisible(NULL)
}

#' Remove user-registered placeholders
#'
#' Unregisters placeholders added with [ptr_define_placeholder_value()],
#' [ptr_define_placeholder_consumer()], or [ptr_define_placeholder_source()].
#' The five built-in placeholders (`var`, `text`, `num`, `expr`, `upload`) are
#' never removed.
#'
#' @param keyword Optional single string. When supplied, only that placeholder
#'   is removed. When omitted (the default), every user-registered placeholder
#'   is removed.
#'
#' @return The character vector of keywords that were removed, invisibly.
#'
#' @examples
#' ptr_define_placeholder_value(
#'   "demo_kw",
#'   build_ui = function(node, ...) shiny::textInput(node$id, "demo"),
#'   resolve_expr = function(value, node, ...) value
#' )
#' ptr_clear_placeholder("demo_kw")
#' @export
ptr_clear_placeholder <- function(keyword = NULL) {
  ensure_registry_initialized()
  builtins <- ptr_builtin_keywords()
  registered <- ls(.ptr_registry)

  if (is.null(keyword)) {
    custom <- setdiff(registered, builtins)
    if (length(custom) == 0L) {
      cli::cli_inform(c(i = "No user-registered placeholders to clear."))
      return(invisible(character()))
    }
    rm(list = custom, envir = .ptr_registry)
    cli::cli_inform(c(
      v = "Cleared {length(custom)} placeholder{?s}: {.val {sort(custom)}}."
    ))
    return(invisible(custom))
  }

  assertthat::assert_that(
    is.character(keyword), length(keyword) == 1L,
    !is.na(keyword), nzchar(keyword)
  )
  if (keyword %in% builtins) {
    rlang::abort(paste0(
      "`", keyword, "` is a built-in placeholder and cannot be cleared."
    ))
  }
  if (!keyword %in% registered) {
    custom <- setdiff(registered, builtins)
    rlang::abort(c(
      paste0("No placeholder named `", keyword, "` is registered."),
      i = if (length(custom) > 0L) {
        paste0("Registered placeholders: ",
               paste0("`", sort(custom), "`", collapse = ", "), ".")
      } else {
        "No user-registered placeholders exist."
      }
    ))
  }
  rm(list = keyword, envir = .ptr_registry)
  cli::cli_inform(c(v = "Cleared placeholder: {.val {keyword}}."))
  invisible(keyword)
}

ptr_registry_lookup <- function(keyword) {
  ensure_registry_initialized()
  if (!exists(keyword, envir = .ptr_registry, inherits = FALSE)) {
    return(NULL)
  }
  get(keyword, envir = .ptr_registry, inherits = FALSE)
}

ptr_registry_keywords <- function() {
  ensure_registry_initialized()
  ls(.ptr_registry)
}

ptr_registry_data_aware_keywords <- function() {
  out <- character()
  for (kw in ptr_registry_keywords()) {
    entry <- ptr_registry_lookup(kw)
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

# A named list of length-1 character values, whose names are restricted to
# the supported copy leaf fields (`ptr_ui_text_leaf_fields()`).
validate_copy_defaults <- function(copy_defaults) {
  if (is.null(copy_defaults)) return(invisible(TRUE))
  if (!is.list(copy_defaults)) {
    rlang::abort("`copy_defaults` must be a named list.")
  }
  nms <- names(copy_defaults)
  if (is.null(nms) || any(!nzchar(nms)) || anyDuplicated(nms)) {
    rlang::abort("`copy_defaults` must have unique, non-empty names.")
  }
  unknown <- setdiff(nms, ptr_ui_text_leaf_fields())
  if (length(unknown) > 0L) {
    rlang::abort(paste0(
      "`copy_defaults` has unsupported field(s): ",
      paste(sort(unknown), collapse = ", "),
      ". Allowed: ", paste(ptr_ui_text_leaf_fields(), collapse = ", "), "."
    ))
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

ptr_registry_register <- function(entry) {
  if (exists(entry$keyword, envir = .ptr_registry, inherits = FALSE)) {
    cli::cli_warn("Overwriting placeholder registry entry: {.val {entry$keyword}}.")
  }
  assign(entry$keyword, entry, envir = .ptr_registry)
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
  ptr_registry_register(entry)
}

#' Define a data-consumer placeholder (e.g. column picker)
#'
#' @param keyword,build_ui,resolve_expr,copy_defaults See
#'   [ptr_define_placeholder_value()]. `build_ui` receives `cols` (resolved
#'   upstream column names) and `data` (the resolved upstream data frame —
#'   `NULL` until upstream resolves).
#' @param validate_input Optional `function(value, upstream_cols)` returning
#'   `TRUE` or `NULL` for a valid input, or a character error message string.
#'   `NULL`-on-valid follows the standard R "no message to report" idiom; the
#'   built-in `var` consumer returns `TRUE`, but both forms are accepted.
#' @export
ptr_define_placeholder_consumer <- function(keyword, build_ui, resolve_expr,
                                          validate_input = NULL,
                                          copy_defaults = list(
                                            label = "Pick a column for {param}"
                                          )) {
  validate_keyword(keyword)
  validate_hook(build_ui, "build_ui", c("node", "cols", "data"))
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
  ptr_registry_register(entry)
}

#' Define a data-source placeholder (e.g. upload, database table)
#'
#' A *source* placeholder produces a data frame the rest of the formula
#' reads from. Built-in example: `upload`. Custom examples: database
#' tables, built-in datasets, URL fetches.
#'
#' @param keyword,build_ui,copy_defaults See [ptr_define_placeholder_value()].
#' @param resolve_data Function `function(value, node, ...)` returning a
#'   data.frame (or `NULL` to signal "no data yet").
#' @param resolve_expr Optional override. Default returns `rlang::sym(value)`,
#'   i.e. substitutes the bare keyword (e.g. the user-typed dataset name)
#'   into the generated code. Override when the generated code should
#'   re-fetch the data rather than reference an in-session object — for
#'   example, `function(value, node, ...) rlang::expr(read.csv(!!path))`.
#' @param companion_id_fn Optional `function(id) -> companion_id_string`.
#'   Use this when the source widget needs **two** bound Shiny inputs that
#'   both participate in the runtime substitution cycle: one at `node$id`
#'   (the data payload) and one at `node$companion_id` (a sibling input,
#'   typically a name or override). The framework calls this function with
#'   the primary id and namespaces the returned companion id alongside it,
#'   so a single `build_ui` can render both widgets and both values reach
#'   `resolve_data` / `resolve_expr` through `node`. Most sources do not
#'   need it — one bound input is the common case. The built-in `upload`
#'   uses it to attach the "Optional dataset name" textbox: the file
#'   contents bind to `node$id`, the user-typed dataset name binds to
#'   `node$companion_id`, and the substitution uses the name as the symbol
#'   inserted into the generated code. Pass `NULL` (default) when one
#'   input suffices.
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
  ptr_registry_register(entry)
}
