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
    if (length(required_args) > 0L) {
      cli::cli_warn(c(
        "{.arg {hook_name}} only declares {.code ...}.",
        i = "Required args: {.val {required_args}}."
      ))
    }
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

# Packages whose top-level function names placeholder keywords must not
# shadow. Per ADR 0009 F1.a: registering a keyword that names a function
# in base R or ggplot2 would silently reinterpret legitimate calls inside
# `ptr_app()` formulas as placeholder invocations.
.ptr_shadow_pkgs <- c(
  "base", "stats", "utils", "methods", "graphics", "grDevices", "ggplot2"
)

# Keywords grandfathered through the F1.a shadow check. Two categories:
#   1. Legacy built-ins from before the ADR 0009 atomic rename (PLAN-08):
#      `var` (stats::var), `text` (graphics::text), `num`, `expr`, `upload`.
#   2. Vignette / fixture custom keywords that the rest of the suite
#      already registers and that this plan is forbidden from touching
#      (scope guard): `range` (base::range) in the plotly-paintr fixture,
#      `date` (base::date) in helper-placeholder-registry.R.
# Both categories disappear once PLAN-08 renames built-ins and the
# fixture rewrite PRs migrate to `pp`-prefixed custom keywords.
.ptr_grandfathered_keywords <- c(
  "var", "text", "num", "expr", "upload",   # built-ins (PLAN-08 target)
  "range", "date"                            # vignette fixtures
)

validate_keyword_no_shadow <- function(keyword) {
  if (keyword %in% .ptr_grandfathered_keywords) return(invisible(TRUE))
  for (pkg in .ptr_shadow_pkgs) {
    ns <- tryCatch(asNamespace(pkg), error = function(e) NULL)
    if (is.null(ns)) next
    if (exists(keyword, envir = ns, inherits = FALSE) &&
        is.function(get(keyword, envir = ns, inherits = FALSE))) {
      rlang::abort(paste0(
        "Placeholder keyword '", keyword, "' shadows `", pkg, "::", keyword,
        "`. Pick a name that does not collide with base R or ggplot2 ",
        "functions (the `pp`-prefix convention sidesteps this)."
      ))
    }
  }
  invisible(TRUE)
}

validate_default_arg <- function(default_arg, keyword) {
  if (is.null(default_arg) || is.function(default_arg)) return(invisible(TRUE))
  rlang::abort(paste0(
    "`default_arg` for placeholder '", keyword,
    "' must be NULL or a function (a validator closure)."
  ))
}

validate_named_args <- function(named_args, keyword) {
  if (!is.list(named_args)) {
    rlang::abort(paste0(
      "`named_args` for placeholder '", keyword, "' must be a list."
    ))
  }
  if (length(named_args) == 0L) return(invisible(TRUE))
  nms <- names(named_args)
  if (is.null(nms) || any(!nzchar(nms)) || anyNA(nms)) {
    rlang::abort(paste0(
      "`named_args` for placeholder '", keyword,
      "' must be a fully-named list (every element needs a non-empty name)."
    ))
  }
  if (anyDuplicated(nms)) {
    rlang::abort(paste0(
      "`named_args` for placeholder '", keyword,
      "' has duplicated names: ",
      paste(unique(nms[duplicated(nms)]), collapse = ", "), "."
    ))
  }
  if ("shared" %in% nms) {
    rlang::abort(paste0(
      "`named_args` for placeholder '", keyword,
      "' may not contain an entry named \"shared\" (shared is reserved by ",
      "ggpaintr for cross-layer binding)."
    ))
  }
  ok <- vapply(named_args, is.function, logical(1))
  if (!all(ok)) {
    bad <- nms[!ok]
    rlang::abort(paste0(
      "`named_args` for placeholder '", keyword,
      "' must contain only validator functions; non-function entries: ",
      paste(bad, collapse = ", "), "."
    ))
  }
  invisible(TRUE)
}

# Best-effort warning when the enclosing `<-` LHS differs from `keyword`.
# Walks `sys.calls()` outward, stops at the first enclosing assignment, and
# warns when the LHS symbol differs from the registered keyword. Stays
# silent (returns NULL) in non-assignment contexts (lapply, top-level call
# without `<-`, etc.). The mismatch is informational only — the
# registration itself still proceeds.
#
# Implementation note: R's `<-` is a primitive that does not normally push
# a call frame, so a top-level `ppFoo <- ptr_define_placeholder_*()` does
# not surface a `<-` frame in `sys.calls()`. The walker is retained as a
# tripwire for the rare shapes where `<-` does appear as a frame (e.g.
# explicit re-binding inside a wrapper that itself takes the LHS symbol as
# an argument), matching the "best-effort" wording in ADR 0009. Authors who
# want a hard guarantee should keep the keyword and the binding identifier
# the same — the explicit-string contract that R6::R6Class and methods::
# setClass also rely on.
ptr_check_keyword_lhs_drift <- function(keyword) {
  calls <- sys.calls()
  if (length(calls) == 0L) return(invisible(NULL))
  for (i in seq.int(length(calls), 1L)) {
    call <- calls[[i]]
    if (is.call(call) && length(call) == 3L &&
        identical(call[[1L]], quote(`<-`))) {
      lhs <- call[[2L]]
      if (is.symbol(lhs) && !identical(as.character(lhs), keyword)) {
        cli::cli_warn(
          paste0(
            "Placeholder definition assigned to `", deparse(lhs),
            "` but registered under keyword \"", keyword,
            "\". The LHS name and the keyword should match for the ",
            "plain-R callable to be in scope under the same identifier."
          )
        )
      }
      return(invisible(NULL))
    }
  }
  invisible(NULL)
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
#' Register a new keyword (e.g. `pct`, `color`, `date`) that ggpaintr will
#' recognise as a substitutable token in a formula. The keyword's widget
#' is built by `build_ui`; the widget's value is turned back into the R
#' code spliced into the rendered call by `resolve_expr`. See
#' `vignette("ggpaintr-customization")` § "Adding a new widget type" for
#' the lifecycle walk-through, signatures table, and runnable
#' `ptr_app()` examples — this help page is reference.
#'
#' Three roles. Pick this constructor for a *value* placeholder (a
#' self-contained widget like a slider, color picker, or numeric input).
#' Use [ptr_define_placeholder_consumer()] when the widget needs the
#' upstream column names (column pickers). Use
#' [ptr_define_placeholder_source()] when the widget *produces* the data
#' the rest of the formula reads from (file upload, dataset chooser).
#'
#' @param keyword Single non-empty string. Must be a syntactically valid R
#'   name (passes `make.names()`) and not an R reserved word. This is the
#'   token users type in the formula, e.g. `geom_point(alpha = pct)`.
#'
#' @param build_ui `function(node, label, ...)` returning a Shiny tag.
#'   Pass `node$id` as the underlying widget's `inputId`. Read `node$keyword`
#'   and `node$param` if you need them. The framework also passes any
#'   `copy_defaults` field you declare by name (`help`, `placeholder`,
#'   `empty_text`) — or accept a `copy = NULL` list and read them off it.
#'   Always end the signature with `...`.
#'
#' @param resolve_expr `function(value, node, ...)` returning the R
#'   expression spliced into the rendered call. `value` is
#'   `input[[node$id]]` — whatever Shiny stores for that widget. Allowed
#'   return types: scalar atomic (numeric / character / logical /
#'   integer), `name`/`symbol` (build with `rlang::sym()`),
#'   `call`/`language` (build with `rlang::expr()`), or `NULL` to **prune
#'   the argument** from the rendered call. Use `NULL` for empty / not-yet
#'   input; throw with `rlang::abort()` for malformed input.
#'
#' @param copy_defaults Named list of single non-NA character defaults
#'   feeding the `ui_text` tree. Allowed names: `label`, `help`,
#'   `placeholder`, `empty_text`. Strings may contain `{param}`, which is
#'   interpolated to the surrounding formal-argument name at render time.
#'
#' @param default_arg Optional validator closure for the (single) positional
#'   argument the keyword accepts inside a formula. `NULL` (default) means
#'   positional arguments are rejected at translate time. A function
#'   receives the unevaluated AST and must return a canonical value or
#'   `rlang::abort()`. Validators are expected to operate on the AST only
#'   and not call `eval()`; ggpaintr trusts the author. Authors who eval in
#'   a validator are opting into the risk of running user code at translate
#'   time.
#'
#' @param named_args Named list of validator closures for additional named
#'   arguments beyond the reserved `shared = ...`. Each entry's closure
#'   receives the unevaluated AST and returns a canonical value or
#'   `rlang::abort()`. Default is `list()` (no named args). The name
#'   `"shared"` is reserved and may not appear here.
#'
#' @param runtime Optional `function(x, ...)` body used when the
#'   placeholder keyword is *also* called as a plain-R function (outside a
#'   formula context). When `NULL` (default), the identity function
#'   `function(x, ...) x` is supplied — calling `pct(0.5)` returns `0.5`
#'   unchanged. Override to give the keyword a non-identity plain-R
#'   meaning. The same runtime is returned to the caller of this helper
#'   so authors can bind it under the same name as the keyword:
#'   `ppPct <- ptr_define_placeholder_value(keyword = "ppPct", ...)`.
#'
#' @return The runtime callable. Default for a value placeholder is the
#'   identity `function(x, ...) x`; override with `runtime = ...`. The
#'   helper is also called for its registration side effect — use
#'   [ptr_clear_placeholder()] to remove the entry.
#'
#' @seealso `vignette("ggpaintr-customization")` for the tutorial;
#'   [ptr_define_placeholder_consumer()], [ptr_define_placeholder_source()],
#'   [ptr_clear_placeholder()].
#'
#' @examples
#' # A percentage placeholder: user types a number 0-100; we splice
#' # the fraction 0-1 into the rendered call.
#' ptr_define_placeholder_value(
#'   keyword = "pct",
#'   build_ui = function(node, label, ...) {
#'     shiny::numericInput(node$id, label = label, value = 50,
#'                         min = 0, max = 100, step = 1)
#'   },
#'   resolve_expr = function(value, node, ...) {
#'     if (length(value) != 1L || !is.finite(value)) return(NULL)
#'     value / 100
#'   },
#'   copy_defaults = list(label = "Percent for {param}")
#' )
#' ptr_clear_placeholder("pct")
#' @export
ptr_define_placeholder_value <- function(keyword, build_ui, resolve_expr,
                                       default_arg = NULL,
                                       named_args = list(),
                                       runtime = NULL,
                                       copy_defaults = list(
                                         label = "Enter a value for {param}"
                                       )) {
  validate_keyword(keyword)
  validate_keyword_no_shadow(keyword)
  validate_hook(build_ui, "build_ui", c("node"))
  validate_hook(resolve_expr, "resolve_expr", c("value", "node"))
  validate_default_arg(default_arg, keyword)
  validate_named_args(named_args, keyword)
  validate_copy_defaults(copy_defaults)
  ptr_check_keyword_lhs_drift(keyword)

  runtime_fn <- runtime %||% function(x, ...) x
  validate_hook(runtime_fn, "runtime", character())

  entry <- list(
    keyword = keyword, role = "value", data_aware = FALSE,
    build_ui = build_ui, resolve_expr = resolve_expr,
    default_arg = default_arg, named_args = named_args,
    runtime = runtime_fn,
    copy_defaults = copy_defaults
  )
  ptr_registry_register(entry)
  runtime_fn
}

#' Define a data-consumer placeholder (e.g. column picker)
#'
#' A *consumer* placeholder is a value placeholder that additionally
#' receives the columns of the upstream data frame — typically a column
#' picker. The built-in example is `var`. See
#' `vignette("ggpaintr-customization")` § "Consumer placeholders" for the
#' tutorial.
#'
#' @param keyword,copy_defaults See [ptr_define_placeholder_value()].
#'
#' @param build_ui `function(node, cols, data, label, ...)` returning a
#'   Shiny tag. `cols` is a character vector of upstream column names
#'   (use as `choices`); `character(0)` before upstream resolves. `data`
#'   is the upstream data frame, or `NULL` while pending — read it only
#'   when you need column types / levels / ranges.
#'
#' @param resolve_expr `function(value, node, ...)`. For a column picker
#'   the typical body is `rlang::sym(value)` so the bare column name is
#'   spliced as an identifier rather than a string literal. See
#'   [ptr_define_placeholder_value()] for allowed return types and the
#'   `NULL`-prunes-the-argument convention.
#'
#' @param validate_input Optional `function(value, upstream_cols)` called
#'   before `resolve_expr`. Return `TRUE` / `NULL` to accept; return a
#'   single character string to reject (surfaced inline as the error
#'   message, layer pruned). Useful when a stale selection no longer
#'   matches any upstream column after a data swap.
#'
#' @param default_arg,named_args See [ptr_define_placeholder_value()].
#'   Consumer placeholders use the same arg-schema slots; the `var`
#'   built-in passes a column-name validator here when used as `var(mpg)`.
#'
#' @param runtime Optional `function(x, ...)` body used when the
#'   placeholder is called as a plain-R function. `NULL` (default) supplies
#'   the identity `function(x, ...) x`, matching the legacy `ppVar`-style
#'   `aes()` NSE shape (the symbol-passthrough convention). Override to
#'   give the consumer a non-identity plain-R meaning.
#'
#' @return The runtime callable (identity by default; override with
#'   `runtime = ...`). Also called for its registration side effect; use
#'   [ptr_clear_placeholder()] to remove it.
#'
#' @seealso `vignette("ggpaintr-customization")` for the tutorial;
#'   [ptr_define_placeholder_value()], [ptr_define_placeholder_source()].
#'
#' @examples
#' # A consumer that picks a numeric-only column.
#' ptr_define_placeholder_consumer(
#'   keyword = "numvar",
#'   build_ui = function(node, cols, data, label, ...) {
#'     numeric_cols <- if (is.null(data)) character(0) else
#'       names(data)[vapply(data, is.numeric, logical(1))]
#'     shiny::selectInput(node$id, label = label, choices = numeric_cols,
#'                        selected = character(0))
#'   },
#'   resolve_expr = function(value, node, ...) {
#'     if (length(value) != 1L || !nzchar(value)) return(NULL)
#'     rlang::sym(value)
#'   },
#'   validate_input = function(value, upstream_cols) {
#'     if (length(value) == 1L && value %in% upstream_cols) TRUE
#'     else "Pick a column that exists in the upstream data."
#'   }
#' )
#' ptr_clear_placeholder("numvar")
#' @export
ptr_define_placeholder_consumer <- function(keyword, build_ui, resolve_expr,
                                          validate_input = NULL,
                                          default_arg = NULL,
                                          named_args = list(),
                                          runtime = NULL,
                                          copy_defaults = list(
                                            label = "Pick a column for {param}"
                                          )) {
  validate_keyword(keyword)
  validate_keyword_no_shadow(keyword)
  validate_hook(build_ui, "build_ui", c("node", "cols", "data"))
  validate_hook(resolve_expr, "resolve_expr", c("value", "node"))
  if (!is.null(validate_input)) {
    validate_hook(validate_input, "validate_input", c("value", "upstream_cols"))
  }
  validate_default_arg(default_arg, keyword)
  validate_named_args(named_args, keyword)
  validate_copy_defaults(copy_defaults)
  ptr_check_keyword_lhs_drift(keyword)

  runtime_fn <- runtime %||% function(x, ...) x
  validate_hook(runtime_fn, "runtime", character())

  entry <- list(
    keyword = keyword, role = "consumer", data_aware = TRUE,
    build_ui = build_ui, resolve_expr = resolve_expr,
    validate_input = validate_input,
    default_arg = default_arg, named_args = named_args,
    runtime = runtime_fn,
    copy_defaults = copy_defaults
  )
  ptr_registry_register(entry)
  runtime_fn
}

#' Define a data-source placeholder (e.g. upload, database table)
#'
#' A *source* placeholder produces a data frame the rest of the formula
#' reads from. Built-in example: `upload`. Custom examples: database
#' tables, built-in datasets, URL fetches.
#'
#' @param keyword,copy_defaults See [ptr_define_placeholder_value()]. See
#'   `vignette("ggpaintr-customization")` § "Source placeholders" for the
#'   tutorial.
#'
#' @param build_ui `function(node, label, ...)` returning a Shiny tag —
#'   same shape as in [ptr_define_placeholder_value()]. With
#'   `companion_id_fn` set, render **two** bound inputs in the same tag,
#'   one with `inputId = node$id` (data payload) and one with
#'   `inputId = node$companion_id` (sibling input — typically the
#'   user-facing dataset name spliced into the rendered code).
#'
#' @param resolve_data `function(value, node, ...)` returning a
#'   `data.frame` (the data downstream consumers read from), or `NULL` to
#'   signal "no data yet". Throw via `rlang::abort()` for malformed
#'   inputs.
#'
#' @param resolve_expr Optional. `function(value, node, ...)` returning
#'   the expression spliced into the rendered code at the placeholder's
#'   position — i.e. *how the data is referred to* in the reproducible
#'   call, not the data itself. Default `rlang::sym(value)` works when the
#'   widget's value is already the symbol you want. Override to make the
#'   rendered code re-fetch instead of referencing an in-session object,
#'   e.g. `function(value, node, ...) rlang::expr(read.csv(!!value$datapath))`.
#'   With `companion_id_fn` set, `value` here is the *companion* input's
#'   value (e.g. the typed dataset name), **not** the primary payload —
#'   the built-in `upload` relies on this so the default splices the typed
#'   name as a bare symbol.
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
#'
#' @param default_arg,named_args See [ptr_define_placeholder_value()].
#'   Source placeholders use the same arg-schema slots.
#'
#' @param runtime Optional `function(...)` body used when the placeholder
#'   is called as a plain-R function (outside `ptr_app()`). `NULL`
#'   (default) supplies a guard that aborts with a message naming the
#'   keyword and noting the call is only meaningful inside `ptr_app()` —
#'   source placeholders typically have no out-of-app meaning (a file
#'   upload widget cannot produce data at the REPL). Override only if the
#'   source has a sensible plain-R interpretation.
#'
#' @return The runtime callable. Default for a source placeholder is a
#'   guard that aborts when called outside an app context. Also called for
#'   its registration side effect; use [ptr_clear_placeholder()] to remove
#'   the entry.
#' @seealso [ptr_define_placeholder_value()], [ptr_define_placeholder_consumer()],
#'   [ptr_clear_placeholder()].
#' @examples
#' # A minimal in-memory dataset source (picks from pre-loaded data frames).
#' ptr_define_placeholder_source(
#'   keyword = "dataset",
#'   build_ui = function(node, label, ...) {
#'     shiny::selectInput(node$id, label = label,
#'                        choices = c("mtcars", "iris"))
#'   },
#'   resolve_data = function(value, node, ...) {
#'     if (length(value) != 1L || !nzchar(value)) return(NULL)
#'     get(value, envir = as.environment("package:datasets"))
#'   }
#' )
#' ptr_clear_placeholder("dataset")
#' @export
ptr_define_placeholder_source <- function(keyword, build_ui, resolve_data,
                                        resolve_expr = NULL,
                                        companion_id_fn = NULL,
                                        default_arg = NULL,
                                        named_args = list(),
                                        runtime = NULL,
                                        copy_defaults = list(
                                          label = "Provide a data source for {param}"
                                        )) {
  validate_keyword(keyword)
  validate_keyword_no_shadow(keyword)
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
  validate_default_arg(default_arg, keyword)
  validate_named_args(named_args, keyword)
  validate_copy_defaults(copy_defaults)
  ptr_check_keyword_lhs_drift(keyword)

  runtime_fn <- runtime %||% local({
    kw <- keyword
    function(...) rlang::abort(
      paste0("`", kw, "()` is only meaningful inside `ptr_app()`.")
    )
  })
  validate_hook(runtime_fn, "runtime", character())

  entry <- list(
    keyword = keyword, role = "source", data_aware = TRUE,
    build_ui = build_ui, resolve_expr = resolve_expr,
    resolve_data = resolve_data, companion_id_fn = companion_id_fn,
    default_arg = default_arg, named_args = named_args,
    runtime = runtime_fn,
    copy_defaults = copy_defaults
  )
  ptr_registry_register(entry)
  runtime_fn
}
