# Built-in placeholders registered against `.ptr_registry`.
#
# Wiring summary:
#   text  -> value     (textInput)
#   num   -> value     (numericInput)
#   expr  -> value     (textAreaInput; P8 wraps in ptr_user_expr)
#   var   -> consumer  (pickerInput; cols resolved from upstream)
#   upload-> source    (fileInput + companion textInput; resolve_data
#                       delegates to ptr_read_uploaded_data verbatim)
#
# UI hooks here are minimal but callable; the app/UI layer wires them up
# at the cutover phase.

# Wrap a control in a tagList with a helpText note when `help` is a
# non-empty string; otherwise return the control unchanged. Mirrors the
# legacy `ptr_attach_help()` from paintr-ui.R.
attach_help <- function(ui, help) {
  if (is.null(help) || !nzchar(trimws(help))) return(ui)
  shiny::tagList(ui, shiny::helpText(help))
}

# ---- text -------------------------------------------------------------------

ptr_builtin_text_build_ui <- function(node, label = NULL, copy = NULL,
                                      selected = NULL, ...) {
  # PLAN-07: seed initial value from `selected` (orchestrator passes
  # `node$default` here when no persisted input exists).
  initial <- if (is.null(selected) || length(selected) == 0L) {
    ""
  } else {
    as.character(selected)[[1L]]
  }
  attach_help(
    shiny::textInput(
      inputId = node$id,
      label = label %||% "Enter a value",
      value = initial,
      placeholder = copy$placeholder
    ),
    copy$help
  )
}

ptr_builtin_text_resolve_expr <- function(value, node, ...) {
  if (!is.character(value) || length(value) != 1L) {
    return(as.character(value))
  }
  strip_matched_quote_pair(value)
}

strip_matched_quote_pair <- function(s) {
  if (nchar(s) < 2L) return(s)
  first <- substr(s, 1L, 1L)
  last <- substr(s, nchar(s), nchar(s))
  if (first == last && first %in% c("'", "\"")) {
    return(substr(s, 2L, nchar(s) - 1L))
  }
  s
}

# ---- num --------------------------------------------------------------------

ptr_builtin_num_build_ui <- function(node, label = NULL, copy = NULL,
                                     selected = NULL, ...) {
  # PLAN-07: seed initial value from `selected` (orchestrator passes
  # `node$default` here when no persisted input exists).
  initial <- if (is.null(selected) || length(selected) == 0L) {
    NA_real_
  } else {
    suppressWarnings(as.numeric(selected[[1L]]))
  }
  control <- shiny::numericInput(
    inputId = node$id,
    label = label %||% "Enter a number",
    value = initial
  )
  has_initial <- !is.na(initial)
  # shiny::numericInput unconditionally renders `value="<as.character(initial)>"`,
  # which is the literal string "NA" when initial is NA_real_. Walk into the
  # rendered `<input>` and (a) overwrite value with a clean numeric (or empty
  # string when no seed), and (b) stamp `copy$empty_text` as the HTML placeholder
  # attribute (numericInput does not expose `placeholder`). Both edits live on
  # the same child so a single walk handles them.
  for (i in seq_along(control$children)) {
    child <- control$children[[i]]
    if (inherits(child, "shiny.tag") && identical(child$name, "input")) {
      if (!is.null(copy$empty_text) && nzchar(copy$empty_text)) {
        child$attribs$placeholder <- copy$empty_text
      }
      if (has_initial) {
        # Use formatC to keep integer-shaped seeds like 5 from rendering as "5.0".
        child$attribs$value <- formatC(initial, format = "fg",
                                       drop0trailing = TRUE)
      } else if (!is.null(copy$empty_text) && nzchar(copy$empty_text)) {
        child$attribs$value <- ""
      }
      control$children[[i]] <- child
      break
    }
  }
  attach_help(control, copy$help)
}

ptr_builtin_num_resolve_expr <- function(value, node, ...) {
  # `as.numeric("abc")` is `NA_real_` with a warning. Drop those cases via
  # NULL so substitute marks the arg as missing and prune deletes it,
  # rather than splicing `NA_real_` into the rendered code.
  out <- suppressWarnings(as.numeric(value))
  if (length(out) != 1L || is.na(out)) return(NULL)
  out
}

# ---- expr -------------------------------------------------------------------

ptr_builtin_expr_build_ui <- function(node, label = NULL, copy = NULL,
                                      selected = NULL, ...) {
  # PLAN-07: seed initial textarea value from `selected`. Strings render
  # verbatim; a language default (from `ptr_default_expression`) is
  # deparsed back to source text.
  initial <- if (is.null(selected) ||
                 (is.atomic(selected) && length(selected) == 0L)) {
    ""
  } else if (is.language(selected)) {
    paste(deparse(selected), collapse = "\n")
  } else {
    as.character(selected)[[1L]]
  }
  attach_help(
    shiny::textAreaInput(
      inputId = node$id,
      label = label %||% "Enter an expression",
      value = initial,
      placeholder = copy$placeholder
    ),
    copy$help
  )
}

ptr_builtin_expr_resolve_expr <- function(value, node, ...) {
  if (!is.character(value) || length(value) != 1L || !nzchar(value)) {
    rlang::abort(paste0(
      "An `expr` input must contain a single R expression written as text ",
      "(for example: `factor(cyl)`)."
    ))
  }
  exprs <- tryCatch(
    rlang::parse_exprs(value),
    error = function(e) {
      rlang::abort(paste0(
        "expr placeholder: could not parse input as R expression: ",
        conditionMessage(e)
      ))
    }
  )
  if (length(exprs) != 1L) {
    rlang::abort(paste0(
      "expr placeholder: input must contain exactly one expression, but ",
      length(exprs), " were found."
    ))
  }
  exprs[[1]]
}

# ---- var --------------------------------------------------------------------

ptr_builtin_var_build_ui <- function(node, cols = character(),
                                     label = NULL, copy = NULL,
                                     selected = character(0), ...) {
  picker_label <- label %||% "Pick a column"
  none_text <- copy$empty_text %||% picker_label
  # Legacy paintr trick: `multiple = TRUE` + `maxOptions = 1L`. This buys
  # two real-browser behaviours that single-select pickerInput can't:
  #   1. No first-choice default at launch (input value starts as
  #      character(0), not the first column — verified in a probe app).
  #   2. Clicking the currently-selected column deselects it (the
  #      old single-select picker silently ignored the click).
  # `noneSelectedText` is the placeholder shown when nothing is picked.
  retained <- intersect(selected, cols)
  attach_help(
    shinyWidgets::pickerInput(
      inputId = node$id,
      label = picker_label,
      choices = cols,
      selected = retained,
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(
        noneSelectedText = none_text,
        maxOptions = 1L
      )
    ),
    copy$help
  )
}

ptr_builtin_var_resolve_expr <- function(value, node, ...) {
  if (!is.character(value) || length(value) != 1L || !nzchar(value)) {
    rlang::abort("A `var` input must be a single column name.")
  }
  rlang::sym(value)
}

ptr_builtin_var_validate_input <- function(value, ctx) {
  if (length(value) != 1L) {
    return(paste0("Expected a single column; got ", length(value), "."))
  }
  if (!value %in% ctx$upstream_cols) {
    return(paste0("Column `", value, "` is not in the data."))
  }
  TRUE
}

# ---- upload -----------------------------------------------------------------

# Kept in sync with the extensions `ptr_read_uploaded_data()` dispatches on.
ptr_upload_accept_formats <- function() {
  c(".csv", ".tsv", ".rds", ".xlsx", ".xls", ".json")
}

ptr_builtin_upload_build_ui <- function(node, label = NULL, copy = NULL,
                                        file_copy = NULL, name_copy = NULL,
                                        ...) {
  shiny::tagList(
    attach_help(
      shiny::fileInput(
        inputId = node$id,
        label = file_copy$label %||% label %||% "Choose a data file",
        accept = ptr_upload_accept_formats()
      ),
      file_copy$help
    ),
    attach_help(
      shiny::textInput(
        inputId = node$companion_id %||% paste0(node$id, "_name"),
        label = name_copy$label %||% "Optional dataset name",
        value = node$default %||% "",
        placeholder = name_copy$placeholder
      ),
      name_copy$help
    )
  )
}

# Delegates to the existing reader in paintr-upload.R verbatim.
ptr_builtin_upload_resolve_data <- function(value, node, ...) {
  ptr_read_uploaded_data(value)
}

# ---- registration -----------------------------------------------------------

# Keywords registered by `ptr_register_builtins()`. Maintained by hand --
# `ptr_clear_placeholder()` uses this to tell built-ins (which it refuses to
# drop) apart from user-registered placeholders.
ptr_builtin_keywords <- function() {
  c(
    "ppText", "ppNum", "ppExpr", "ppVar", "ppUpload",
    # ADR 0020 structural keywords. They are registered alongside the
    # placeholder built-ins so `ptr_clear_placeholder()` refuses to drop
    # them, but they carry `role = "structural"` (not a placeholder role)
    # and are intercepted by the translator before placeholder-arg
    # extraction ever runs.
    "ppLayerOff", "ppVerbOff"
  )
}

#' Placeholder Identity Helpers
#'
#' These are the plain-R callables returned by registering the five
#' built-in ggpaintr placeholders (`ppVar`, `ppNum`, `ppText`, `ppExpr`,
#' `ppUpload`). Inside a formula passed to [ptr_app()] / [ptr_server()]
#' the parser recognises calls to these names as placeholder invocations
#' and binds them to Shiny widgets (see `vignette("ggpaintr-use-cases")`).
#' Outside `ptr_app()` they behave as plain R functions: the first four
#' return their argument unchanged (identity), so a formula such as
#' `aes(x = ppVar(mpg))` evaluates identically to `aes(x = mpg)` under
#' ggplot2's tidy-eval. `ppUpload` is identical when called with an
#' argument (`ppUpload(penguins)` returns `penguins`), so a formula such
#' as `ppUpload(penguins) |> filter(...) |> ggplot(...)` evaluates as
#' plain R when `penguins` is in scope. The no-arg form `ppUpload()`
#' aborts outside `ptr_app()` — it is meaningful only as a placeholder
#' slot.
#'
#' @param x A column name (`ppVar`), numeric (`ppNum`), string (`ppText`),
#'   expression (`ppExpr`), or dataset name/value (`ppUpload`). Passed
#'   through unchanged.
#' @param ... Additional arguments (e.g. named arguments consumed by a
#'   custom placeholder's `named_args` schema). Ignored by the built-in
#'   identity implementation.
#'
#' @return The input value unchanged. The no-arg form `ppUpload()` does
#'   not return; it aborts with a guard message.
#'
#' @examples
#' # Identity inside ggplot2's tidy-eval:
#' library(ggplot2)
#' p1 <- ggplot(mtcars, aes(x = mpg)) + geom_histogram(bins = 10)
#' p2 <- ggplot(mtcars, aes(x = ppVar(mpg))) + geom_histogram(bins = 10)
#' # p1 and p2 produce the same plot.
#'
#' # Inside ptr_app() / ptr_server(), the same call binds to a column picker:
#' # ptr_app("ggplot(mtcars, aes(x = ppVar(mpg))) + geom_point()")
#' @name pp_placeholders
NULL

ptr_register_builtins <- function() {
  # Break the init recursion: each constructor below calls
  # `ensure_registry_initialized()` (ADR-0014 Shape A), which on a cleared
  # registry would re-enter this function and double-register every
  # builtin, raising five "Overwriting placeholder" warnings per call.
  # Flipping `done` first short-circuits that re-entry; the constructors
  # then just `ptr_registry_register()` cleanly into the empty env.
  .ptr_registry_initialized$done <- TRUE
  text_fn <- ptr_define_placeholder_value(
    keyword = "ppText",
    build_ui = ptr_builtin_text_build_ui,
    resolve_expr = ptr_builtin_text_resolve_expr,
    default_arg = ptr_default_string(),
    copy_defaults = list(
      label = "Enter a value for {param}",
      placeholder = "Plain text - quotes are added automatically"
    )
  )
  num_fn <- ptr_define_placeholder_value(
    keyword = "ppNum",
    build_ui = ptr_builtin_num_build_ui,
    resolve_expr = ptr_builtin_num_resolve_expr,
    default_arg = ptr_default_numeric(),
    copy_defaults = list(label = "Enter a number for {param}")
  )
  expr_fn <- ptr_define_placeholder_value(
    keyword = "ppExpr",
    build_ui = ptr_builtin_expr_build_ui,
    resolve_expr = ptr_builtin_expr_resolve_expr,
    default_arg = ptr_default_expression(),
    copy_defaults = list(label = "Enter an expression for {param}")
  )
  var_fn <- ptr_define_placeholder_consumer(
    keyword = "ppVar",
    build_ui = ptr_builtin_var_build_ui,
    resolve_expr = ptr_builtin_var_resolve_expr,
    validate_input = ptr_builtin_var_validate_input,
    default_arg = ptr_default_symbol_or_string(),
    copy_defaults = list(
      label = "Pick a column for {param}",
      empty_text = "Choose one column"
    )
  )
  upload_fn <- ptr_define_placeholder_source(
    keyword = "ppUpload",
    build_ui = ptr_builtin_upload_build_ui,
    resolve_data = ptr_builtin_upload_resolve_data,
    companion_id_fn = ptr_upload_name_id,
    default_arg = ptr_default_symbol_or_string(),
    copy_defaults = list(label = "Upload data for {param}")
  )
  # ADR 0020 structural keywords. These never become `ptr_ph_*` nodes;
  # the translator's special-unwrap branches reshape the tree before
  # placeholder-arg extraction runs. The registry entry is the minimum
  # needed for `placeholder_keyword()` / `is_placeholder_call()` (which
  # both walk `ptr_registry_keywords()`) to recognise the names.
  ptr_register_structural_keyword("ppLayerOff")
  ptr_register_structural_keyword("ppVerbOff")
  # The plain-R callables that users see (`ppVar`, `ppNum`, ...) are the
  # top-level functions below carrying `@export` roxygen tags. The
  # registry entries above carry the same identity/guard semantics via
  # `runtime_fn` for parity with custom placeholders (which receive
  # their runtime from the define helper). We do not overwrite the
  # package-namespace bindings here — once the package is loaded, those
  # bindings are locked.
  invisible(list(
    ppText = text_fn, ppNum = num_fn, ppExpr = expr_fn,
    ppVar = var_fn, ppUpload = upload_fn
  ))
}

#' @rdname pp_placeholders
#' @export
ppVar <- function(x = NULL, ...) x

#' @rdname pp_placeholders
#' @export
ppNum <- function(x = NULL, ...) x

#' @rdname pp_placeholders
#' @export
ppText <- function(x = NULL, ...) x

#' @rdname pp_placeholders
#' @export
ppExpr <- function(x = NULL, ...) x

#' @rdname pp_placeholders
#' @export
ppUpload <- function(x, ...) {
  if (missing(x)) {
    rlang::abort("`ppUpload()` is only meaningful inside `ptr_app()`.")
  }
  x
}

#' Off-by-default layer / pipeline-stage wrappers
#'
#' Two ADR-0020 *structural* keywords that mark a layer or a pipeline
#' stage as off-by-default in `ptr_app()`. Inside `ptr_app()` /
#' `ptr_server()` the parser sees the wrapper and unwraps it at translate
#' time, stamping the boot-state metadata on the resulting node:
#' `ppLayerOff(layer_expr, hide = TRUE)` becomes a `ptr_layer` with
#' `default_active = FALSE`; `ppVerbOff(.data, verb_expr, hide = TRUE)`
#' becomes the inner verb call with `default_stage_enabled = FALSE`. The
#' wrapper itself never appears in the typed tree.
#'
#' Outside `ptr_app()` they behave per their R semantics so naked-ggplot
#' scripts still render:
#' `ppLayerOff(geom_point(), TRUE)` returns `NULL` (so
#' `ggplot(mtcars, aes(x = mpg, y = wt)) + ppLayerOff(geom_point(), TRUE)`
#' renders without the hidden layer); `ppLayerOff(geom_point(), FALSE)`
#' returns the layer. `ppVerbOff(.data, mutate(x = 1), TRUE)` returns
#' `.data` unchanged; `ppVerbOff(.data, mutate(x = 1), FALSE)` routes
#' `.data` through the verb call.
#'
#' @section Data-argument position (`ppVerbOff` only):
#' `ppVerbOff(.data, verb_expr, hide = TRUE)` assumes the data must be
#' inserted as the **first positional argument** of `verb_expr` when
#' `hide = FALSE`. This matches the tidyverse convention and the
#' translator's pipeline-stage handling; non-tidyverse verbs that take
#' data in a later argument are not supported via `ppVerbOff` (use a
#' lambda stage or a named wrapper instead). See
#' [ADR 0020](../articles/adr/0020-pp-layer-off-pp-verb-off-toggles.html)
#' Risks §3.
#'
#' @param layer_expr A ggplot2 layer expression (e.g. `geom_point()`,
#'   `facet_wrap(~ cyl)`). Evaluated only when `hide = FALSE`.
#' @param .data A data frame or pipe-supplied dataset (the implicit
#'   `.data` slot when used as a pipeline stage).
#' @param verb_expr A data-pipeline verb call (e.g. `mutate(mpg = mpg +
#'   100)`, `filter(cyl == 6)`). Evaluated with `.data` inserted as the
#'   first positional argument only when `hide = FALSE`.
#' @param hide A length-1 logical literal (`TRUE` or `FALSE`). In
#'   `ptr_app()` formulas this MUST be a literal — the translator aborts
#'   on a non-literal so the formula remains the single source of truth
#'   for the app's boot state. Defaults to `TRUE`.
#'
#' @return Outside `ptr_app()`: `ppLayerOff` returns `NULL` when
#'   `hide = TRUE`, otherwise the evaluated `layer_expr`. `ppVerbOff`
#'   returns `.data` unchanged when `hide = TRUE`, otherwise the result
#'   of `verb_expr` applied to `.data`.
#'
#' @examples
#' library(ggplot2)
#' # Naked-R semantics: hide = TRUE drops the layer to NULL.
#' p1 <- ggplot(mtcars, aes(x = mpg, y = wt)) +
#'   ppLayerOff(geom_point(), TRUE)        # equivalent to no geom_point
#' p2 <- ggplot(mtcars, aes(x = mpg, y = wt)) +
#'   ppLayerOff(geom_point(), FALSE)       # the layer is added
#'
#' # Naked-R semantics: hide = TRUE leaves the data unchanged.
#' identical(ppVerbOff(mtcars, dplyr::mutate(mpg = mpg + 100), TRUE), mtcars)
#'
#' # Inside ptr_app(), the wrapper becomes a node-level default and a
#' # boot-state-off checkbox (Plan 02 wires the UI):
#' # ptr_app("ggplot() + ppLayerOff(geom_point(aes(x = mpg, y = wt)), TRUE)")
#' @name pp_off_toggles
NULL

#' @rdname pp_off_toggles
#' @export
ppLayerOff <- function(layer_expr, hide = TRUE) {
  assertthat::assert_that(
    is.logical(hide), length(hide) == 1L, !is.na(hide),
    msg = "`hide` must be a length-1 non-NA logical."
  )
  if (isTRUE(hide)) return(NULL)
  layer_expr
}

#' @rdname pp_off_toggles
#' @export
ppVerbOff <- function(.data, verb_expr, hide = TRUE) {
  assertthat::assert_that(
    is.logical(hide), length(hide) == 1L, !is.na(hide),
    msg = "`hide` must be a length-1 non-NA logical."
  )
  if (isTRUE(hide)) return(.data)
  # Insert `.data` as the first positional argument of `verb_expr` and
  # evaluate in the caller's frame. Matches the translator's pipeline-
  # stage handling: tidyverse verbs take data in slot 1.
  call_expr <- substitute(verb_expr)
  if (!is.call(call_expr)) {
    rlang::abort(
      "`ppVerbOff(verb_expr = )` must be a verb call, e.g. `mutate(x = 1)`."
    )
  }
  data_sym <- substitute(.data)
  new_call <- as.call(c(
    list(call_expr[[1L]]), list(data_sym), as.list(call_expr[-1L])
  ))
  eval(new_call, envir = parent.frame())
}

.onLoad <- function(libname, pkgname) {
  ptr_register_builtins()
  ptr_register_constant_fold_builtins()
}

`%||%` <- function(a, b) if (is.null(a)) b else a
