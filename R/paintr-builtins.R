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
  # verbatim; a language default (from `ptr_arg_expression`) is
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
  # Empty / non-character / mid-typing values are transient by definition --
  # the user is between keystrokes or just opened the widget. Signal
  # `ptr_partial_input` (see [ptr_signal_partial()]) so live picker
  # entry_reactives stay silent; the Update/Draw plot path still surfaces
  # this as a normal inline error.
  # `is.na(value)` must be checked before `nzchar(value)` because
  # `nzchar(NA_character_)` returns `NA`, which would make the `||`
  # chain evaluate to `NA` and crash with "missing value where TRUE/FALSE
  # needed". The length check guards `is.na()` against vector input.
  if (!is.character(value) || length(value) != 1L ||
      is.na(value) || !nzchar(value)) {
    ptr_signal_partial(paste0(
      "An `expr` input must contain a single R expression written as text ",
      "(for example: `factor(cyl)`)."
    ))
  }
  exprs <- tryCatch(
    rlang::parse_exprs(value),
    error = function(e) {
      ptr_signal_partial(paste0(
        "expr placeholder: could not parse input as R expression: ",
        conditionMessage(e)
      ))
    }
  )
  if (length(exprs) != 1L) {
    ptr_signal_partial(paste0(
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

# ADR 0025 item #7: the source hook now returns ONLY the source widget
# (the fileInput). The shortcut textInput is emitted as STATIC UI by
# `build_ui_for.ptr_ph_data_source()` (R/paintr-build-ui.R) so it survives
# the source uiOutput's rising-edge re-render (which clears a stale file
# pill once a typed shortcut takes over). `name_copy` is retained as a
# formal only for backward-compatible call sites that still pass it; it is
# no longer consumed here.
ptr_builtin_upload_build_ui <- function(node, label = NULL, copy = NULL,
                                        file_copy = NULL, name_copy = NULL,
                                        ...) {
  attach_help(
    shiny::fileInput(
      inputId = node$id,
      label = file_copy$label %||% label %||% "Choose a data file",
      accept = ptr_upload_accept_formats()
    ),
    file_copy$help
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
    "ppLayerOff", "ppVerbSwitch"
  )
}

#' Placeholder Identity Helpers
#'
#' These are the plain-R callables returned by registering the five
#' built-in ggpaintr placeholders (`ppVar`, `ppNum`, `ppText`, `ppExpr`,
#' `ppUpload`). Inside a formula passed to [ptr_app()] / [ptr_server()]
#' the parser recognises calls to these names as placeholder invocations
#' and binds them to Shiny widgets (see `vignette("ggpaintr-tutorial")`).
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
#' # ptr_app(ggplot(mtcars, aes(x = ppVar(mpg), y = ppVar)) + geom_point())
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
  # Remove any prior builtin entries so re-registration is silent. The
  # registry env is anchored in `options()` and survives `load_all` cycles
  # (see comment in `R/paintr-registry.R`), so callers that re-invoke this
  # to "reset to builtins" without first running `ptr_registry_clear()`
  # would otherwise hit a populated env and trigger "Overwriting placeholder"
  # warnings from `ptr_registry_register()`. Custom placeholders are
  # preserved. The keyword list is sourced from `ptr_builtin_keywords()` so
  # downstream renames (e.g. structural-keyword additions) flow in by editing
  # only that one definition.
  for (k in ptr_builtin_keywords()) {
    if (exists(k, envir = .ptr_registry, inherits = FALSE)) {
      rm(list = k, envir = .ptr_registry)
    }
  }
  text_fn <- ptr_define_placeholder_value(
    keyword = "ppText",
    build_ui = ptr_builtin_text_build_ui,
    resolve_expr = ptr_builtin_text_resolve_expr,
    parse_positional_arg = ptr_arg_string(),
    ui_text_defaults = list(
      label = "Enter a value for {param}",
      placeholder = "Plain text - quotes are added automatically"
    )
  )
  num_fn <- ptr_define_placeholder_value(
    keyword = "ppNum",
    build_ui = ptr_builtin_num_build_ui,
    resolve_expr = ptr_builtin_num_resolve_expr,
    parse_positional_arg = ptr_arg_numeric(),
    ui_text_defaults = list(label = "Enter a number for {param}")
  )
  expr_fn <- ptr_define_placeholder_value(
    keyword = "ppExpr",
    build_ui = ptr_builtin_expr_build_ui,
    resolve_expr = ptr_builtin_expr_resolve_expr,
    parse_positional_arg = ptr_arg_expression(),
    ui_text_defaults = list(label = "Enter an expression for {param}")
  )
  var_fn <- ptr_define_placeholder_consumer(
    keyword = "ppVar",
    build_ui = ptr_builtin_var_build_ui,
    resolve_expr = ptr_builtin_var_resolve_expr,
    validate_session_input = ptr_builtin_var_validate_input,
    parse_positional_arg = ptr_arg_symbol_or_string(),
    ui_text_defaults = list(
      label = "Pick a column for {param}",
      empty_text = "Choose one column"
    )
  )
  upload_fn <- ptr_define_placeholder_source(
    keyword = "ppUpload",
    build_ui = ptr_builtin_upload_build_ui,
    resolve_data = ptr_builtin_upload_resolve_data,
    shortcut = TRUE,
    parse_positional_arg = ptr_arg_symbol_or_string(),
    ui_text_defaults = list(label = "Upload data for {param}")
  )
  # ADR 0020 structural keywords. These never become `ptr_ph_*` nodes;
  # the translator's special-unwrap branches reshape the tree before
  # placeholder-arg extraction runs. The registry entry is the minimum
  # needed for `placeholder_keyword()` / `is_placeholder_call()` (which
  # both walk `ptr_registry_keywords()`) to recognise the names.
  ptr_register_structural_keyword("ppLayerOff")
  ptr_register_structural_keyword("ppVerbSwitch")
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

#' Off-by-default layer wrapper
#'
#' ADR-0020 *structural* keyword that marks a layer as off-by-default in
#' `ptr_app()`. Inside `ptr_app()` / `ptr_server()` the parser sees the
#' wrapper and unwraps it at translate time, stamping the boot-state
#' metadata on the resulting node: `ppLayerOff(layer_expr, hide = TRUE)`
#' becomes a `ptr_layer` with `default_active = FALSE`. The wrapper
#' itself never appears in the typed tree.
#'
#' Outside `ptr_app()` it behaves per its R semantics so naked-ggplot
#' scripts still render: `ppLayerOff(geom_point(), TRUE)` returns `NULL`
#' (so `ggplot(mtcars, aes(x = mpg, y = wt)) + ppLayerOff(geom_point(), TRUE)`
#' renders without the hidden layer); `ppLayerOff(geom_point(), FALSE)`
#' returns the layer.
#'
#' For the pipeline-stage sibling that exposes a user-toggleable checkbox
#' (ADR-0021), see [ppVerbSwitch()].
#'
#' @param layer_expr A ggplot2 layer expression (e.g. `geom_point()`,
#'   `facet_wrap(~ cyl)`). Evaluated only when `hide = FALSE`.
#' @param hide A length-1 logical literal (`TRUE` or `FALSE`). In
#'   `ptr_app()` formulas this MUST be a literal — the translator aborts
#'   on a non-literal so the formula remains the single source of truth
#'   for the app's boot state. Defaults to `TRUE`.
#'
#' @return Outside `ptr_app()`: `NULL` when `hide = TRUE`, otherwise the
#'   evaluated `layer_expr`.
#'
#' @examples
#' library(ggplot2)
#' # Naked-R semantics: hide = TRUE drops the layer to NULL.
#' p1 <- ggplot(mtcars, aes(x = mpg, y = wt)) +
#'   ppLayerOff(geom_point(), TRUE)        # equivalent to no geom_point
#' p2 <- ggplot(mtcars, aes(x = mpg, y = wt)) +
#'   ppLayerOff(geom_point(), FALSE)       # the layer is added
#'
#' # Inside ptr_app(), the wrapper becomes a node-level default and a
#' # boot-state-off checkbox:
#' # ptr_app(ggplot() + ppLayerOff(geom_point(aes(x = mpg, y = wt)), TRUE))
#' @export
ppLayerOff <- function(layer_expr, hide = TRUE) {
  assertthat::assert_that(
    is.logical(hide), length(hide) == 1L, !is.na(hide),
    msg = "`hide` must be a length-1 non-NA logical."
  )
  if (isTRUE(hide)) return(NULL)
  layer_expr
}

#' Switchable pipeline-stage wrapper
#'
#' ADR-0021 *structural* keyword that marks a pipeline stage as user-
#' toggleable in `ptr_app()`. The boolean argument is `switch_on`
#' (positive sense: TRUE applies the verb, FALSE skips it) and an optional
#' `label` carries the UI text for the resulting checkbox. Inside
#' `ptr_app()` / `ptr_server()`
#' the parser sees the wrapper and unwraps it at translate time, stamping
#' the boot-state metadata + UI label onto the resulting node. The
#' wrapper itself never appears in the typed tree.
#'
#' Outside `ptr_app()` it behaves per its R semantics so naked-dplyr
#' scripts still render: `ppVerbSwitch(.data, mutate(x = 1), FALSE)`
#' returns `.data` unchanged; `ppVerbSwitch(.data, mutate(x = 1), TRUE)`
#' routes `.data` through the verb call. `label` is metadata-only
#' outside `ptr_app()` (the naked-R path ignores it).
#'
#' @section Data-argument position:
#' `ppVerbSwitch(.data, verb_expr, switch_on = TRUE)` inserts `.data` as
#' the **first positional argument** of `verb_expr` when `switch_on` is
#' TRUE. This matches the tidyverse convention and the translator's
#' pipeline-stage handling; non-tidyverse verbs that take data in a
#' later argument are not supported (use a lambda stage or a named
#' wrapper instead).
#'
#' @param .data A data frame or pipe-supplied dataset (the implicit
#'   `.data` slot when used as a pipeline stage).
#' @param verb_expr A data-pipeline verb call (e.g. `mutate(mpg = mpg +
#'   100)`, `filter(cyl == 6)`). Evaluated with `.data` inserted as the
#'   first positional argument only when `switch_on = TRUE`.
#' @param switch_on A length-1 non-NA logical literal. In `ptr_app()`
#'   formulas this MUST be a literal — the translator aborts on a
#'   non-literal so the formula remains the single source of truth for
#'   the app's boot state. Defaults to `TRUE` (apply the verb).
#' @param label Optional length-1 character used as the checkbox label
#'   inside `ptr_app()`. Ignored by the naked-R path. Defaults to `NULL`.
#'
#' @return Outside `ptr_app()`: returns `.data` unchanged when
#'   `switch_on = FALSE`, otherwise the result of `verb_expr` applied to
#'   `.data`.
#'
#' @examples
#' # Naked-R semantics: switch_on = FALSE leaves the data unchanged.
#' identical(
#'   ppVerbSwitch(mtcars, dplyr::mutate(mpg = mpg + 100), FALSE),
#'   mtcars
#' )
#'
#' # switch_on = TRUE routes .data through the verb.
#' result <- ppVerbSwitch(mtcars, dplyr::filter(mpg > 20), TRUE)
#' nrow(result)  # 14
#'
#' # Inside ptr_app(), the wrapper becomes a node-level default + a
#' # labelled boot-state-on checkbox (plans 03-05 wire the UI):
#' # ptr_app("mtcars |> ppVerbSwitch(filter(mpg > 20), TRUE, label = 'Filter')")
#'
#' @export
ppVerbSwitch <- function(.data, verb_expr, switch_on = TRUE, label = NULL) {
  assertthat::assert_that(
    is.logical(switch_on), length(switch_on) == 1L, !is.na(switch_on),
    msg = "`switch_on` must be a length-1 non-NA logical."
  )
  # Validate the verb-call shape before honoring `switch_on`. The wrapper
  # must reject malformed input regardless of the boot state so misuse
  # surfaces immediately (BDD SC-5: a non-call `verb_expr` errors even
  # when `switch_on = FALSE`).
  call_expr <- substitute(verb_expr)
  if (!is.call(call_expr)) {
    rlang::abort(
      "`ppVerbSwitch(verb_expr = )` must be a verb call, e.g. `mutate(x = 1)`."
    )
  }
  if (!isTRUE(switch_on)) return(.data)
  # Insert `.data` as the first positional argument of `verb_expr` and
  # evaluate in the caller's frame. Matches the translator's pipeline-
  # stage handling: tidyverse verbs take data in slot 1.
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
