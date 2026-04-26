# Registry of public ggpaintr settings.
# Each entry maps a short setting name (used in ptr_options()) to its
# underlying base R `option()` name and default value. Both settings are
# logical scalars, so a single validator suffices.
ptr_settings <- list(
  verbose = list(
    option  = "ggpaintr.verbose",
    default = TRUE
  ),
  checkbox_default_all_other_layer = list(
    option  = "ggpaintr.checkbox_default_all_other_layer",
    default = TRUE
  )
)

#' Get or Set ggpaintr Package Options
#'
#' Combined getter/setter for ggpaintr's global settings, modeled after base
#' [base::options()]. Calling with no arguments returns all current settings
#' as a named list. Passing one or more named logical arguments sets those
#' settings and invisibly returns the previous values, suitable for the
#' `do.call(ptr_options, old)` round-trip pattern used by
#' `withr::with_options()` and `on.exit()`.
#'
#' @section Available settings:
#'
#' \describe{
#'   \item{`verbose`}{
#'     Logical. When `TRUE` (default), ggpaintr emits informative messages
#'     such as "Layer foo() removed (no arguments provided)." Set to `FALSE`
#'     to silence these messages. Underlying option:
#'     `options(ggpaintr.verbose = ...)`.
#'   }
#'   \item{`checkbox_default_all_other_layer`}{
#'     Logical. The fallback initial state for layer checkboxes that aren't
#'     explicitly named in a call's `checkbox_defaults` argument. `TRUE`
#'     (default) starts unspecified layers checked (current behavior); `FALSE`
#'     starts unspecified layers unchecked, so apps with many alternative
#'     layers can opt-in only the ones they want via `checkbox_defaults =`.
#'     The per-call argument always wins over this global fallback.
#'     Underlying option:
#'     `options(ggpaintr.checkbox_default_all_other_layer = ...)`.
#'   }
#' }
#'
#' @param ... Named logical arguments — one per setting to update. Setting
#'   names must match the registry above.
#'
#' @return When called with no arguments, a named list of all current setting
#'   values. When called with named arguments, the previous values of the
#'   updated settings, returned invisibly.
#'
#' @examples
#' # Inspect current values
#' ptr_options()
#'
#' # Silence the "Layer ... removed" notice for one block
#' old <- ptr_options(verbose = FALSE)
#' on.exit(do.call(ptr_options, old), add = TRUE)
#'
#' # Start every app with all layers unchecked unless the call opts them in
#' ptr_options(checkbox_default_all_other_layer = FALSE)
#' @export
ptr_options <- function(...) {
  args <- list(...)

  if (length(args) == 0L) {
    return(stats::setNames(
      lapply(ptr_settings, ptr_get_setting),
      names(ptr_settings)
    ))
  }

  if (is.null(names(args)) || any(!nzchar(names(args)))) {
    rlang::abort("`ptr_options()` arguments must all be named.")
  }

  unknown <- setdiff(names(args), names(ptr_settings))
  if (length(unknown) > 0L) {
    rlang::abort(sprintf(
      "Unknown ggpaintr setting(s): %s. Valid: %s.",
      paste(unknown, collapse = ", "),
      paste(names(ptr_settings), collapse = ", ")
    ))
  }

  for (key in names(args)) {
    ptr_validate_setting_value(key, args[[key]])
  }

  prev <- stats::setNames(
    lapply(names(args), function(key) ptr_get_setting(ptr_settings[[key]])),
    names(args)
  )

  to_set <- stats::setNames(args, vapply(
    names(args),
    function(key) ptr_settings[[key]]$option,
    character(1)
  ))
  do.call(options, to_set)

  invisible(prev)
}

#' @noRd
ptr_get_setting <- function(setting) {
  isTRUE(getOption(setting$option, default = setting$default))
}

#' @noRd
ptr_validate_setting_value <- function(key, value) {
  if (!is.logical(value)) {
    rlang::abort(sprintf(
      "`ptr_options(%s = ...)` must be logical, not %s.",
      key, paste(class(value), collapse = "/")
    ))
  }
  if (length(value) != 1L) {
    rlang::abort(sprintf(
      "`ptr_options(%s = ...)` must be a single TRUE / FALSE value.",
      key
    ))
  }
  if (is.na(value)) {
    rlang::abort(sprintf(
      "`ptr_options(%s = ...)` must not be NA.",
      key
    ))
  }
  invisible(TRUE)
}

#' Read the global "default checkbox state for unspecified layers" option.
#' @noRd
ptr_default_layer_state <- function() {
  ptr_get_setting(ptr_settings$checkbox_default_all_other_layer)
}
