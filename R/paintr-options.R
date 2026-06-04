# Registry of public ggpaintr settings.
# Each entry maps a short setting name (used in ptr_options()) to its
# underlying base R `option()` name and default value. The single registered
# setting is a logical scalar, so a single validator suffices.
ptr_settings <- list(
  verbose = list(
    option  = "ggpaintr.verbose",
    default = FALSE
  ),
  gate_draw = list(
    option  = "ggpaintr.gate_draw",
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
#'     Logical. When `TRUE`, ggpaintr emits informative messages such as
#'     "Layer foo() removed (no arguments provided)." Default `FALSE` —
#'     these messages are intended for debugging the formula pipeline and
#'     are off by default. Underlying option: `options(ggpaintr.verbose = ...)`.
#'   }
#'   \item{`gate_draw`}{
#'     Logical. When `TRUE` (the default), the rendered plot updates only
#'     when the user clicks the "Update plot" button — every placeholder
#'     change is batched until the click. When `FALSE`, the button is
#'     omitted from the UI and the plot re-renders reactively on every
#'     placeholder change (live mode). Read once when the app is built, so
#'     set it before calling [ptr_app()] / [ptr_ui()]. Underlying option:
#'     `options(ggpaintr.gate_draw = ...)`.
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
