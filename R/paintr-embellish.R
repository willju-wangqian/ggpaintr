#' Built-in helpers for a placeholder's plain-R evaluation slot
#'
#' A placeholder-embellished ggplot expression must stay valid plain R that
#' renders the original plot with no app running. Each placeholder carries a
#' callable (the `embellish_eval =` argument of the
#' `ptr_define_placeholder_*()` constructors) that supplies its plain-R
#' meaning when the naked expression is evaluated outside `ptr_app()`. These
#' two factories are the built-in callables for that slot:
#'
#' * [embellish_identity()] returns the identity `function(x, ...) x` — the
#'   slot's default behaviour. The placeholder call becomes a no-op wrapper:
#'   it returns its argument unchanged.
#' * [embellish_symbol_to_string()] returns a function that captures its
#'   argument *unevaluated* and turns column references into a character
#'   vector of names. This is the pattern a column-selecting consumer needs
#'   so the naked expression works inside a tidyselect verb: tidyselect
#'   evaluates an unknown wrapper call in non-masked scope, where bare column
#'   symbols throw "object not found"; returning the names as strings
#'   sidesteps that because tidyselect accepts selection by name.
#'
#' These helpers are *author-controlled* plain-R semantics, never derived —
#' only the author knows the intended live-R meaning of a placeholder.
#'
#' @return Each factory returns a function of signature `function(x, ...)`.
#'   [embellish_identity()]'s function returns its first argument `x`
#'   unchanged. [embellish_symbol_to_string()]'s function returns a character
#'   vector of column names captured from the unevaluated `x`.
#' @name embellish_helpers
#' @examples
#' f <- embellish_identity()
#' f(5L)
#'
#' g <- embellish_symbol_to_string()
#' g(c(mpg, hp))
#' g(mpg)
NULL

#' @rdname embellish_helpers
#' @export
embellish_identity <- function() {
  function(x, ...) x
}

#' @rdname embellish_helpers
#' @export
embellish_symbol_to_string <- function() {
  function(x, ...) {
    expr <- rlang::enexpr(x)
    embellish_expr_to_names(expr)
  }
}

# Convert a captured column-selection expression into a character vector of
# column names, without forcing any bare symbol. Handles three in-contract
# shapes: a `c(...)` call of symbols/strings, a single bare symbol, a single
# string literal. Anything else falls through to a best-effort eval in the
# caller's frame (explicitly out of contract).
embellish_expr_to_names <- function(expr) {
  if (rlang::is_call(expr, "c")) {
    elements <- rlang::call_args(expr)
    return(unname(purrr::map_chr(elements, embellish_atom_to_name)))
  }
  if (rlang::is_symbol(expr) || is.character(expr)) {
    return(embellish_atom_to_name(expr))
  }
  # Out-of-contract fallback (e.g. starts_with("a"), arithmetic): best-effort,
  # not contract-guaranteed. Evaluate in the caller's frame.
  eval(expr, envir = parent.frame(2L))
}

# A single selection atom -> its column name. A bare symbol becomes its name
# string (never forced); a string literal passes through unchanged.
embellish_atom_to_name <- function(atom) {
  if (rlang::is_symbol(atom)) {
    return(rlang::as_string(atom))
  }
  if (is.character(atom) && length(atom) == 1L) {
    return(atom)
  }
  # Neither symbol nor string: out of contract -> caller's error.
  eval(atom, envir = parent.frame(2L))
}
