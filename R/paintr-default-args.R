#' Argument validators for placeholder definitions (`ptr_arg_*`)
#'
#' These helpers are factories that return a closure of shape
#' `function(arg_expr) -> canonical_value | abort()`. The closure validates
#' the unevaluated R expression captured as a placeholder's positional default
#' argument and returns a canonical value, or aborts with a clear message.
#'
#' The validators operate on AST only: they do not call `eval()`, `parse()`,
#' or any deparse-and-reparse cycle on their input. The numeric helper
#' `ptr_arg_numeric()` (scalar by default, vector via `vector = TRUE`) walks
#' the AST
#' against the constant-fold allowlist registry (see
#' [ptr_register_constant_fold()]) and then evaluate in a sealed environment
#' whose only bindings are the registered names.
#'
#' Symbol policy is per-helper:
#'
#' * `ptr_arg_symbol_or_string()` accepts a bareword symbol (returned as
#'   its character name, preserving non-syntactic / backticked names) or any
#'   single string literal (including the empty string).
#' * `ptr_arg_symbol()` accepts only a bareword symbol (returned as its
#'   character name); rejects string literals, numbers, and compound calls.
#' * `ptr_arg_string()` accepts only a single string literal (including
#'   the empty string); rejects symbols and numbers.
#' * `ptr_arg_numeric()` accepts any AST whose every node is a syntactic
#'   literal or a registered constant-fold name; in the default scalar mode
#'   the result must be a length-one non-NA numeric.
#'   For the vector form use `ptr_arg_numeric(vector = TRUE, length = NULL)`.
#' * `ptr_arg_expression()` is a verbatim store: it returns its input
#'   unchanged so it can later be evaluated in the data context. As a
#'   convenience it emits a one-shot warning if the user wraps the
#'   expression in `quote()`, `bquote()`, `rlang::ppExpr()`, or `rlang::quo()`
#'   (the wrapper is stored verbatim).
#'
#' Each of `ptr_arg_symbol_or_string()`, `ptr_arg_symbol()`,
#' `ptr_arg_string()`, and `ptr_arg_numeric()` takes a `vector` flag. With
#' `vector = FALSE` (the default) the validator parses a single scalar element
#' and returns a length-one value. With `vector = TRUE` it parses a `c(...)`
#' literal element-by-element (each element subject to the helper's scalar
#' element rule) and returns the whole vector; a lone element is treated as a
#' length-one vector. For `ptr_arg_numeric(vector = TRUE)` the optional
#' `length` check (honored only in vector mode) asserts the parsed vector's
#' length.
#'
#' @param vector Logical scalar (default `FALSE`). When `TRUE`, the validator
#'   parses a `c(...)` literal element-by-element and returns the whole vector
#'   instead of a single scalar.
#' @param length Optional integer length required of the resulting numeric
#'   vector; honored only when `vector = TRUE`. `NULL` (the default) imposes no
#'   length check.
#' @return A closure that takes an unevaluated expression and returns the
#'   canonical default value, or aborts.
#' @name ptr_arg_validators
#' @examples
#' is_symbol_ok <- ptr_arg_symbol_or_string()
#' is_symbol_ok(quote(mpg))
#' is_symbol_ok("mpg")
#'
#' is_num <- ptr_arg_numeric()
#' is_num(5)
#' is_num(quote(2 * pi))
#'
#' is_vec <- ptr_arg_numeric(vector = TRUE, length = 2L)
#' is_vec(quote(c(0, 1)))
NULL

# ---- Constant-fold allowlist registry --------------------------------------

# Internal storage; parent = baseenv() so `pi`, `Inf`, `NaN`, etc. resolve
# via the parent without explicit registration.
#
# IMPORTANT: callers must reach this env via `ptr_constant_fold_env_get()`,
# NOT by lexical reference to `ptr_constant_fold_env`. Under
# `devtools::load_all()`, exported functions have their closure rebound to
# `package:ggpaintr` (the attach env), which shadows the namespace binding
# with an empty placeholder of the same name. Lexical lookup from an exported
# function then finds the empty shadow, while `.onLoad` has populated the
# namespace copy -- the two diverge and every numeric-fold helper sees an
# empty registry. The accessor below pins lookup to the namespace, so there
# is only ever one env regardless of how the package is loaded.
ptr_constant_fold_env <- new.env(parent = baseenv())

ptr_constant_fold_env_get <- function() {
  get("ptr_constant_fold_env", envir = asNamespace("ggpaintr"))
}

#' Constant-fold allowlist registry
#'
#' The numeric default-argument validator [ptr_arg_numeric()] (scalar by
#' default, vector via `vector = TRUE`) walks the placeholder's
#' default-argument
#' AST against an allowlist of function and constant names. Authors can
#' extend the allowlist with [ptr_register_constant_fold()] when their
#' placeholder definitions need additional pure operators.
#'
#' Built-in entries seeded at package load:
#'
#' * Arithmetic: `-`, `+`, `*`, `/`, `^`, `%%`, `%/%`
#' * Sequence constructors: `:`, `c`, `seq`, `seq.int`, `seq_len`, `seq_along`
#'
#' Syntactic literals (`TRUE`, `FALSE`, `NA`, `NA_integer_`, `NA_real_`,
#' `Inf`, `NaN`) are recognised by the walker directly and never need
#' registration. `pi` resolves through the registry's parent
#' (`baseenv()`).
#'
#' @param name Character scalar function or constant name.
#' @param value Function or numeric constant to bind under `name`.
#' @return [ptr_register_constant_fold()] and [ptr_clear_constant_fold()]
#'   return `invisible(NULL)`. [ptr_constant_fold_keywords()] returns a
#'   character vector of currently registered names.
#' @name ptr_constant_fold_registry
#' @examples
#' ptr_register_constant_fold("log10", log10)
#' ptr_arg_numeric()(quote(log10(100)))
#' ptr_clear_constant_fold("log10")
NULL

#' @rdname ptr_constant_fold_registry
#' @export
ptr_register_constant_fold <- function(name, value) {
  if (!rlang::is_string(name) || !nzchar(name)) {
    rlang::abort("`name` must be a non-empty character scalar.")
  }
  assign(name, value, envir = ptr_constant_fold_env_get())
  invisible(NULL)
}

#' @rdname ptr_constant_fold_registry
#' @export
ptr_clear_constant_fold <- function(name = NULL) {
  env <- ptr_constant_fold_env_get()
  if (is.null(name)) {
    rm(list = ls(envir = env, all.names = TRUE), envir = env)
    ptr_register_constant_fold_builtins()
    return(invisible(NULL))
  }
  if (!rlang::is_string(name) || !nzchar(name)) {
    rlang::abort("`name` must be a non-empty character scalar or NULL.")
  }
  if (exists(name, envir = env, inherits = FALSE)) {
    rm(list = name, envir = env)
  }
  invisible(NULL)
}

#' @rdname ptr_constant_fold_registry
#' @export
ptr_constant_fold_keywords <- function() {
  sort(ls(envir = ptr_constant_fold_env_get(), all.names = TRUE))
}

# Seeded built-ins; called from .onLoad() (paintr-builtins.R) and from
# ptr_clear_constant_fold(NULL) to repopulate after a wipe.
ptr_register_constant_fold_builtins <- function() {
  env <- ptr_constant_fold_env_get()
  arith <- c("-", "+", "*", "/", "^", "%%", "%/%")
  seq_ops <- c(":", "c", "seq", "seq.int", "seq_len", "seq_along")
  for (nm in c(arith, seq_ops)) {
    assign(nm, get(nm, envir = baseenv()), envir = env)
  }
  invisible(NULL)
}

# ---- AST walker + sealed-env eval ------------------------------------------

validate_constant_ast <- function(expr) {
  if (rlang::is_syntactic_literal(expr)) return(invisible())
  # Already-evaluated atomic values (e.g. the literal `-5` arrives as a bare
  # double, not the call `-(5)`) are accepted as-is. `is_syntactic_literal()`
  # is intentionally strict (rejects negatives, NA_*_, scientific notation
  # parsed as double), so we widen here to any length-1 atomic.
  if (rlang::is_scalar_atomic(expr)) return(invisible())
  env <- ptr_constant_fold_env_get()
  if (rlang::is_symbol(expr)) {
    name <- rlang::as_string(expr)
    # inherits = TRUE so `pi`, `Inf`, etc. resolve via baseenv() parent.
    if (exists(name, envir = env, inherits = TRUE)) {
      return(invisible())
    }
    rlang::abort(sprintf(
      "symbol `%s` not in constant-fold allowlist", name
    ))
  }
  if (rlang::is_call(expr)) {
    head <- expr[[1L]]
    if (!rlang::is_symbol(head)) {
      rlang::abort("non-symbol call head not allowed in constant expression")
    }
    name <- rlang::as_string(head)
    if (!exists(name, envir = env, inherits = FALSE)) {
      rlang::abort(sprintf(
        "function `%s` not in constant-fold allowlist", name
      ))
    }
    n <- length(expr)
    if (n >= 2L) {
      for (i in seq.int(2L, n)) {
        child <- expr[[i]]
        if (!rlang::is_missing(child)) validate_constant_ast(child)
      }
    }
    return(invisible())
  }
  rlang::abort(sprintf(
    "unsupported AST node type: %s", typeof(expr)
  ))
}

ptr_constant_fold <- function(expr) {
  validate_constant_ast(expr)
  rlang::eval_bare(expr, ptr_constant_fold_env_get())
}

# ---- Validator factories ---------------------------------------------------

# Split a default-argument AST into its element ASTs for vector-mode parsing.
# A `c(...)` call yields its argument ASTs; any other expression is treated as a
# single element. Used by the symbol/string vector-capable validators, which
# parse each element with a per-element rule rather than constant-folding.
ptr_arg_vector_elements <- function(arg_expr) {
  if (rlang::is_call(arg_expr, "c")) {
    n <- length(arg_expr)
    if (n < 2L) return(list())
    lapply(seq.int(2L, n), function(i) arg_expr[[i]])
  } else {
    list(arg_expr)
  }
}

#' @rdname ptr_arg_validators
#' @export
ptr_arg_symbol_or_string <- function(vector = FALSE) {
  element <- function(el) {
    if (rlang::is_symbol(el)) {
      return(rlang::as_string(el))
    }
    if (rlang::is_string(el)) {
      return(el)
    }
    rlang::abort(
      "Default must be a bareword column name or a single string literal."
    )
  }
  function(arg_expr) {
    if (!isTRUE(vector)) {
      return(element(arg_expr))
    }
    vapply(ptr_arg_vector_elements(arg_expr), element, character(1L))
  }
}

#' @rdname ptr_arg_validators
#' @export
ptr_arg_string <- function(vector = FALSE) {
  element <- function(el) {
    if (rlang::is_string(el)) {
      return(el)
    }
    rlang::abort("Default must be a single string literal.")
  }
  function(arg_expr) {
    if (!isTRUE(vector)) {
      return(element(arg_expr))
    }
    vapply(ptr_arg_vector_elements(arg_expr), element, character(1L))
  }
}

#' @rdname ptr_arg_validators
#' @export
ptr_arg_symbol <- function(vector = FALSE) {
  element <- function(el) {
    if (rlang::is_symbol(el)) {
      return(rlang::as_string(el))
    }
    rlang::abort("Default must be a bareword column name.")
  }
  function(arg_expr) {
    if (!isTRUE(vector)) {
      return(element(arg_expr))
    }
    vapply(ptr_arg_vector_elements(arg_expr), element, character(1L))
  }
}

#' @rdname ptr_arg_validators
#' @export
ptr_arg_numeric <- function(vector = FALSE, length = NULL) {
  expected_length <- NULL
  if (isTRUE(vector) && !is.null(length)) {
    if (!is.numeric(length) || base::length(length) != 1L ||
        is.na(length) || length < 0L) {
      rlang::abort("`length` must be a non-negative scalar integer or NULL.")
    }
    expected_length <- as.integer(length)
  }
  function(arg_expr) {
    val <- ptr_constant_fold(arg_expr)
    if (!isTRUE(vector)) {
      if (!is.numeric(val) || base::length(val) != 1L || is.na(val)) {
        rlang::abort("Default must be a single numeric literal.")
      }
      return(val)
    }
    if (!is.numeric(val) || anyNA(val)) {
      rlang::abort("Default must be a numeric vector of non-NA values.")
    }
    if (!is.null(expected_length) && base::length(val) != expected_length) {
      rlang::abort(sprintf(
        "Default must be a numeric vector of length %d.", expected_length
      ))
    }
    val
  }
}

# Wrapper-call heads that trigger the A5.b convenience warning.
.ptr_expr_wrapper_names <- c("quote", "bquote", "expr", "quo")

#' @rdname ptr_arg_validators
#' @export
ptr_arg_expression <- function() {
  function(arg_expr) {
    if (rlang::is_call(arg_expr)) {
      head <- arg_expr[[1L]]
      wname <- NULL
      if (rlang::is_symbol(head)) {
        wname <- rlang::as_string(head)
      } else if (rlang::is_call(head, "::")) {
        # rlang::expr / rlang::quo
        ns <- tryCatch(rlang::as_string(head[[2L]]),
                       error = function(e) NA_character_)
        fn <- tryCatch(rlang::as_string(head[[3L]]),
                       error = function(e) NA_character_)
        if (identical(ns, "rlang") && !is.na(fn)) wname <- fn
      }
      if (!is.null(wname) && wname %in% .ptr_expr_wrapper_names) {
        cli::cli_warn(c(
          "{.fn ppExpr} already captures its argument unevaluated.",
          "i" = "You do not need to wrap it in {.fn quote} / {.fn rlang::expr}.",
          "i" = "Pre-quoted expression stored verbatim - textarea shows the wrapper."
        ))
      }
    }
    arg_expr
  }
}
