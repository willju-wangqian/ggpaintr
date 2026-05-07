# P10 — render-code. Typed tree -> code text. Pipe surface preserved via
# `ptr_pipeline$op`; comments are gone (parser stripped them in P1);
# `ptr_user_expr` renders inner verbatim; `pkg::fn` and `pkg:::fn` heads
# preserved by sourcing from the layer/call's underlying language head.

ptr_render <- function(node) {
  render_walk(node)
}

render_walk <- function(node) UseMethod("render_walk")

#' @export
render_walk.ptr_root <- function(node) {
  if (length(node$layers) == 0L) return("")
  parts <- vapply(node$layers, render_walk, character(1))
  paste(parts, collapse = " +\n  ")
}

#' @export
render_walk.ptr_layer <- function(node) {
  head_text <- layer_head_text(node)
  is_piped <- layer_is_piped(node)
  child_args <- render_arg_list(node$children)
  if (is_piped) {
    op <- layer_pipe_op(node)
    if (is.null(node$data_arg)) {
      return(paste0(head_text, "(", child_args, ")"))
    }
    upstream_text <- render_walk(node$data_arg)
    return(paste0(
      upstream_text, " ", op, " ", head_text, "(", child_args, ")"
    ))
  }
  if (is.null(node$data_arg)) {
    return(paste0(head_text, "(", child_args, ")"))
  }
  data_text <- render_walk(node$data_arg)
  if (nzchar(child_args)) {
    paste0(head_text, "(", data_text, ", ", child_args, ")")
  } else {
    paste0(head_text, "(", data_text, ")")
  }
}

#' @export
render_walk.ptr_pipeline <- function(node) {
  render_pipeline_body(node)
}

render_pipeline_body <- function(node) {
  if (length(node$stages) == 0L) return("")
  parts <- vapply(node$stages, render_walk, character(1))
  paste(parts, collapse = paste0(" ", node$op, " "))
}

#' @export
render_walk.ptr_call <- function(node) {
  if (is.symbol(node$fun)) {
    name <- as.character(node$fun)
    arg_names <- names(node$args) %||% rep_len("", length(node$args))
    all_unnamed <- !any(nzchar(arg_names))
    if (all_unnamed && length(node$args) == 2L && is_binary_infix_op(name)) {
      lhs <- render_binary_operand(node$args[[1]])
      rhs <- render_binary_operand(node$args[[2]])
      return(paste0(lhs, " ", name, " ", rhs))
    }
    if (all_unnamed && length(node$args) == 1L &&
        name %in% c("-", "+", "!")) {
      return(paste0(name, render_binary_operand(node$args[[1]])))
    }
  }
  head_text <- call_head_text(node$fun)
  args_text <- render_arg_list(node$args)
  paste0(head_text, "(", args_text, ")")
}

# Built-in binary infix operators plus any user-defined %xxx% form.
is_binary_infix_op <- function(name) {
  if (name %in% c("+", "-", "*", "/", "^", "%%", "%/%",
                  "==", "!=", "<", ">", "<=", ">=",
                  "&", "|", "&&", "||",
                  ":", "%in%")) return(TRUE)
  grepl("^%[^%]*%$", name)
}

# Conservative parenthesisation: wrap an operand if it is itself a binary
# infix call. Over-parenthesises in some cases (e.g. `(a + b) + c`) but is
# always semantically correct without a precedence table.
render_binary_operand <- function(arg) {
  txt <- render_walk(arg)
  if (inherits(arg, "ptr_call") && is.symbol(arg$fun) &&
      is_binary_infix_op(as.character(arg$fun))) {
    return(paste0("(", txt, ")"))
  }
  txt
}

render_arg_list <- function(args) {
  if (length(args) == 0L) return("")
  arg_names <- names(args) %||% rep_len("", length(args))
  parts <- character(length(args))
  for (i in seq_along(args)) {
    txt <- render_walk(args[[i]])
    if (nzchar(arg_names[i])) {
      parts[i] <- paste0(arg_names[i], " = ", txt)
    } else {
      parts[i] <- txt
    }
  }
  paste(parts, collapse = ", ")
}

#' @export
render_walk.ptr_literal <- function(node) {
  rlang::expr_text(node$expr)
}

#' @export
render_walk.ptr_user_expr <- function(node) {
  rlang::expr_text(node$inner)
}

#' @export
render_walk.ptr_missing <- function(node) ""

#' @export
render_walk.ptr_ph_value <- function(node) rlang::expr_text(node$expr)

#' @export
render_walk.ptr_ph_data_consumer <- function(node) rlang::expr_text(node$expr)

#' @export
render_walk.ptr_ph_data_source <- function(node) rlang::expr_text(node$expr)

#' @export
render_walk.default <- function(node) {
  if (is_ptr_node(node)) return("")
  rlang::expr_text(node)
}

# ---- helpers ---------------------------------------------------------------

# Layer was piped if its underlying expression's outermost head is a pipe op.
layer_is_piped <- function(layer) {
  e <- layer$expr
  if (!is.call(e)) return(FALSE)
  head <- e[[1L]]
  is.symbol(head) && as.character(head) %in%
    c("%>%", "%ptrPipeNative%", "|>")
}

# Recover the outermost pipe op of a piped layer. Sentinel maps back to "|>".
layer_pipe_op <- function(layer) {
  e <- layer$expr
  if (is.call(e) && is.symbol(e[[1L]])) {
    s <- as.character(e[[1L]])
    if (s == "%ptrPipeNative%") return("|>")
    if (s %in% c("%>%", "|>")) return(s)
  }
  "|>"
}

# Recover the head text from the underlying language. For piped layers, peel
# pipes to reach the terminal call head; for plain calls, use the call's
# head directly. Falls back to layer$name if expr is unavailable.
layer_head_text <- function(layer) {
  e <- layer$expr
  if (is.call(e)) {
    while (is.call(e) && is.symbol(e[[1L]]) &&
           as.character(e[[1L]]) %in% c("%>%", "%ptrPipeNative%", "|>")) {
      e <- e[[3L]]
    }
    if (is.call(e)) return(call_head_text(e[[1L]]))
  }
  layer$name %||% "<anon>"
}

call_head_text <- function(fun) {
  if (is.symbol(fun)) return(as.character(fun))
  if (is.call(fun)) return(rlang::expr_text(fun))
  rlang::expr_text(fun)
}
