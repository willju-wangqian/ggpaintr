# P10 — render-code. Typed tree -> code text. Pipe surface preserved via
# `ptr_pipeline$op`; comments are gone (parser stripped them in P1);
# `ptr_user_expr` renders inner verbatim; `pkg::fn` and `pkg:::fn` heads
# preserved by sourcing from the layer/call's underlying language head.
#
# Calls whose single-line form would run past `RENDER_WIDTH` columns (measured
# from the column the call head starts at) expand their argument list one per
# line, indented two spaces past the call, with the closing paren on its own
# line — the styler/tidyverse convention. Wrapping recurses, so a nested call
# re-wraps at its own deeper indent. `+` layers always sit on their own line
# (legacy behaviour); pipe chains stay inline.

RENDER_WIDTH <- 80L

#' Render a typed tree back to R source code
#'
#' @param node Typed tree root (or any node) to render.
#' @param preserve_placeholders If `FALSE` (default), placeholder nodes are
#'   assumed to have been replaced by literal nodes during substitution and are
#'   rendered as today (`expr_text(node$expr)` as a fallback). If `TRUE`, any
#'   surviving placeholder node is emitted as `ppX(current_pick)` -- the
#'   round-trippable "formula with placeholders" view. The keyword and
#'   `current_pick` are read directly from the node; tests in this layer stub
#'   them by hand.
#' @return Character scalar of R source code.
#' @keywords internal
ptr_render <- function(node, preserve_placeholders = FALSE) {
  render_walk(node, indent = 0L, preserve_placeholders = preserve_placeholders)
}

render_walk <- function(node, indent = 0L, preserve_placeholders = FALSE)
  UseMethod("render_walk")

#' @export
render_walk.ptr_root <- function(node, indent = 0L, preserve_placeholders = FALSE) {
  if (length(node$layers) == 0L) return("")
  parts <- character(length(node$layers))
  for (i in seq_along(node$layers)) {
    # Layer 1 starts in column 0; later layers are pulled two spaces in by the
    # " +\n  " join, so wrap them at indent 2 to keep continuation lines aligned.
    parts[i] <- render_walk(node$layers[[i]], indent = if (i == 1L) 0L else 2L,
                            preserve_placeholders = preserve_placeholders)
  }
  paste(parts, collapse = " +\n  ")
}

#' @export
render_walk.ptr_layer <- function(node, indent = 0L, preserve_placeholders = FALSE) {
  head_text <- layer_head_text(node)
  is_piped <- layer_is_piped(node)
  if (is_piped) {
    terminal <- function(ci) render_call_text(head_text, node$children, ci,
                                              preserve_placeholders = preserve_placeholders)
    if (is.null(node$data_arg)) return(terminal(indent))
    op <- layer_pipe_op(node)
    # Flatten a same-op upstream pipeline so each stage is its own segment
    # (`a |> b() |> c()`); a different-op or non-pipeline upstream stays one
    # opaque segment and wraps via its own render_walk if it is itself long.
    up_segs <- if (inherits(node$data_arg, "ptr_pipeline") &&
                   identical(node$data_arg$op, op)) {
      lapply(node$data_arg$stages, function(s) {
        force(s)
        function(ci) render_walk(s, ci, preserve_placeholders = preserve_placeholders)
      })
    } else {
      list(function(ci) render_walk(node$data_arg, ci,
                                    preserve_placeholders = preserve_placeholders))
    }
    return(render_pipe_chain(c(up_segs, list(terminal)), op, indent))
  }
  if (is.null(node$data_arg)) {
    return(render_call_text(head_text, node$children, indent,
                            preserve_placeholders = preserve_placeholders))
  }
  # Render `data = ...` (named) for the same reason eval emits it named:
  # `geom_*()`/`stat_*()` take `mapping` first, so positional data text is
  # not round-trippable. An empty/missing data slot collapses to nothing
  # (the thunk yields "" and render_call_text drops it).
  data_thunk <- function(ci) {
    dt <- render_walk(node$data_arg, ci + nchar("data = "),
                      preserve_placeholders = preserve_placeholders)
    if (nzchar(dt)) paste0("data = ", dt) else ""
  }
  render_call_text(head_text, node$children, indent, lead_thunks = list(data_thunk),
                   preserve_placeholders = preserve_placeholders)
}

#' @export
render_walk.ptr_pipeline <- function(node, indent = 0L, preserve_placeholders = FALSE) {
  render_pipeline_body(node, indent, preserve_placeholders = preserve_placeholders)
}

render_pipeline_body <- function(node, indent = 0L, preserve_placeholders = FALSE) {
  if (length(node$stages) == 0L) return("")
  thunks <- lapply(node$stages, function(s) {
    force(s)
    function(ci) render_walk(s, ci, preserve_placeholders = preserve_placeholders)
  })
  render_pipe_chain(thunks, node$op, indent)
}

# Lay out a pipe chain. `seg_thunks` is a list of `function(col) -> text`, one
# per segment; `op` is the pipe operator (`|>` or `%>%`). Single-segment chains
# render inline; any multi-stage chain prints one segment per line with the
# operator trailing each line and continuations at `indent + 2`.
render_pipe_chain <- function(seg_thunks, op, indent) {
  if (length(seg_thunks) == 1L) return(seg_thunks[[1]](indent))
  child <- indent + 2L
  pad <- strrep(" ", child)
  parts <- c(seg_thunks[[1]](indent),
             vapply(seg_thunks[-1], function(f) f(child), character(1)))
  paste0(parts[1], paste0(" ", op, "\n", pad, parts[-1], collapse = ""))
}

#' @export
render_walk.ptr_call <- function(node, indent = 0L, preserve_placeholders = FALSE) {
  if (is.symbol(node$fun)) {
    name <- as.character(node$fun)
    arg_names <- names(node$args) %||% rep_len("", length(node$args))
    all_unnamed <- !any(nzchar(arg_names))
    if (all_unnamed && length(node$args) == 2L && is_binary_infix_op(name)) {
      lhs <- render_binary_operand(node$args[[1]], indent, preserve_placeholders)
      rhs <- render_binary_operand(node$args[[2]], indent, preserve_placeholders)
      return(paste0(lhs, " ", name, " ", rhs))
    }
    if (all_unnamed && length(node$args) == 1L &&
        name %in% c("-", "+", "!", "~")) {
      return(paste0(name, render_binary_operand(node$args[[1]], indent, preserve_placeholders)))
    }
    # Bracket / extractor / namespace forms: render `x[i, j]`, `x[[i]]`,
    # `x$y`, `x@y`, `pkg::name` in their syntactic form rather than as
    # `[(x, i, j)` etc. Names on bracket args (e.g. `drop = FALSE`) are
    # preserved.
    if (length(node$args) >= 1L && name %in% c("[", "[[")) {
      lhs <- render_binary_operand(node$args[[1]], indent, preserve_placeholders)
      rest <- node$args[-1L]
      if (length(rest) == 0L) {
        close <- if (name == "[") "]" else "]]"
        return(paste0(lhs, name, close))
      }
      inner_pieces <- render_arg_pieces(rest, indent + nchar(lhs) + nchar(name),
                                        preserve_placeholders)
      close <- if (name == "[") "]" else "]]"
      return(paste0(lhs, name, paste(inner_pieces, collapse = ", "), close))
    }
    if (all_unnamed && length(node$args) == 2L &&
        name %in% c("$", "@", "::", ":::")) {
      lhs <- render_binary_operand(node$args[[1]], indent, preserve_placeholders)
      rhs_node <- node$args[[2]]
      rhs <- if (is.symbol(rhs_node)) as.character(rhs_node)
             else render_walk(rhs_node, indent, preserve_placeholders = preserve_placeholders)
      return(paste0(lhs, name, rhs))
    }
  }
  head_text <- call_head_text(node$fun)
  render_call_text(head_text, node$args, indent,
                   preserve_placeholders = preserve_placeholders)
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
render_binary_operand <- function(arg, indent = 0L, preserve_placeholders = FALSE) {
  txt <- render_walk(arg, indent, preserve_placeholders = preserve_placeholders)
  if (inherits(arg, "ptr_call") && is.symbol(arg$fun) &&
      is_binary_infix_op(as.character(arg$fun))) {
    return(paste0("(", txt, ")"))
  }
  txt
}

# Render `head(arg1, arg2, ...)`, choosing inline vs one-per-line layout.
# `args` is a pairlist/list of arguments. `lead_thunks` is a list of
# `function(col) -> "name = value"` producing leading arguments (e.g. the
# re-named `data = ...` slot on a layer) rendered at the column they land at;
# a thunk that yields "" is dropped. `indent` is the column the head starts
# at; when the call is expanded, arguments land at `indent + 2`.
render_call_text <- function(head_text, args, indent, lead_thunks = list(),
                             preserve_placeholders = FALSE) {
  arg_pieces <- function(ci) {
    lead <- vapply(lead_thunks, function(f) f(ci), character(1))
    pieces <- c(lead, render_arg_pieces(args, ci, preserve_placeholders))
    pieces[nzchar(pieces)]
  }
  inline_pieces <- arg_pieces(indent + nchar(head_text, type = "bytes") + 1L)
  one_line <- paste0(head_text, "(", paste(inline_pieces, collapse = ", "), ")")
  fits <- !grepl("\n", one_line, fixed = TRUE) &&
    indent + nchar(one_line) <= RENDER_WIDTH
  if (length(inline_pieces) == 0L || fits) return(one_line)
  child_indent <- indent + 2L
  pad <- strrep(" ", child_indent)
  body <- paste0(pad, arg_pieces(child_indent), collapse = ",\n")
  paste0(head_text, "(\n", body, "\n", strrep(" ", indent), ")")
}

# One formatted string per argument, each carrying its `name = ` prefix where
# named. Each is rendered as if its value started in column `indent` (plus the
# width of any `name = ` prefix), so nested calls wrap against the right margin.
render_arg_pieces <- function(args, indent, preserve_placeholders = FALSE) {
  if (length(args) == 0L) return(character(0))
  arg_names <- names(args) %||% rep_len("", length(args))
  pieces <- character(length(args))
  for (i in seq_along(args)) {
    prefix <- if (nzchar(arg_names[i])) paste0(arg_names[i], " = ") else ""
    pieces[i] <- paste0(prefix, render_walk(args[[i]], indent + nchar(prefix),
                                            preserve_placeholders = preserve_placeholders))
  }
  pieces
}

#' @export
render_walk.ptr_literal <- function(node, indent = 0L, preserve_placeholders = FALSE) {
  rlang::expr_text(node$expr)
}

#' @export
render_walk.ptr_closure <- function(node, indent = 0L, preserve_placeholders = FALSE) {
  rlang::expr_text(call("function", node$formals, node_to_lang(node$body)))
}

#' @export
render_walk.ptr_user_expr <- function(node, indent = 0L, preserve_placeholders = FALSE) {
  rlang::expr_text(node$inner)
}

#' @export
render_walk.ptr_missing <- function(node, indent = 0L, preserve_placeholders = FALSE) ""

#' @export
render_walk.ptr_ph_value <- function(node, indent = 0L, preserve_placeholders = FALSE) {
  if (isTRUE(preserve_placeholders)) {
    return(render_placeholder_preserved(node))
  }
  rlang::expr_text(node$expr)
}

#' @export
render_walk.ptr_ph_data_consumer <- function(node, indent = 0L, preserve_placeholders = FALSE) {
  if (isTRUE(preserve_placeholders)) {
    return(render_placeholder_preserved(node))
  }
  rlang::expr_text(node$expr)
}

#' @export
render_walk.ptr_ph_data_source <- function(node, indent = 0L, preserve_placeholders = FALSE) {
  if (isTRUE(preserve_placeholders)) {
    return(render_placeholder_preserved(node))
  }
  rlang::expr_text(node$expr)
}

#' @export
render_walk.default <- function(node, indent = 0L, preserve_placeholders = FALSE) {
  if (is_ptr_node(node)) return("")
  rlang::expr_text(node)
}

# ---- preserve-mode placeholder rendering -----------------------------------

# Emit `ppX(current_pick)` (or `ppX(current_pick, shared = "k")`) for a
# placeholder node whose `current_pick` field has been set by an upstream pass
# (PLAN-07/08). No current_pick -> empty call `ppX()`. The keyword on the node
# is emitted verbatim so the post-rename vocabulary (ppVar/ppNum/...) and the
# legacy vocabulary (var/num/...) both flow through unchanged.
render_placeholder_preserved <- function(node) {
  keyword <- node$keyword
  if (is.null(keyword) || !nzchar(keyword)) keyword <- "<anon>"
  has_pick <- !is.null(node$current_pick)
  inner <- if (has_pick) format_placeholder_value(node$current_pick, keyword) else ""
  shared_part <- if (!is.null(node$shared) && nzchar(as.character(node$shared))) {
    paste0("shared = \"", as.character(node$shared), "\"")
  } else {
    ""
  }
  args <- c(inner, shared_part)
  args <- args[nzchar(args)]
  paste0(keyword, "(", paste(args, collapse = ", "), ")")
}

# Map a current pick + keyword to the rendered inner-arg text. Dispatch is
# keyword-first (so var/ppVar always bareword, text/ppText always quoted, etc.)
# with a type-based fallback for unknown keywords. Backticks wrap non-syntactic
# column names; quotes use deparse() to escape correctly.
format_placeholder_value <- function(value, keyword) {
  # Language objects (calls, symbols, base R expression() vectors) always
  # deparse as code. We avoid rlang::is_expression() here because it returns
  # TRUE for bare character scalars too, which would route every var pick
  # into expr_text() instead of the bareword/quoted dispatch below.
  if (rlang::is_call(value) || rlang::is_symbol(value) ||
      inherits(value, "expression")) {
    return(rlang::expr_text(value))
  }
  bareword_kw <- c("var", "ppVar", "upload", "ppUpload")
  string_kw   <- c("text", "ppText")
  numeric_kw  <- c("num", "ppNum")
  expr_kw     <- c("expr", "ppExpr")
  if (keyword %in% bareword_kw) return(format_bareword(value))
  if (keyword %in% string_kw)   return(format_string(value))
  if (keyword %in% numeric_kw)  return(format_numeric(value))
  if (keyword %in% expr_kw)     return(format_expr(value))
  # Unknown keyword: fall back to type-of dispatch.
  if (is.character(value)) return(format_string(value))
  if (is.numeric(value) || is.logical(value)) return(format_numeric(value))
  rlang::expr_text(value)
}

format_bareword <- function(value) {
  s <- as.character(value)
  if (length(s) != 1L || is.na(s) || !nzchar(s)) return("")
  if (make.names(s) == s) return(s)
  paste0("`", s, "`")
}

format_string <- function(value) {
  s <- as.character(value)
  if (length(s) != 1L) s <- paste0(s, collapse = ", ")
  deparse(s)
}

format_numeric <- function(value) {
  if (length(value) == 0L) return("")
  if (length(value) == 1L) return(format(value))
  paste0("c(", paste(vapply(value, format, character(1)), collapse = ", "), ")")
}

format_expr <- function(value) {
  if (is.character(value) && length(value) == 1L) {
    parsed <- tryCatch(rlang::parse_expr(value), error = function(e) NULL)
    if (!is.null(parsed)) return(rlang::expr_text(parsed))
    return(deparse(value))
  }
  rlang::expr_text(value)
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
