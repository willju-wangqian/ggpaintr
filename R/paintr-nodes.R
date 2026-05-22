# Typed AST node constructors and predicates.
#
# Every non-leaf node carries `expr` — the underlying R `language` object — so
# render (P10) and eval (P11) need no reverse translation.

new_ptr_node <- function(class, ...) {
  structure(list(...), class = c(class, "ptr_node"))
}

ptr_root <- function(layers, expr = NULL) {
  new_ptr_node("ptr_root", layers = layers, expr = expr)
}

ptr_layer <- function(name, expr, data_arg = NULL, children = list(),
                     active_input_id = NULL, default_active = TRUE,
                     active = TRUE, source_pipe_op = NULL) {
  new_ptr_node(
    "ptr_layer",
    name = name, expr = expr, data_arg = data_arg, children = children,
    active_input_id = active_input_id, default_active = default_active,
    active = active, source_pipe_op = source_pipe_op
  )
}

ptr_pipeline <- function(stages, op, expr) {
  if (!op %in% c("|>", "%>%")) {
    rlang::abort(paste0("Unknown pipe op: ", op))
  }
  new_ptr_node("ptr_pipeline", stages = stages, op = op, expr = expr)
}

ptr_call <- function(fun, args, expr) {
  new_ptr_node("ptr_call", fun = fun, args = args, expr = expr)
}

# Placeholder nodes carry two additive slots populated by the registry/parser
# (PLAN-06): `default` (the validated positional default-arg value, or NULL)
# and `named_args` (a named list of validated named-argument values, empty
# when none). The constructors store whatever they are given; shape-checking
# happens upstream.
ptr_ph_value <- function(id = NA_character_, keyword, param = NA_character_,
                       expr, shared = NULL,
                       default = NULL, named_args = list()) {
  new_ptr_node(
    "ptr_ph_value",
    id = id, keyword = keyword, param = param, expr = expr, shared = shared,
    default = default, named_args = named_args
  )
}

ptr_ph_data_consumer <- function(id = NA_character_, keyword, param = NA_character_,
                                expr, shared = NULL, upstream = NULL,
                                default = NULL, named_args = list()) {
  new_ptr_node(
    "ptr_ph_data_consumer",
    id = id, keyword = keyword, param = param, expr = expr,
    shared = shared, upstream = upstream,
    default = default, named_args = named_args
  )
}

ptr_ph_data_source <- function(id = NA_character_, keyword, param = NA_character_,
                              expr, shared = NULL, companion_id = NULL,
                              default = NULL, named_args = list()) {
  new_ptr_node(
    "ptr_ph_data_source",
    id = id, keyword = keyword, param = param, expr = expr,
    shared = shared, companion_id = companion_id,
    default = default, named_args = named_args
  )
}

ptr_user_expr <- function(inner) {
  new_ptr_node("ptr_user_expr", inner = inner)
}

ptr_literal <- function(expr) {
  new_ptr_node("ptr_literal", expr = expr)
}

# An anonymous-function literal (`function(.x) body`). `formals` is the raw
# formals pairlist, kept verbatim so the closure can be reconstructed and
# deparsed correctly. `body` is a translated child node, so placeholders
# inside the lambda body still resolve. A `function` call is *not* an
# ordinary call (its components are formals/body/srcref, not positional
# args), so it needs its own node rather than going through `ptr_call`.
ptr_closure <- function(formals, body, expr) {
  new_ptr_node("ptr_closure", formals = formals, body = body, expr = expr)
}

ptr_missing <- function() {
  new_ptr_node("ptr_missing")
}

is_ptr_node <- function(x) inherits(x, "ptr_node")
is_ptr_root <- function(x) inherits(x, "ptr_root")
is_ptr_layer <- function(x) inherits(x, "ptr_layer")
is_ptr_pipeline <- function(x) inherits(x, "ptr_pipeline")
is_ptr_call <- function(x) inherits(x, "ptr_call")
is_ptr_ph_value <- function(x) inherits(x, "ptr_ph_value")
is_ptr_ph_data_consumer <- function(x) inherits(x, "ptr_ph_data_consumer")
is_ptr_ph_data_source <- function(x) inherits(x, "ptr_ph_data_source")
is_ptr_placeholder <- function(x) {
  is_ptr_ph_value(x) || is_ptr_ph_data_consumer(x) || is_ptr_ph_data_source(x)
}
is_ptr_user_expr <- function(x) inherits(x, "ptr_user_expr")
is_ptr_closure <- function(x) inherits(x, "ptr_closure")
is_ptr_literal <- function(x) inherits(x, "ptr_literal")
is_ptr_missing <- function(x) inherits(x, "ptr_missing")

# Structural equality of two typed-tree nodes, ignoring non-structural metadata
# (`$op`, `$expr`). The canonical-pipeline-lift contract (ADR 0012 §1) requires
# that `|>`, `%>%`, and nested-call surface forms produce trees that compare
# equal *structurally* — but `$op` is surface-pipe metadata and `$expr` is the
# original-AST hint duplicated alongside the load-bearing `$fun`/`$args`/
# `$stages` fields. Both legitimately diverge across surface forms.
#
# `ptr_literal` is the one exception: its `$expr` slot *is* the load-bearing
# value (a literal has no other content), so we compare it. Lists are walked
# element-wise with name equality; leaves via `identical()`.
#
# Internal helper; not exported.
ptr_tree_structural_equal <- function(a, b) {
  if (is_ptr_node(a) || is_ptr_node(b)) {
    if (!is_ptr_node(a) || !is_ptr_node(b)) return(FALSE)
    if (!identical(class(a), class(b))) return(FALSE)
    if (is_ptr_literal(a)) {
      return(identical(a$expr, b$expr))
    }
    names_a <- setdiff(names(a), c("op", "expr"))
    names_b <- setdiff(names(b), c("op", "expr"))
    if (!setequal(names_a, names_b)) return(FALSE)
    for (nm in names_a) {
      if (!ptr_tree_structural_equal(a[[nm]], b[[nm]])) return(FALSE)
    }
    return(TRUE)
  }
  if (is.list(a) || is.list(b)) {
    if (!is.list(a) || !is.list(b)) return(FALSE)
    if (length(a) != length(b)) return(FALSE)
    na <- names(a) %||% rep_len("", length(a))
    nb <- names(b) %||% rep_len("", length(b))
    if (!identical(na, nb)) return(FALSE)
    for (i in seq_along(a)) {
      if (!ptr_tree_structural_equal(a[[i]], b[[i]])) return(FALSE)
    }
    return(TRUE)
  }
  identical(a, b)
}
