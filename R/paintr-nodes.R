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
                     active = TRUE) {
  new_ptr_node(
    "ptr_layer",
    name = name, expr = expr, data_arg = data_arg, children = children,
    active_input_id = active_input_id, default_active = default_active,
    active = active
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

ptr_ph_value <- function(id = NA_character_, keyword, param = NA_character_,
                       expr, shared = NULL) {
  new_ptr_node(
    "ptr_ph_value",
    id = id, keyword = keyword, param = param, expr = expr, shared = shared
  )
}

ptr_ph_data_consumer <- function(id = NA_character_, keyword, param = NA_character_,
                                expr, shared = NULL, upstream = NULL) {
  new_ptr_node(
    "ptr_ph_data_consumer",
    id = id, keyword = keyword, param = param, expr = expr,
    shared = shared, upstream = upstream
  )
}

ptr_ph_data_source <- function(id = NA_character_, keyword, param = NA_character_,
                              expr, shared = NULL, companion_id = NA_character_) {
  new_ptr_node(
    "ptr_ph_data_source",
    id = id, keyword = keyword, param = param, expr = expr,
    shared = shared, companion_id = companion_id
  )
}

ptr_user_expr <- function(inner) {
  new_ptr_node("ptr_user_expr", inner = inner)
}

ptr_literal <- function(expr) {
  new_ptr_node("ptr_literal", expr = expr)
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
is_ptr_literal <- function(x) inherits(x, "ptr_literal")
is_ptr_missing <- function(x) inherits(x, "ptr_missing")
