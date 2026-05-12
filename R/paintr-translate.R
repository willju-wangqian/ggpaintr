# P1 — translate. String formula -> typed tree.
#
# Steps:
#   1. Validate input shape (single non-empty string).
#   2. `rlang::parse_exprs`; reject zero / multiple top-level expressions.
#   3. Depth check (default 100).
#   4. Split top-level `+` into layer expressions; translate each.
#   5. Dedupe layer names with -2/-3 suffixes; attach checkbox ids for
#      non-ggplot layers.
#   6. Wrap in `ptr_root`; run safety walker (P5).

ptr_translate <- function(formula, expr_check = TRUE, max_depth = 100L,
                          ns_fn = shiny::NS(NULL), annotate = TRUE) {
  assertthat::assert_that(
    is.character(formula), length(formula) == 1L, !is.na(formula),
    msg = "`formula` must be a single string."
  )
  if (!nzchar(trimws(formula, which = "both"))) {
    rlang::abort("Formula is empty or whitespace.")
  }

  preprocessed <- preprocess_native_pipe(formula)
  exprs <- tryCatch(
    rlang::parse_exprs(preprocessed),
    error = function(e) {
      rlang::abort(paste0("Could not parse formula: ", conditionMessage(e)))
    }
  )
  if (length(exprs) == 0L) {
    rlang::abort("Formula is empty or whitespace.")
  }
  if (length(exprs) > 1L) {
    rlang::abort(paste0(
      "Formula must contain exactly one top-level expression ",
      "(found ", length(exprs), "). Did you forget a `+`?"
    ))
  }

  root_expr <- exprs[[1L]]
  check_translate_depth(root_expr, max_depth = max_depth)

  layer_exprs <- split_top_plus(root_expr)
  layers <- lapply(layer_exprs, translate_layer)
  layer_names <- vapply(layers, function(l) l$name, character(1))
  layer_names <- dedupe_layer_names(layer_names)

  for (i in seq_along(layers)) {
    layers[[i]]$name <- layer_names[i]
    if (is_ptr_layer(layers[[i]])) {
      if (layer_names[i] == "ggplot") {
        layers[[i]]$active_input_id <- NULL
      } else {
        layers[[i]]$active_input_id <- paste0(layer_names[i], "_checkbox")
      }
    }
  }

  root <- ptr_root(layers = layers, expr = root_expr)
  ptr_validate_tree_safety(root, expr_check = expr_check)
  if (annotate) {
    root <- ptr_assign_ids(root, ns_fn = ns_fn)
    root <- ptr_classify_data(root)
    root <- ptr_shared_bind(root)
    ptr_assert_ids_assigned(root)
    ptr_assert_classified(root)
  }
  root
}

# ---- depth ------------------------------------------------------------------

check_translate_depth <- function(x, max_depth, .depth = 0L) {
  if (.depth > max_depth) {
    rlang::abort(paste0(
      "Formula nesting exceeds maximum depth (", max_depth, ")."
    ))
  }
  if (is.call(x) || is.pairlist(x)) {
    for (i in seq_along(x)) {
      check_translate_depth(x[[i]], max_depth = max_depth, .depth = .depth + 1L)
    }
  }
  invisible(NULL)
}

# ---- top-level + split ------------------------------------------------------

split_top_plus <- function(expr) {
  if (is.call(expr) && identical(expr[[1L]], quote(`+`)) && length(expr) == 3L) {
    c(split_top_plus(expr[[2L]]), list(expr[[3L]]))
  } else {
    list(expr)
  }
}

# ---- layer name dedupe ------------------------------------------------------

dedupe_layer_names <- function(names) {
  out <- character(length(names))
  counts <- list()
  for (i in seq_along(names)) {
    nm <- names[[i]]
    if (is.null(counts[[nm]])) {
      counts[[nm]] <- 1L
      out[i] <- nm
    } else {
      counts[[nm]] <- counts[[nm]] + 1L
      out[i] <- paste0(nm, "-", counts[[nm]])
    }
  }
  out
}

# ---- translate per node -----------------------------------------------------

translate_layer <- function(expr) {
  ph <- detect_placeholder(expr)
  if (!is.null(ph)) {
    node <- build_placeholder_node(expr, ph)
    node$name <- ph$keyword
    return(node)
  }
  if (is.call(expr) && is_pipe_head(expr[[1L]])) {
    return(translate_piped_layer(expr))
  }
  translate_plain_layer(expr)
}

# Extract the bare layer name from a layer-level expression. Supports plain
# calls, namespaced (`pkg::fn`, `pkg:::fn`), pipelines (terminal stage's name),
# and parenthesised heads.
layer_call_name <- function(expr) {
  if (is.call(expr)) {
    head <- expr[[1L]]
    if (is_pipe_head(head) && length(expr) >= 3L) {
      return(layer_call_name(expr[[3L]]))
    }
    if (is.symbol(head)) return(as.character(head))
    if (is.call(head)) {
      op <- head[[1L]]
      if (identical(op, quote(`::`)) || identical(op, quote(`:::`))) {
        return(as.character(head[[3L]]))
      }
      if (identical(op, quote(`(`))) {
        return(layer_call_name(head[[2L]]))
      }
    }
    return(rlang::expr_text(head))
  }
  if (is.symbol(expr)) return(as.character(expr))
  rlang::expr_text(expr)
}

.ptr_pipe_native_sentinel <- "%ptrPipeNative%"

is_pipe_head <- function(x) {
  if (!is.symbol(x)) return(FALSE)
  s <- as.character(x)
  s == "%>%" || s == .ptr_pipe_native_sentinel
}

pipe_op_from_symbol <- function(sym) {
  s <- as.character(sym)
  if (s == .ptr_pipe_native_sentinel) "|>" else s
}

# Native `|>` is desugared by R's parser at parse time, so a parsed AST
# never carries a `|>` call. To preserve native-pipe surface we substitute
# `|>` -> `%ptrPipeNative%` BEFORE parsing, using token positions from
# `getParseData()` so we never touch `|>` characters inside string
# literals or comments. The sentinel parses as a real `%any%` call which
# survives translation; it is mapped back to "|>" at op-extraction time.
# This is parse-time only — not the runtime gsub round-trip G7 forbids.
preprocess_native_pipe <- function(formula) {
  pd <- tryCatch(
    utils::getParseData(parse(text = formula, keep.source = TRUE)),
    error = function(e) NULL
  )
  if (is.null(pd) || nrow(pd) == 0L) return(formula)
  rows <- which(pd$token == "PIPE")
  if (length(rows) == 0L) return(formula)
  rows <- rows[order(-pd$line1[rows], -pd$col1[rows])]
  lines <- strsplit(formula, "\n", fixed = TRUE)[[1L]]
  if (length(lines) == 0L) lines <- ""
  for (i in rows) {
    ln <- pd$line1[i]
    c1 <- pd$col1[i]
    c2 <- pd$col2[i]
    line_str <- lines[ln]
    before <- if (c1 > 1L) substr(line_str, 1L, c1 - 1L) else ""
    after <- if (c2 < nchar(line_str)) substr(line_str, c2 + 1L, nchar(line_str)) else ""
    lines[ln] <- paste0(before, " ", .ptr_pipe_native_sentinel, " ", after)
  }
  paste(lines, collapse = "\n")
}

# Inspect a translated layer body for its data_arg (the first positional
# arg of a ggplot/geom call, or the upstream side of a terminal pipeline).
# Used by P2; P1 just attaches the subtree pointer.
translate_piped_layer <- function(expr) {
  outer_op <- pipe_op_from_symbol(expr[[1L]])
  outer_op_sym <- as.character(expr[[1L]])
  stages_raw <- collect_pipe_stages(expr, outer_op_sym)
  terminal <- stages_raw[[length(stages_raw)]]
  upstream_raw <- stages_raw[-length(stages_raw)]
  upstream_stages <- lapply(upstream_raw, translate_node)
  data_arg_expr <- build_pipe_left_expr(expr)
  data_arg <- ptr_pipeline(
    stages = upstream_stages, op = outer_op, expr = data_arg_expr
  )
  layer_name <- layer_call_name(terminal)
  children <- translate_layer_children(terminal)
  ptr_layer(
    name = layer_name, expr = expr, data_arg = data_arg,
    children = children, active_input_id = NULL,
    default_active = TRUE, active = TRUE
  )
}

translate_plain_layer <- function(expr) {
  layer_name <- layer_call_name(expr)
  if (is.symbol(expr)) {
    return(ptr_layer(
      name = layer_name, expr = expr, data_arg = NULL,
      children = list(ptr_literal(expr)),
      active_input_id = NULL, default_active = TRUE, active = TRUE
    ))
  }
  data_arg <- extract_call_data_arg(expr)
  children <- translate_layer_children(expr, exclude_data = !is.null(data_arg))
  ptr_layer(
    name = layer_name, expr = expr, data_arg = data_arg,
    children = children, active_input_id = NULL,
    default_active = TRUE, active = TRUE
  )
}

build_pipe_left_expr <- function(expr) {
  if (is.call(expr) && is_pipe_head(expr[[1L]]) && length(expr) >= 3L) {
    return(expr[[2L]])
  }
  expr
}

extract_call_data_arg <- function(expr) {
  if (!is.call(expr)) return(NULL)
  args <- as.list(expr[-1L])
  if (length(args) == 0L) return(NULL)
  arg_names <- names(args) %||% rep_len("", length(args))
  if ("data" %in% arg_names) {
    return(translate_node(args[[match("data", arg_names)]]))
  }
  # Only ggplot() takes `data` as its first positional argument.
  # geom_*()/stat_*() take `mapping` first, so positional matching there
  # would misroute aes() into the layer's data pipeline.
  head <- expr[[1L]]
  if (is.symbol(head) && identical(as.character(head), "ggplot") &&
      arg_names[1L] == "") {
    return(translate_node(args[[1L]]))
  }
  NULL
}

translate_layer_children <- function(expr, exclude_data = FALSE) {
  if (!is.call(expr)) return(list())
  args <- as.list(expr[-1L])
  if (length(args) == 0L) return(list())
  arg_names <- names(args) %||% rep_len("", length(args))
  keep <- rep_len(TRUE, length(args))
  if (exclude_data) {
    if ("data" %in% arg_names) {
      keep[match("data", arg_names)] <- FALSE
    } else if (arg_names[1L] == "") {
      keep[1L] <- FALSE
    }
  }
  child_args <- args[keep]
  child_names <- arg_names[keep]
  out <- vector("list", length(child_args))
  for (i in seq_along(child_args)) {
    out[[i]] <- translate_node(child_args[[i]])
  }
  names(out) <- child_names
  out
}

# ---- generic translate ------------------------------------------------------

translate_node <- function(expr) {
  if (is.call(expr)) {
    head <- expr[[1L]]
    if (is_pipe_head(head)) {
      return(translate_pipeline(expr))
    }
    ph <- detect_placeholder(expr)
    if (!is.null(ph)) {
      return(build_placeholder_node(expr, ph))
    }
    return(translate_call(expr))
  }
  if (is.symbol(expr)) {
    ph <- detect_placeholder(expr)
    if (!is.null(ph)) {
      return(build_placeholder_node(expr, ph))
    }
    return(ptr_literal(expr))
  }
  ptr_literal(expr)
}

translate_pipeline <- function(expr) {
  op <- pipe_op_from_symbol(expr[[1L]])
  op_sym <- as.character(expr[[1L]])
  stages <- collect_pipe_stages(expr, op_sym)
  translated <- lapply(stages, translate_node)
  ptr_pipeline(stages = translated, op = op, expr = expr)
}

# Flatten a left-associative pipe chain into ordered stages, preserving the
# per-stage op via the surrounding nodes. Mixed-op chains keep each stage's
# op via the node carrying that stage on its right side; for a uniform
# pipeline we expose a single op (the outermost). For mixed chains the spec
# requires per-stage op; we represent that by nesting `ptr_pipeline` nodes
# rather than inventing a per-stage op vector.
collect_pipe_stages <- function(expr, op) {
  out <- list()
  cur <- expr
  while (is.call(cur) && length(cur) >= 3L &&
         is.symbol(cur[[1L]]) && as.character(cur[[1L]]) == op) {
    out <- c(list(cur[[3L]]), out)
    cur <- cur[[2L]]
  }
  c(list(cur), out)
}

translate_call <- function(expr) {
  fun <- expr[[1L]]
  arg_exprs <- as.list(expr[-1L])
  arg_names <- names(arg_exprs) %||% rep_len("", length(arg_exprs))
  args <- vector("list", length(arg_exprs))
  for (i in seq_along(arg_exprs)) {
    args[[i]] <- translate_node(arg_exprs[[i]])
  }
  names(args) <- arg_names
  ptr_call(fun = fun, args = args, expr = expr)
}

# ---- placeholder detection --------------------------------------------------

detect_placeholder <- function(expr) {
  kw <- placeholder_keyword(expr)
  if (is.null(kw)) return(NULL)
  entry <- ptr_registry_lookup(kw)
  if (is.null(entry)) return(NULL)
  shared <- extract_shared(expr, kw)
  list(keyword = kw, entry = entry, shared = shared)
}

placeholder_keyword <- function(expr) {
  if (is.symbol(expr)) {
    nm <- as.character(expr)
    if (nm %in% ptr_registry_keywords()) return(nm)
    return(NULL)
  }
  if (is.call(expr) && is.symbol(expr[[1L]])) {
    nm <- as.character(expr[[1L]])
    if (nm %in% ptr_registry_keywords()) return(nm)
  }
  NULL
}

extract_shared <- function(expr, keyword) {
  if (is.symbol(expr)) return(NULL)
  if (length(expr) == 1L) return(NULL)
  args <- as.list(expr[-1L])
  arg_names <- names(args) %||% rep_len("", length(args))
  if (any(arg_names == "")) {
    rlang::abort(paste0(
      "Placeholder `", keyword, "(...)` accepts only the named ",
      "`shared` argument; positional args are not allowed."
    ))
  }
  unknown <- setdiff(arg_names, "shared")
  if (length(unknown) > 0L) {
    rlang::abort(paste0(
      "Placeholder `", keyword, "(...)` got unknown argument(s): ",
      paste(unknown, collapse = ", "), ". Only `shared` is allowed."
    ))
  }
  if (!"shared" %in% arg_names) return(NULL)
  shared_val <- args[[match("shared", arg_names)]]
  if (!is.character(shared_val) || length(shared_val) != 1L ||
      is.na(shared_val) || !nzchar(shared_val)) {
    rlang::abort(paste0(
      "`", keyword, "(shared = ...)` requires a single non-empty string."
    ))
  }
  shared_val
}

build_placeholder_node <- function(expr, ph) {
  role <- ph$entry$role
  switch(
    role,
    "value" = ptr_ph_value(
      keyword = ph$keyword, expr = expr, shared = ph$shared
    ),
    "consumer" = ptr_ph_data_consumer(
      keyword = ph$keyword, expr = expr, shared = ph$shared
    ),
    "source" = ptr_ph_data_source(
      keyword = ph$keyword, expr = expr, shared = ph$shared
    ),
    rlang::abort(paste0("Unknown placeholder role: ", role))
  )
}
