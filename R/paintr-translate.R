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

ptr_first_pipe_op <- function(formula) {
  m_pct  <- regexpr("%>%", formula, fixed = TRUE)
  m_pipe <- regexpr("|>",  formula, fixed = TRUE)
  pct_pos  <- if (m_pct[1]  == -1L) Inf else as.integer(m_pct)
  pipe_pos <- if (m_pipe[1] == -1L) Inf else as.integer(m_pipe)
  if (is.infinite(pct_pos) && is.infinite(pipe_pos)) "|>"
  else if (pct_pos < pipe_pos) "%>%" else "|>"
}

ptr_translate <- function(formula, expr_check = TRUE, max_depth = 100L,
                          ns_fn = shiny::NS(NULL), annotate = TRUE) {
  assertthat::assert_that(
    is.character(formula), length(formula) == 1L, !is.na(formula),
    msg = "`formula` must be a single string."
  )
  if (!nzchar(trimws(formula, which = "both"))) {
    rlang::abort("Formula is empty or whitespace.")
  }

  # First-pipe-op rule (PLAN-01, ADR 0012 §5 OQ2). Derive a render-time
  # annotation from the source string: pick whichever of `%>%` / `|>`
  # appears first; default to `|>` when neither is present. In expression
  # mode `|>` is already desugared at parse time, so only `%>%` can win
  # there; in string mode the rule disambiguates mixed-op formulas to one
  # canonical choice. The hint is propagated as a pure render-time slot;
  # the typed tree's structure is unchanged across surface forms (the
  # canonical-tree invariant, ADR 0012 §1).
  op_hint <- ptr_first_pipe_op(formula)

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
      "A plot formula must be a single ggplot expression, but ",
      length(exprs), " were found. Join layers with `+`, e.g. ",
      "`ggplot(...) + geom_point(...)`."
    ))
  }

  root_expr <- exprs[[1L]]
  check_translate_depth(root_expr, max_depth = max_depth)

  # Canonical-tree contract (ADR 0012 §1, PLAN-02): pipes are pure
  # first-arg-insertion sugar with no surviving structural meaning. Desugar
  # every `%>%` and native-pipe sentinel to fully-nested call form before
  # any per-layer translation runs, so every surface form (`|>`, `%>%`,
  # nested-call) produces structurally-identical layer expressions. The
  # `ptr_classify_calls` pass then lifts the nested chain back into a
  # canonical `ptr_pipeline` whose shape depends only on the chain's
  # semantics — never on which pipe operator the user typed.
  root_expr <- desugar_pipes_to_nested(root_expr)

  layer_exprs <- split_top_plus(root_expr)
  layers <- lapply(layer_exprs, translate_layer, op_hint = op_hint)
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
    # Pass order (PLAN-02): classify_calls (lift) → assign_ids → classify_data
    # → shared_bind. `assign_ids` must run AFTER `classify_calls` so the
    # structural-position-derived ids are stamped on the canonical post-lift
    # shape — otherwise nested-call and `%>%` users would get different ids
    # for the same widget. `classify_data` must run AFTER `assign_ids` so the
    # `upstream` references it pins on each `ptr_ph_data_consumer` point at
    # post-id-stamping nodes (R lists are copy-on-modify; a pre-id reference
    # would freeze placeholders inside the upstream subtree at id = NA).
    # Side benefit: `assign_stage_ids` (called inside `ptr_assign_ids`) now
    # finds stages to id-stamp for all surface forms.
    root <- ptr_classify_calls(root, op_hint = op_hint)
    # ADR 0021: a `ptr_call(ppVerbSwitch, ...)` (or `ppLayerOff` at layer
    # position) is legitimate ONLY at a structural-keyword position
    # (lifted into a pipeline stage by `ptr_classify_calls` and unwrapped
    # by `build_pipeline_from_lift`, or handled by `translate_layer`).
    # If any survives the lift, the wrapper sits at a wrong position
    # (e.g. inside `aes()`, a geom's mapping/arg list, or a non-data arg).
    ptr_assert_no_surviving_structural_wrappers(root)
    root <- ptr_assign_ids(root, ns_fn = ns_fn)
    # ADR 0021: `is_data_chain_call` is the single gate (placeholder OR
    # `has_user_control`); the `ppVerbSwitch` unwrap stamps
    # `has_user_control = TRUE`, so `assign_stage_ids` covers it.
    root <- ptr_classify_data(root)
    root <- ptr_shared_bind(root)
    ptr_validate_shared_roles(root)
    ptr_assert_ids_assigned(root)
    ptr_assert_classified(root)
  }
  root
}

# Walks the typed tree and aborts (class `"ptr_translate_error"`) if any
# `ppLayerOff` or `ppVerbSwitch` wrapper survived translation. The
# special-unwrap branches in `translate_layer` (layer position) and
# `build_pipeline_from_lift` (pipeline-stage position) intercept the
# legitimate positions; a surviving `ptr_call(<keyword>, ...)` means the
# wrapper sat at a wrong position.
ptr_assert_no_surviving_structural_wrappers <- function(root) {
  bad <- character()
  ptr_walk(root, function(n) {
    if (!is_ptr_call(n)) return()
    head <- n$fun
    if (!is.symbol(head)) return()
    nm <- as.character(head)
    if (nm %in% c("ppLayerOff", "ppVerbSwitch")) {
      bad[[length(bad) + 1L]] <<- nm
    }
  })
  if (length(bad) > 0L) {
    nm <- bad[[1L]]
    abort_translate(paste0(
      "`", nm, "(...)` was used outside its valid position. ",
      if (nm == "ppLayerOff") {
        paste0(
          "`ppLayerOff(layer_expr, hide)` is only valid as a top-level ",
          "layer (joined to a `ggplot()` chain with `+`)."
        )
      } else {
        paste0(
          "`ppVerbSwitch(.data, verb_expr, switch_on, label)` is only ",
          "valid as a stage in a data-argument pipeline (e.g. `mtcars |> ",
          "ppVerbSwitch(mutate(x = 1), TRUE) |> ggplot(...)`)."
        )
      }
    ))
  }
  invisible(NULL)
}

# Canonical-pipeline-lift pass (ADR 0012 §2). Walks each layer's `$data_arg`
# only (never `$children` — the `+` fence holds, ADR §3.4) and, when the
# data-arg is a `ptr_call`, attempts to lift the nested-call chain into a
# canonical `ptr_pipeline` via `try_lift_to_pipeline`. The lift fires when
# three gates pass: at least one stage exists above the source, the
# stages-then-source list round-trips identical to the desugared input, and
# the source is a non-call AST node. Any failure leaves the data-arg
# unchanged. Internal.
ptr_classify_calls <- function(root, op_hint = "|>") {
  if (!is_ptr_root(root)) {
    rlang::abort("ptr_classify_calls expects a ptr_root.")
  }
  for (i in seq_along(root$layers)) {
    layer <- root$layers[[i]]
    if (!is_ptr_layer(layer)) next
    da <- layer$data_arg
    if (!is_ptr_call(da)) next
    raw_ast <- da$expr
    lifted  <- try_lift_to_pipeline(raw_ast)
    if (!isTRUE(lifted$success)) next
    root$layers[[i]]$data_arg <- build_pipeline_from_lift(lifted$parts,
                                                         op_hint = op_hint)
  }
  root
}

# Construct a typed `ptr_pipeline` from the result of `try_lift_to_pipeline`.
# The source (a raw AST symbol) becomes stage[[1]] via `translate_node`; each
# stage's stripped call goes through `translate_node` and then gets its
# `first_arg_name` attached (chr scalar captured at split time, "" when the
# user wrote the source positionally). The canonical-nested-form expression
# is recorded as the pipeline's `$expr` for downstream visibility.
build_pipeline_from_lift <- function(parts, op_hint = "|>") {
  source_node <- translate_node(parts$source)
  stage_nodes <- vector("list", length(parts$stages))
  for (j in seq_along(parts$stages)) {
    st <- parts$stages[[j]]
    # ADR 0021 special-unwrap branch for `ppVerbSwitch` at pipeline-stage
    # position. By this point the lift's resugar step has stripped the
    # implicit `.data` slot, so `st$call` is
    # `ppVerbSwitch(verb_expr, switch_on, label)`. Unwrap to the inner
    # verb call with `has_user_control = TRUE`,
    # `default_stage_enabled = switch_on`, and `stage_label = label`.
    if (is_structural_keyword_call(st$call, "ppVerbSwitch")) {
      typed <- unwrap_pp_verb_switch_stage(st$call)
    } else {
      typed <- translate_node(st$call)
    }
    typed$first_arg_name <- st$first_arg_name
    stage_nodes[[j]] <- typed
  }
  canonical <- rebuild_nested_from_stages(parts)
  ptr_pipeline(
    stages = c(list(source_node), stage_nodes),
    op     = op_hint,
    expr   = canonical
  )
}

# ---- canonical pipeline lift -----------------------------------------------
#
# Engine (ADR 0012 §3.1, PLAN-02 §"Engine"): four pure helpers that take a
# raw R AST and produce either a structurally-canonical pipeline parts list
# or a typed rejection reason. The lift fires the same way regardless of
# surface form because Step 1 erases the pipe operators.

# Step 1 — desugar all pipe operators (`%>%` and the native-pipe sentinel)
# into fully-nested call form by inserting LHS as the RHS's first positional
# arg. Recurses into all args of non-pipe calls, preserving arg names. A
# no-op when the input contains no pipes.
desugar_pipes_to_nested <- function(expr) {
  if (!is.call(expr)) return(expr)
  if (length(expr) >= 3L && is.symbol(expr[[1L]])) {
    head_str <- as.character(expr[[1L]])
    if (head_str == "%>%" || head_str == .ptr_pipe_native_sentinel) {
      lhs <- desugar_pipes_to_nested(expr[[2L]])
      rhs <- desugar_pipes_to_nested(expr[[3L]])
      if (is.call(rhs)) {
        return(as.call(c(list(rhs[[1L]]), list(lhs), as.list(rhs[-1L]))))
      }
      if (is.symbol(rhs)) {
        return(as.call(list(rhs, lhs)))
      }
      # Pathological RHS (e.g., literal). Leave the call untouched — the
      # safety walker / downstream consumers will reject it on their own
      # terms.
      return(expr)
    }
  }
  # Recurse into a non-pipe call's args, preserving arg names.
  recursed <- lapply(as.list(expr), desugar_pipes_to_nested)
  out <- as.call(recursed)
  names(out) <- names(expr)
  out
}

# Step 2 — split a fully-nested call into `list(source, stages)`. Always-
# aggressive descent: walks through every call whose first positional arg is
# itself a call (ADR §1's "tree is semantic, not syntactic" — no early-stop
# heuristic, no verb whitelist). On loop termination, if `cur` is still a
# call whose first arg is a non-call, executes a post-loop split that
# extracts the non-call as the source. Each stage records its `first_arg_name`
# (a chr scalar, "" when the user wrote that slot positionally) so the round
# trip rebuilds the named-arg shape losslessly.
resugar_pipeline_stages <- function(expr) {
  stages <- list()
  cur <- expr
  while (is.call(cur) && length(cur) >= 2L && is.call(cur[[2L]])) {
    head <- cur[[1L]]
    rest_args <- as.list(cur[-c(1L, 2L)])
    nm_all <- names(cur) %||% rep_len("", length(cur))
    fan <- nm_all[2L]
    if (is.na(fan)) fan <- ""
    stages <- c(
      list(list(call = as.call(c(list(head), rest_args)),
                first_arg_name = fan)),
      stages
    )
    cur <- cur[[2L]]
  }
  # Post-loop split: cur is still a call whose first arg is a non-call.
  # Extract that non-call as the source and bank cur (stripped of its first
  # arg) as the deepest stage. Skip when cur is any registered placeholder
  # call — every placeholder's arg-bearing form is captured state on the
  # node (via `default_arg`), not upstream data, so the call must stay
  # atomic regardless of role.
  if (is.call(cur) && length(cur) >= 2L && !is.call(cur[[2L]]) &&
      !is_placeholder_call(cur)) {
    head <- cur[[1L]]
    rest_args <- as.list(cur[-c(1L, 2L)])
    nm_all <- names(cur) %||% rep_len("", length(cur))
    fan <- nm_all[2L]
    if (is.na(fan)) fan <- ""
    source_arg <- cur[[2L]]
    stages <- c(
      list(list(call = as.call(c(list(head), rest_args)),
                first_arg_name = fan)),
      stages
    )
    cur <- source_arg
  }
  list(source = cur, stages = stages)
}

# Step 3 — inverse of Step 2. Folds the source + ordered stages back into a
# fully-nested call, restoring each stage's captured `first_arg_name` on the
# inserted source slot. Used by GATE 1 (round-trip identity) and by the
# pipeline constructor's `expr=` slot.
rebuild_nested_from_stages <- function(parts) {
  if (length(parts$stages) == 0L) return(parts$source)
  acc <- parts$source
  for (st in parts$stages) {
    rest_args <- as.list(st$call[-1L])
    new_args  <- c(list(acc), rest_args)
    nms <- names(new_args) %||% rep_len("", length(new_args))
    nms[1L] <- st$first_arg_name %||% ""
    if (all(!nzchar(nms))) {
      # No named args anywhere — keep the call unnamed so identical() against
      # the canonical-nested form holds (R drops the names attribute when no
      # element carries a name).
      names(new_args) <- NULL
    } else {
      names(new_args) <- nms
    }
    acc <- as.call(c(list(st$call[[1L]]), new_args))
  }
  acc
}

# True when `expr` is a call whose head names ANY registered placeholder
# (regardless of role: value, consumer, or source). For every placeholder
# the arg-bearing form `keyword(arg)` carries `arg` as the placeholder's
# captured `default_arg` -- it is captured state on the node, not upstream
# data -- so the canonical lift must treat the whole call as an atomic
# leaf (NOT split out the arg as a separate pipeline stage). Source-role
# placeholders are the typical leaf case (`ppUpload(penguins)`); value and
# consumer roles are pathological in source position (`ppVar(mpg) |>
# filter(...)` is nonsense) but get the same protection for symmetry --
# otherwise lifting through a value/consumer placeholder loses its
# default-arg capture and silently breaks the per-role contract. The
# lookup is best-effort: when the registry is unavailable or the head is
# not a bare symbol, we return FALSE and the normal lift rules apply.
is_placeholder_call <- function(expr) {
  if (!is.call(expr) || length(expr) < 1L) return(FALSE)
  head <- expr[[1L]]
  if (!is.symbol(head)) return(FALSE)
  entry <- tryCatch(ptr_registry_lookup(as.character(head)),
                    error = function(e) NULL)
  if (is.null(entry)) return(FALSE)
  # ADR 0020 / 0021: structural keywords (`ppLayerOff`, `ppVerbSwitch`)
  # live in the registry only so `placeholder_keyword()` recognises their
  # names; they are NOT placeholders and the canonical-pipeline lift must
  # continue to treat them as ordinary callables (so the post-loop
  # source-split still fires when a structural wrapper sits at the
  # deepest non-call slot).
  if (identical(entry$role, "structural")) return(FALSE)
  TRUE
}

# Step 4 — round-trip + grounding gates. Returns either
# `list(success = TRUE, parts = ...)` when the lift may fire, or
# `list(success = FALSE, reason = <no-stages|round-trip-mismatch|
# opaque-call-source|non-data-source>)` otherwise. Three independent gates,
# all of which must pass:
#   GATE 0 (non-empty):    the stages list must hold at least one stage above
#                          the source. The 0-stage case is degenerate
#                          (e.g. `f()` with no args, where source = the
#                          unchanged input call) — nothing to lift.
#   GATE 1 (round-trip):   the split + rebuild must equal the canonical-
#                          nested form, otherwise the split bisected a
#                          structure we cannot losslessly reconstruct.
#   GATE 2 (grounding):    the source slot must be a SYMBOL. A call source
#                          is an "opaque-call terminal" (ADR §3.2); a
#                          non-call non-symbol (a literal that fell out of
#                          bisecting `data.frame(x = 1)`, say) is not a
#                          structurally-valid data source.
# At lift time the source is raw AST, so the grounding check is
# `is.symbol(source)` — not `is_ptr_ph_data_source` (placeholders are still
# plain symbols pre-translate_node).
try_lift_to_pipeline <- function(expr) {
  canonical <- desugar_pipes_to_nested(expr)
  parts     <- resugar_pipeline_stages(canonical)
  if (length(parts$stages) < 1L) {
    return(list(success = FALSE, reason = "no-stages"))
  }
  rebuilt <- rebuild_nested_from_stages(parts)
  if (!identical(rebuilt, canonical)) {
    return(list(success = FALSE, reason = "round-trip-mismatch"))
  }
  if (is.call(parts$source) && !is_placeholder_call(parts$source)) {
    return(list(success = FALSE, reason = "opaque-call-source"))
  }
  if (!is.symbol(parts$source) && !is_placeholder_call(parts$source)) {
    return(list(success = FALSE, reason = "non-data-source"))
  }
  list(success = TRUE, parts = parts)
}

# ---- depth ------------------------------------------------------------------

check_translate_depth <- function(x, max_depth, .depth = 0L) {
  if (.depth > max_depth) {
    rlang::abort(paste0(
      "The formula is nested too deeply (limit: ", max_depth, " levels). ",
      "Simplify it -- see `?ptr_app` for the expected formula shape."
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

translate_layer <- function(expr, op_hint = "|>") {
  # ADR 0020 / 0021 special-unwrap branches. `ppLayerOff` is legal here and
  # unwraps to the inner layer with `default_active = !hide`. `ppVerbSwitch`
  # is NOT legal at a layer position — it belongs in a pipeline-stage
  # position (intercepted by `build_pipeline_from_lift`). The error class
  # `"ptr_translate_error"` is introduced by this validator.
  if (is_structural_keyword_call(expr, "ppLayerOff")) {
    return(unwrap_pp_layer_off(expr, op_hint = op_hint))
  }
  if (is_structural_keyword_call(expr, "ppVerbSwitch")) {
    abort_translate(paste0(
      "`ppVerbSwitch(...)` was used at a layer position, but it is only ",
      "valid as a pipeline-stage wrapper inside a data argument (e.g. ",
      "`mtcars |> ppVerbSwitch(mutate(x = 1), TRUE) |> ggplot(...)`). Layers ",
      "are added with `+`, not piped — use `ppLayerOff(layer_expr, hide)` ",
      "for off-by-default layers."
    ))
  }
  ph <- detect_placeholder(expr)
  if (!is.null(ph)) {
    node <- build_placeholder_node(expr, ph)
    node$name <- ph$keyword
    return(node)
  }
  # PLAN-02: `ptr_translate` desugars every `%>%` and native-pipe sentinel to
  # fully-nested call form before layer translation runs, so a layer expr
  # never carries a pipe head here. The post-desugar layer always goes
  # through `translate_plain_layer`; canonical pipeline construction is the
  # job of `ptr_classify_calls` (it lifts the nested data-arg chain).
  translate_plain_layer(expr, op_hint = op_hint)
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

# PLAN-02: `translate_piped_layer` (the legacy `%>%`-only layer translator)
# and its helper `build_pipe_left_expr` were deleted along with
# `translate_pipeline` and `collect_pipe_stages` (the `%>%`-only inner
# pipeline constructor). The formula-level `desugar_pipes_to_nested` pass in
# `ptr_translate` collapses every pipe surface form to nested-call shape
# before per-layer translation runs, so every layer now goes through
# `translate_plain_layer`; the canonical `ptr_pipeline` is constructed by
# `ptr_classify_calls` from the post-desugar nested data-arg chain.

translate_plain_layer <- function(expr, op_hint = "|>") {
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
  # PLAN-01 (ADR 0012 §5 OQ2): stamp the render-time `source_pipe_op` hint
  # only when this layer carries a source piped in (`data_arg` non-NULL).
  # No data_arg ⇒ slot stays NULL ⇒ render side has no chain to emit.
  source_pipe_op <- if (!is.null(data_arg)) op_hint else NULL
  ptr_layer(
    name = layer_name, expr = expr, data_arg = data_arg,
    children = children, active_input_id = NULL,
    default_active = TRUE, active = TRUE,
    source_pipe_op = source_pipe_op
  )
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
    # ADR 0020 wrong-position guard for `ppLayerOff`. The legitimate
    # special-unwrap branch runs in `translate_layer` (layer position);
    # if `ppLayerOff(...)` reaches the generic translator, it is buried
    # inside another expression and structurally invalid.
    #
    # `ppVerbSwitch` is deliberately NOT aborted here: the data-arg of
    # `ggplot()` IS a legitimate position for it (the surrounding
    # `extract_call_data_arg` calls back into `translate_node`, and
    # `ptr_classify_calls` later lifts the resulting `ptr_call(ppVerbSwitch,
    # ...)` into a pipeline whose stages are handled by
    # `build_pipeline_from_lift`'s unwrap branch). A post-translate
    # surviving-wrapper scan catches `ppVerbSwitch` calls that did not get
    # lifted into a stage (e.g. used inside `aes()` or as a non-data arg).
    if (is_structural_keyword_call(expr, "ppLayerOff")) {
      abort_translate(paste0(
        "`ppLayerOff(...)` was used outside a layer position. ",
        "`ppLayerOff(layer_expr, hide)` is only valid as a top-level layer ",
        "(joined to a `ggplot()` chain with `+`). It cannot appear inside ",
        "`aes()`, a layer's mapping/arg list, or any other sub-expression."
      ))
    }
    # PLAN-02: pipes were already collapsed to nested form by
    # `desugar_pipes_to_nested` at the formula level, so a pipe head can
    # never reach here; the legacy `translate_pipeline` / `collect_pipe_stages`
    # dispatch is gone. Canonical pipeline construction happens later in
    # `ptr_classify_calls` via the `try_lift_to_pipeline` engine.
    ph <- detect_placeholder(expr)
    if (!is.null(ph)) {
      return(build_placeholder_node(expr, ph))
    }
    if (identical(head, as.name("function"))) {
      return(ptr_closure(
        formals = expr[[2L]],
        body    = translate_node(expr[[3L]]),
        expr    = expr
      ))
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
  # ADR 0020: structural keywords are recognised by `placeholder_keyword()`
  # for translator special-unwrap branches, but they are NOT placeholders.
  # Returning NULL here keeps `build_placeholder_node` / `extract_placeholder_args`
  # from running on them — the wrapper's positional args (`layer_expr`,
  # `verb_expr`, `hide`) would otherwise be rejected as "no default_arg".
  if (identical(entry$role, "structural")) return(NULL)
  args <- extract_placeholder_args(expr, entry)
  list(keyword = kw, entry = entry,
       shared = args$shared, default = args$default,
       named_args = args$named_args)
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

# ---- ADR 0020 structural-keyword helpers ----------------------------------

# Translate-time error class used by ADR 0020 validators. No pre-existing
# class convention in this file (the lone earlier `rlang::abort()` carries
# none); this class is introduced for the two new validators (literal-only
# `hide`; wrong-position keyword) and any future translate-time validation
# should reuse it.
abort_translate <- function(msg) {
  rlang::abort(msg, class = "ptr_translate_error")
}

# TRUE when `expr` is a call whose head names the given structural keyword
# (one of "ppLayerOff", "ppVerbSwitch"). Pure shape check; the
# registry-lookup version (`is_placeholder_call`) excludes structural
# roles by design and is the wrong primitive for the special-unwrap
# branches.
is_structural_keyword_call <- function(expr, keyword) {
  if (!is.call(expr) || length(expr) < 1L) return(FALSE)
  head <- expr[[1L]]
  is.symbol(head) && identical(as.character(head), keyword)
}

# Returns the literal logical `hide` value (a length-1 non-NA logical) from
# a `ppLayerOff()` wrapper call, OR aborts with class
# `"ptr_translate_error"` when the slot is missing, non-literal, or not a
# length-1 logical. The wrapper accepts `hide` named or positional;
# `hide_position` is the positional slot (2 for `ppLayerOff`). Default is
# TRUE when the slot is omitted entirely — matches the function-body
# default.
validate_pp_off_hide <- function(expr, keyword, hide_position) {
  args <- as.list(expr[-1L])
  arg_names <- names(args) %||% rep_len("", length(args))
  if ("hide" %in% arg_names) {
    hide_expr <- args[[which(arg_names == "hide")[[1L]]]]
  } else if (length(args) >= hide_position &&
             !nzchar(arg_names[[hide_position]])) {
    hide_expr <- args[[hide_position]]
  } else {
    return(TRUE)
  }
  if (!is.logical(hide_expr) || length(hide_expr) != 1L || is.na(hide_expr)) {
    abort_translate(paste0(
      "`", keyword, "(hide = )` must be a length-1 logical literal ",
      "(TRUE or FALSE); got `", deparse(hide_expr)[[1L]],
      "`. The boot-state metadata is consumed at translate time so the ",
      "formula remains the single source of truth — variables and ",
      "non-literal expressions are not allowed in this slot."
    ))
  }
  hide_expr
}

# `ppLayerOff(layer_expr, hide = TRUE)` at a layer position: validate
# `hide`, then translate the inner layer expression through the normal
# `translate_plain_layer` path. Stamp `default_active = !hide` on the
# resulting `ptr_layer`. The wrapper itself never appears in the tree
# (ADR 0012 §1: tree is semantic, not syntactic — a `hide = FALSE`
# wrapper produces the same shape as the bare layer).
unwrap_pp_layer_off <- function(expr, op_hint = "|>") {
  args <- as.list(expr[-1L])
  if (length(args) < 1L) {
    abort_translate(paste0(
      "`ppLayerOff(layer_expr, hide = TRUE)` requires a layer ",
      "expression in position 1; got an empty call."
    ))
  }
  arg_names <- names(args) %||% rep_len("", length(args))
  # First positional arg is the layer expression.
  inner_idx <- which(!nzchar(arg_names))[1L]
  if (is.na(inner_idx)) {
    abort_translate(paste0(
      "`ppLayerOff(layer_expr, hide = TRUE)` requires the layer ",
      "expression as the first positional argument."
    ))
  }
  inner_expr <- args[[inner_idx]]
  hide <- validate_pp_off_hide(expr, "ppLayerOff", hide_position = 2L)
  layer <- translate_plain_layer(inner_expr, op_hint = op_hint)
  layer$default_active <- !isTRUE(hide)
  layer
}

# Returns the literal logical `switch_on` value (a length-1 non-NA logical)
# from a `ppVerbSwitch(...)` wrapper call, OR aborts with class
# `"ptr_translate_error"` when the slot is missing, non-literal, or not a
# length-1 logical. Accepts `switch_on` named or positional; `switch_on_position`
# is the positional slot (2 at pipeline-stage position after `.data` has been
# stripped by the lift's resugar step). Default is TRUE when the slot is
# omitted entirely — matches the function-body default of `ppVerbSwitch`.
# Mirrors `validate_pp_off_hide`'s shape verbatim so reviewers can diff-compare.
validate_pp_verb_switch_switch_on <- function(expr, switch_on_position) {
  args <- as.list(expr[-1L])
  arg_names <- names(args) %||% rep_len("", length(args))
  if ("switch_on" %in% arg_names) {
    sw_expr <- args[[which(arg_names == "switch_on")[[1L]]]]
  } else if (length(args) >= switch_on_position &&
             !nzchar(arg_names[[switch_on_position]])) {
    sw_expr <- args[[switch_on_position]]
  } else {
    return(TRUE)
  }
  if (!is.logical(sw_expr) || length(sw_expr) != 1L || is.na(sw_expr)) {
    abort_translate(paste0(
      "`ppVerbSwitch()` argument `switch_on` must be a length-1 logical ",
      "literal (TRUE or FALSE); got `", deparse(sw_expr)[[1L]],
      "`. The boot-state metadata is consumed at translate time so the ",
      "formula remains the single source of truth — variables and ",
      "non-literal expressions are not allowed in this slot."
    ))
  }
  sw_expr
}

# Returns the literal character `label` value (a length-1 non-NA character)
# from a `ppVerbSwitch(...)` wrapper call, OR `NULL` when the slot is
# omitted / explicitly `NULL`, OR aborts with class `"ptr_translate_error"`
# when the slot is non-literal or not a length-1 character. Accepts `label`
# named or positional; `label_position` is the positional slot (3 at
# pipeline-stage position after `.data` has been stripped). The label is
# UI-routing metadata consumed at translate time and so is restricted to
# literals for the same source-of-truth reason as `switch_on` / `hide`.
validate_pp_verb_switch_label <- function(expr, label_position) {
  args <- as.list(expr[-1L])
  arg_names <- names(args) %||% rep_len("", length(args))
  if ("label" %in% arg_names) {
    lab_expr <- args[[which(arg_names == "label")[[1L]]]]
  } else if (length(args) >= label_position &&
             !nzchar(arg_names[[label_position]])) {
    lab_expr <- args[[label_position]]
  } else {
    return(NULL)
  }
  if (is.null(lab_expr)) return(NULL)
  if (!is.character(lab_expr) || length(lab_expr) != 1L || is.na(lab_expr)) {
    abort_translate(paste0(
      "`ppVerbSwitch()` argument `label` must be a length-1 character ",
      "literal or NULL; got `", deparse(lab_expr)[[1L]],
      "`. The boot-state metadata is consumed at translate time so the ",
      "formula remains the single source of truth — variables and ",
      "non-literal expressions are not allowed in this slot."
    ))
  }
  lab_expr
}

# `ppVerbSwitch(.data, verb_expr, switch_on = TRUE, label = NULL)` at a
# pipeline-stage position (i.e. as a `parts$stages[[j]]$call` after the
# source has been split off, which strips the implicit `.data` slot — so
# the call we see here is `ppVerbSwitch(verb_expr, switch_on, label)`).
# Validate `switch_on` + `label`, translate the inner verb call through
# `translate_node`, and stamp three fields on the resulting `ptr_call`:
# `has_user_control = TRUE`, `default_stage_enabled = switch_on`,
# `stage_label = label`. Used inside `build_pipeline_from_lift`.
unwrap_pp_verb_switch_stage <- function(stage_call) {
  args <- as.list(stage_call[-1L])
  arg_names <- names(args) %||% rep_len("", length(args))
  inner_idx <- which(!nzchar(arg_names))[1L]
  if (is.na(inner_idx)) {
    abort_translate(paste0(
      "`ppVerbSwitch(.data, verb_expr, switch_on = TRUE, label = NULL)` ",
      "requires the verb expression as a positional argument."
    ))
  }
  verb_expr <- args[[inner_idx]]
  switch_on <- validate_pp_verb_switch_switch_on(stage_call,
                                                 switch_on_position = 2L)
  label <- validate_pp_verb_switch_label(stage_call, label_position = 3L)
  inner_node <- translate_node(verb_expr)
  if (!is_ptr_call(inner_node)) {
    abort_translate(paste0(
      "`ppVerbSwitch(verb_expr = )` must be a verb call (e.g. `mutate(...)`); ",
      "got `", deparse(verb_expr)[[1L]], "`."
    ))
  }
  inner_node$has_user_control <- TRUE
  inner_node$default_stage_enabled <- isTRUE(switch_on)
  inner_node$stage_label <- label
  inner_node
}

# Parse a placeholder call's argument list against its registry entry's
# schema (`entry$default_arg` + `entry$named_args`). Returns a list with
# fields `shared`, `default`, `named_args`. Aborts on positional args when
# the schema does not accept one, on unknown named args, and propagates
# validator-aborts unchanged. NEVER eval()s a placeholder's argument AST --
# validators receive the unevaluated language object.
extract_placeholder_args <- function(expr, entry) {
  empty <- list(shared = NULL, default = NULL, named_args = list())
  if (is.symbol(expr)) return(empty)
  if (length(expr) == 1L) return(empty)

  args      <- as.list(expr[-1L])
  arg_names <- names(args) %||% rep_len("", length(args))
  keyword   <- entry$keyword

  named_idx  <- nzchar(arg_names)
  named      <- args[named_idx]
  positional <- args[!named_idx]

  # Positional handling
  default <- NULL
  if (length(positional) >= 1L) {
    if (is.null(entry$default_arg)) {
      rlang::abort(paste0(
        "Placeholder `", keyword, "(...)` accepts only the named ",
        "`shared` argument; positional args are not allowed."
      ))
    }
    if (length(positional) > 1L) {
      rlang::abort(paste0(
        "Placeholder `", keyword, "(...)` accepts at most one positional ",
        "argument (the default); got ", length(positional), "."
      ))
    }
    default <- entry$default_arg(positional[[1L]])
  }

  # Named handling
  declared   <- names(entry$named_args %||% list())
  recognized <- c("shared", declared)
  unknown    <- setdiff(names(named), recognized)
  if (length(unknown) > 0L) {
    rlang::abort(paste0(
      "Placeholder `", keyword, "(...)` got unknown argument(s): ",
      paste(unknown, collapse = ", "), ". ",
      "Allowed: ", paste(recognized, collapse = ", "), "."
    ))
  }

  named_args_out <- list()
  for (nm in declared) {
    if (nm %in% names(named)) {
      validator <- entry$named_args[[nm]]
      named_args_out[[nm]] <- validator(named[[nm]])
    }
  }

  shared <- extract_shared_value(named[["shared"]], keyword)

  list(shared = shared, default = default, named_args = named_args_out)
}

# Validate the shared= named argument (existing semantics, lifted out so
# extract_placeholder_args stays readable). Returns the validated string
# or NULL when shared is not supplied.
extract_shared_value <- function(shared_ast, keyword) {
  if (is.null(shared_ast)) return(NULL)
  if (!is.character(shared_ast) || length(shared_ast) != 1L ||
      is.na(shared_ast) || !nzchar(shared_ast)) {
    rlang::abort(paste0(
      "`", keyword, "(shared = ...)` requires a single non-empty string."
    ))
  }
  shared_ast
}

# Detect formulas that pipe a `ggplot()` object into a subsequent call, e.g.
# `mtcars |> ggplot(aes(...)) |> geom_point()`. This evaluates to
# `geom_point(<ggplot>)` (the geom receives the plot as its first arg),
# which fails with "mapping must be created by aes()". We don't rewrite —
# just surface a diagnostic so the user knows to use `+` for layers. Returns
# a character vector of warning messages (one per detected misuse).
detect_pipe_layer_misuse <- function(root) {
  if (!is_ptr_root(root)) return(character())
  warnings <- character()
  for (layer in root$layers) {
    if (!is_ptr_layer(layer)) next
    warnings <- c(warnings, pipe_layer_warnings_for_layer(layer))
  }
  unique(warnings)
}

pipe_layer_warnings_for_layer <- function(layer) {
  out <- character()
  da <- layer$data_arg
  if (!is_ptr_pipeline(da)) return(out)
  stages <- da$stages
  ggplot_idx <- which(vapply(stages, is_ggplot_call_node, logical(1)))
  if (length(ggplot_idx) == 0L) return(out)
  trailing <- stages[seq_len(length(stages))[-seq_len(max(ggplot_idx))]]
  for (s in trailing) {
    nm <- pipe_stage_display_name(s)
    if (!is.null(nm)) out <- c(out, pipe_layer_misuse_msg(nm))
  }
  if (isTRUE(layer_is_piped(layer))) {
    nm <- layer$name %||% "<layer>"
    out <- c(out, pipe_layer_misuse_msg(nm))
  }
  out
}

is_ggplot_call_node <- function(node) {
  if (!is_ptr_call(node)) return(FALSE)
  head <- node$fun
  if (is.symbol(head)) return(identical(as.character(head), "ggplot"))
  if (is.call(head) && length(head) >= 3L &&
      is.symbol(head[[1L]]) &&
      as.character(head[[1L]]) %in% c("::", ":::") &&
      is.symbol(head[[3L]])) {
    return(as.character(head[[3L]]) == "ggplot")
  }
  FALSE
}

pipe_stage_display_name <- function(node) {
  if (!is_ptr_call(node)) return(NULL)
  head <- node$fun
  if (is.symbol(head)) return(as.character(head))
  if (is.call(head) && length(head) >= 3L &&
      is.symbol(head[[1L]]) &&
      as.character(head[[1L]]) %in% c("::", ":::") &&
      is.symbol(head[[3L]])) {
    return(paste0(as.character(head[[2L]]), "::", as.character(head[[3L]])))
  }
  NULL
}

pipe_layer_misuse_msg <- function(layer_name) {
  paste0(
    "It looks like `|>` is being used to add the `", layer_name,
    "()` layer to a `ggplot()`. ggplot layers must be added with `+`, ",
    "not `|>` -- try `... + ", layer_name, "()` instead."
  )
}

build_placeholder_node <- function(expr, ph) {
  role <- ph$entry$role
  switch(
    role,
    "value" = ptr_ph_value(
      keyword = ph$keyword, expr = expr, shared = ph$shared,
      default = ph$default, named_args = ph$named_args
    ),
    "consumer" = ptr_ph_data_consumer(
      keyword = ph$keyword, expr = expr, shared = ph$shared,
      default = ph$default, named_args = ph$named_args
    ),
    "source" = ptr_ph_data_source(
      keyword = ph$keyword, expr = expr, shared = ph$shared,
      default = ph$default, named_args = ph$named_args
    ),
    rlang::abort(paste0("Unknown placeholder role: ", role))
  )
}

# B1.a (ADR 0009 edge-case): when two placeholder occurrences share the
# same `shared` key but come from different roles (one ppVar consumer + one
# ppNum value, for instance), one shared widget can't faithfully back both.
# Abort at translate-time with a message naming the conflicting roles.
ptr_validate_shared_roles <- function(root) {
  occs <- collect_shared_occurrences(root)
  for (key in names(occs)) {
    bucket <- occs[[key]]
    roles <- vapply(bucket, function(n) {
      if (is_ptr_ph_data_consumer(n)) "consumer"
      else if (is_ptr_ph_data_source(n)) "source"
      else "value"
    }, character(1))
    if (length(unique(roles)) > 1L) {
      keywords <- vapply(bucket, function(n) n$keyword, character(1))
      rlang::abort(paste0(
        "Shared key `\"", key, "\"` is used by placeholders with ",
        "incompatible roles: ",
        paste0(keywords, "=", roles, collapse = ", "), ". ",
        "A shared widget can only back one role at a time."
      ))
    }
  }
  invisible(root)
}

# Walk the tree and bucket every placeholder occurrence (value, consumer,
# source) by its `$shared` key. Returns a named list of lists of nodes.
collect_shared_occurrences <- function(root) {
  phs <- ptr_collect(root, function(x) {
    is_ptr_placeholder(x) && !is.null(x$shared)
  })
  buckets <- list()
  for (n in phs) {
    key <- n$shared
    buckets[[key]] <- c(buckets[[key]] %||% list(), list(n))
  }
  buckets
}
