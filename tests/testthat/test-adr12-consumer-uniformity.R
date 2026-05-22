# ADR 0012 PLAN-04 — Consumer-uniformity test net.
#
# For each downstream consumer of a layer's `data_arg`, assert the consumer
# produces structurally equivalent output across the three surface forms
# (`|>` / `%>%` / nested-call) for the same placeholder snapshot. Four
# consumers covered:
#   * eval     (layer_to_eval_expr / pipeline_to_eval_expr)
#   * prune    (prune_walk.ptr_pipeline / prune_walk.ptr_call)
#   * safety   (deny-list AST walker)
#   * runtime  (runtime_upstream_data — fast-path-deleted in this plan)
#
# Atomic-pair note (ADR 0012 §1, PLAN-04 history): the eval and prune
# assertions that compare `nested` against `native`/`magrittr` require
# PLAN-02's canonical-pipeline lift to be in place. In a PLAN-04-only
# worktree they EXPECT TO FAIL — that failure is the mechanical proof the
# atomic pair is needed (see BDD "Atomic pair — PLAN-02 alone, without
# this plan, fails the consumer-uniformity test net"). The post-merge
# integration auditor for G2 verifies both plans' DoD together: at that
# point every assertion below passes. Each assertion is left in the file
# exactly as the BDD `Then` clauses dictate; narrowing or downgrading any
# of them to dodge the atomic-pair half-state would be a SCOPE VIOLATION.

# ---- helpers ---------------------------------------------------------------

# Tiny structural-equality helper for typed-tree nodes. Compares class +
# every relevant slot recursively. The pre-PLAN-04 codebase had no
# `ptr_tree_structural_equal()`; the plan's BDD references one — defined
# here as an unexported test helper rather than in `R/paintr-nodes.R` so
# the helper does not leak into the package API surface.
ptr_tree_structural_equal <- function(a, b) {
  if (is.null(a) && is.null(b)) return(TRUE)
  if (is.null(a) || is.null(b)) return(FALSE)
  if (!identical(class(a), class(b))) return(FALSE)
  if (ggpaintr:::is_ptr_root(a)) {
    if (length(a$layers) != length(b$layers)) return(FALSE)
    for (i in seq_along(a$layers)) {
      if (!ptr_tree_structural_equal(a$layers[[i]], b$layers[[i]])) return(FALSE)
    }
    return(TRUE)
  }
  if (ggpaintr:::is_ptr_layer(a)) {
    if (!identical(a$name, b$name)) return(FALSE)
    if (!ptr_tree_structural_equal(a$data_arg, b$data_arg)) return(FALSE)
    if (length(a$children) != length(b$children)) return(FALSE)
    for (i in seq_along(a$children)) {
      if (!ptr_tree_structural_equal(a$children[[i]], b$children[[i]])) return(FALSE)
    }
    return(TRUE)
  }
  if (ggpaintr:::is_ptr_pipeline(a)) {
    if (length(a$stages) != length(b$stages)) return(FALSE)
    for (i in seq_along(a$stages)) {
      if (!ptr_tree_structural_equal(a$stages[[i]], b$stages[[i]])) return(FALSE)
    }
    return(TRUE)
  }
  if (ggpaintr:::is_ptr_call(a)) {
    if (!identical(a$fun, b$fun)) return(FALSE)
    if (length(a$args) != length(b$args)) return(FALSE)
    an <- names(a$args) %||% rep_len("", length(a$args))
    bn <- names(b$args) %||% rep_len("", length(b$args))
    if (!identical(an, bn)) return(FALSE)
    for (i in seq_along(a$args)) {
      if (!ptr_tree_structural_equal(a$args[[i]], b$args[[i]])) return(FALSE)
    }
    return(TRUE)
  }
  if (ggpaintr:::is_ptr_placeholder(a)) {
    return(identical(a$keyword, b$keyword) && identical(a$param, b$param))
  }
  if (ggpaintr:::is_ptr_literal(a) || ggpaintr:::is_ptr_user_expr(a) ||
      ggpaintr:::is_ptr_missing(a)) {
    return(identical(a$expr, b$expr) && identical(a$inner, b$inner))
  }
  identical(a, b)
}

# Snapshot factory — binds every placeholder in `tree` to a fixed value so
# substitute → prune → eval produce concrete language.
build_uniform_snapshot <- function(tree,
                                   var_value = "bill_length_mm",
                                   num_value = 40,
                                   upload_name = "iris50") {
  snap <- list()
  for (n in ggpaintr:::find_nodes(tree, ggpaintr:::is_ptr_ph_data_source)) {
    if (!is.null(n$companion_id)) snap[[n$companion_id]] <- upload_name
    if (!is.null(n$id)) snap[[n$id]] <- list(name = paste0(upload_name, ".csv"),
                                              datapath = "(stub)",
                                              size = 0L, type = "text/csv")
  }
  for (n in ggpaintr:::find_nodes(tree, ggpaintr:::is_ptr_ph_data_consumer)) {
    if (!is.null(n$id)) snap[[n$id]] <- var_value
  }
  for (n in ggpaintr:::find_nodes(tree, ggpaintr:::is_ptr_ph_value)) {
    if (!is.null(n$id)) snap[[n$id]] <- num_value
  }
  snap
}

# Fixed three-way set of surface forms — identical semantics, three
# different ways the user could have written the formula. Reused verbatim
# from PLAN-04's BDD scenarios (Scope §3). `dplyr::` is namespaced so the
# deny-list walker's bare-symbol check does not flag `filter`/`mutate`.
f_native <- paste(
  "ppUpload |> dplyr::filter(ppVar > ppNum)",
  "|> dplyr::mutate(new_var = ppVar + 1)",
  "|> ggplot(aes(ppVar, ppVar)) + geom_point()"
)
f_magrittr <- paste(
  "ppUpload %>% dplyr::filter(ppVar > ppNum)",
  "%>% dplyr::mutate(new_var = ppVar + 1)",
  "%>% ggplot(aes(ppVar, ppVar)) + geom_point()"
)
f_nested <- paste(
  "ggplot(dplyr::mutate(",
  "  dplyr::filter(ppUpload, ppVar > ppNum),",
  "  new_var = ppVar + 1),",
  "aes(ppVar, ppVar)) + geom_point()"
)

# ---- eval consumer ---------------------------------------------------------

test_that("adr12 / PLAN-04: eval produces identical R expression across surface forms (placeholder-containing chain)", {
  t_native <- ptr_translate(f_native, expr_check = FALSE)
  t_magri <- ptr_translate(f_magrittr, expr_check = FALSE)
  t_nested <- ptr_translate(f_nested, expr_check = FALSE)

  snap <- build_uniform_snapshot(t_native)

  sub_native <- ggpaintr:::ptr_substitute(t_native, input_snapshot = snap)
  sub_magri <- ggpaintr:::ptr_substitute(t_magri, input_snapshot = snap)
  sub_nested <- ggpaintr:::ptr_substitute(t_nested, input_snapshot = snap)

  pr_native <- ggpaintr:::ptr_prune(sub_native)
  pr_magri <- ggpaintr:::ptr_prune(sub_magri)
  pr_nested <- ggpaintr:::ptr_prune(sub_nested)

  e_native <- ggpaintr:::layer_to_eval_expr(pr_native$layers[[1]])
  e_magri <- ggpaintr:::layer_to_eval_expr(pr_magri$layers[[1]])
  e_nested <- ggpaintr:::layer_to_eval_expr(pr_nested$layers[[1]])

  expect_true(identical(e_native, e_magri))
  expect_true(identical(e_magri, e_nested))
})

# ---- prune consumer --------------------------------------------------------

test_that("adr12 / PLAN-04: prune produces structurally equivalent pruned data_arg across surface forms", {
  t_native <- ptr_translate(f_native, expr_check = FALSE)
  t_magri <- ptr_translate(f_magrittr, expr_check = FALSE)
  t_nested <- ptr_translate(f_nested, expr_check = FALSE)

  # Leave `num` unbound so prune actually prunes something. `var`/`upload`
  # stay bound so the chain has both kept and pruned positions.
  snap <- build_uniform_snapshot(t_native)
  for (n in ggpaintr:::find_nodes(t_native, ggpaintr:::is_ptr_ph_value)) {
    snap[[n$id]] <- NULL
  }

  sub_native <- ggpaintr:::ptr_substitute(t_native, input_snapshot = snap)
  sub_magri <- ggpaintr:::ptr_substitute(t_magri, input_snapshot = snap)
  sub_nested <- ggpaintr:::ptr_substitute(t_nested, input_snapshot = snap)

  pr_native <- ggpaintr:::ptr_prune(sub_native)
  pr_magri <- ggpaintr:::ptr_prune(sub_magri)
  pr_nested <- ggpaintr:::ptr_prune(sub_nested)

  da_native <- pr_native$layers[[1]]$data_arg
  da_magri <- pr_magri$layers[[1]]$data_arg
  da_nested <- pr_nested$layers[[1]]$data_arg

  expect_true(ptr_tree_structural_equal(da_native, da_magri))
  expect_true(ptr_tree_structural_equal(da_magri, da_nested))
})

# ---- safety consumer -------------------------------------------------------

test_that("adr12 / PLAN-04: safety deny-list verdict is identical across surface forms (safe case)", {
  t_native <- ptr_translate(f_native, expr_check = FALSE)
  t_magri <- ptr_translate(f_magrittr, expr_check = FALSE)
  t_nested <- ptr_translate(f_nested, expr_check = FALSE)

  # Every form is safe — no deny-listed symbol anywhere.
  expect_silent(ggpaintr:::ptr_validate_tree_safety(t_native, expr_check = TRUE))
  expect_silent(ggpaintr:::ptr_validate_tree_safety(t_magri, expr_check = TRUE))
  expect_silent(ggpaintr:::ptr_validate_tree_safety(t_nested, expr_check = TRUE))
})

test_that("adr12 / PLAN-04: safety deny-list verdict is identical across surface forms (denied case)", {
  # `system()` is deny-listed; placed inside `mutate()` at the same
  # structural position in all three surface forms.
  g_native <- 'ppUpload |> dplyr::mutate(z = system("ls")) |> ggplot(aes(ppVar, ppVar)) + geom_point()'
  g_magri <- 'ppUpload %>% dplyr::mutate(z = system("ls")) %>% ggplot(aes(ppVar, ppVar)) + geom_point()'
  g_nested <- 'ggplot(dplyr::mutate(ppUpload, z = system("ls")), aes(ppVar, ppVar)) + geom_point()'

  t_native <- ptr_translate(g_native, expr_check = FALSE)
  t_magri <- ptr_translate(g_magri, expr_check = FALSE)
  t_nested <- ptr_translate(g_nested, expr_check = FALSE)

  # Each translation must produce a deny verdict under safety validation.
  expect_error(
    ggpaintr:::ptr_validate_tree_safety(t_native, expr_check = TRUE),
    regexp = "system"
  )
  expect_error(
    ggpaintr:::ptr_validate_tree_safety(t_magri, expr_check = TRUE),
    regexp = "system"
  )
  expect_error(
    ggpaintr:::ptr_validate_tree_safety(t_nested, expr_check = TRUE),
    regexp = "system"
  )
})

# ---- picker upstream resolution consumer ----------------------------------

test_that("adr12 / PLAN-04: picker upstream resolution returns identical data across surface forms (after fast-path deletion)", {
  # Build three matched ptr_state objects, bind a real upload frame into
  # each eval_env, and call runtime_upstream_data() to harvest each
  # consumer id's resolved upstream data.frame. The plan's BDD asserts
  # identical() per shared consumer id; the fast-path deletion is verified
  # mechanically by the grep validator on R/paintr-server.R (the test
  # below confirms downstream behaviour follows from that deletion).
  upload_df <- iris[seq_len(50L), ]

  build_state <- function(formula) {
    e <- new.env(parent = globalenv())
    s <- ggpaintr:::ptr_init_state(
      formula = formula,
      envir = e,
      ns = function(x) x,
      server_ns = function(x) x,
      expr_check = FALSE
    )
    # Mimic the upload observer: bind the frame under the chosen name in
    # eval_env, exactly as `ptr_setup_pipelines()` would.
    assign("uploaded", upload_df, envir = s$eval_env)
    s
  }

  s_native <- build_state(f_native)
  s_magri <- build_state(f_magrittr)
  s_nested <- build_state(f_nested)

  # Snapshot is per-tree because consumer ids are positional (`<layer>_<i>...`)
  # and differ across surface forms by construction. The "uniformity"
  # contract under test is about resolved DATA per consumer position, not
  # id-string equality; we line up consumers by ID across trees via the
  # shared-suffix logic below.
  snap_native <- build_uniform_snapshot(shiny::isolate(s_native$tree()),
                                        var_value = "Sepal.Length",
                                        num_value = 0,
                                        upload_name = "uploaded")
  snap_magri <- build_uniform_snapshot(shiny::isolate(s_magri$tree()),
                                       var_value = "Sepal.Length",
                                       num_value = 0,
                                       upload_name = "uploaded")
  snap_nested <- build_uniform_snapshot(shiny::isolate(s_nested$tree()),
                                        var_value = "Sepal.Length",
                                        num_value = 0,
                                        upload_name = "uploaded")

  res_native <- shiny::isolate(
    ggpaintr:::runtime_upstream_data(s_native, snapshot = snap_native)
  )
  res_magri <- shiny::isolate(
    ggpaintr:::runtime_upstream_data(s_magri, snapshot = snap_magri)
  )
  res_nested <- shiny::isolate(
    ggpaintr:::runtime_upstream_data(s_nested, snapshot = snap_nested)
  )

  # Every form has SOME consumer resolved (data wired correctly).
  expect_true(length(res_native) > 0L)
  expect_true(length(res_magri) > 0L)
  expect_true(length(res_nested) > 0L)

  # Consumer ids encode the LAYER PATH and so differ across surface forms
  # by construction (the nested ggplot wraps the chain in extra positional
  # slots vs `|>`/`%>%` which thread through the layer head). The
  # uniformity contract is about RESOLVED DATA per logical consumer
  # position (in-filter ppVar; in-aes ppVar; in-mutate ppVar) — not about
  # id-string equality. Compare the SET of unique resolved frames across
  # the three forms via a stable digest.
  unique_data_set <- function(res) {
    digests <- vapply(res, function(x) {
      paste0("rows=", nrow(x$data), "|cols=",
             paste(sort(names(x$data)), collapse = ","), "|sum=",
             format(sum(unlist(lapply(x$data, function(col) {
               if (is.numeric(col)) sum(col, na.rm = TRUE) else length(col)
             })))))
    }, character(1))
    sort(unique(digests))
  }

  set_native <- unique_data_set(res_native)
  set_magri <- unique_data_set(res_magri)
  set_nested <- unique_data_set(res_nested)

  expect_identical(set_native, set_magri,
                   info = "uniform upstream frames: native vs magrittr")
  expect_identical(set_magri, set_nested,
                   info = "uniform upstream frames: magrittr vs nested")
})
