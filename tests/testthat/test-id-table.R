# Tests for ptr_id_table — the public id enumerator described in
# CONTEXT.md's L3 section. Coverage targets every role/kind/include_in_ui
# combination, the namespace prefix rule, determinism, parent_call
# derivation, and source/companion / stage-id / shared-key cases.

f_basic <- "ggplot(ppUpload, aes(x = ppVar, y = ppVar)) + geom_point(color = ppText)"
f_shared <- "ggplot(ppUpload, aes(x = ppVar(shared = \"xcol\"))) + geom_smooth()"
f_pipeline <- "ggplot(ppUpload |> head(ppNum), aes(x = ppVar))"

test_that("formula must be a single non-NA string", {
  expect_error(ptr_id_table(NULL), "single string")
  expect_error(ptr_id_table(NA_character_), "single string")
  expect_error(ptr_id_table(c("a", "b")), "single string")
})

test_that("id arg must be a single non-empty string when supplied", {
  expect_error(ptr_id_table(f_basic, id = ""), "non-empty")
  expect_error(ptr_id_table(f_basic, id = c("a", "b")), "non-empty")
  expect_error(ptr_id_table(f_basic, id = NA_character_), "non-empty")
})

test_that("returns a data.frame with the documented 10 columns in order", {
  out <- ptr_id_table(f_basic)
  expect_s3_class(out, "data.frame")
  expect_equal(
    names(out),
    c("id", "kind", "role", "scope", "include_in_ui",
      "layer", "keyword", "param", "parent_call", "shared")
  )
  expect_type(out$id, "character")
  expect_type(out$kind, "character")
  expect_type(out$role, "character")
  expect_type(out$scope, "character")
  expect_type(out$include_in_ui, "logical")
  expect_type(out$layer, "character")
  expect_type(out$keyword, "character")
  expect_type(out$param, "character")
  expect_type(out$parent_call, "character")
  expect_type(out$shared, "character")
})

test_that("output is deterministic across runs", {
  a <- ptr_id_table(f_basic)
  b <- ptr_id_table(f_basic)
  expect_equal(a, b)
})

test_that("kind / role values stay within the documented vocabulary", {
  out <- ptr_id_table(f_pipeline)
  expect_true(all(out$kind %in% c("input_widget", "output_slot")))
  expect_true(all(out$role %in% c(
    "placeholder", "source_companion",
    "layer_checkbox", "layer_subtab", "layer_content",
    "stage_enabled",
    "ptr_plot", "ptr_error", "ptr_code", "ptr_code_mode", "ptr_update_plot"
  )))
  expect_true(all(out$scope %in% c("instance", "global")))
})

test_that("static infrastructure rows are always present at the head", {
  out <- ptr_id_table(f_basic)
  expect_equal(
    out$role[seq_len(5)],
    c("ptr_update_plot", "ptr_plot", "ptr_error", "ptr_code", "ptr_code_mode")
  )
  expect_equal(out$id[seq_len(5)],
               c("ptr_update_plot", "ptr_plot", "ptr_error",
                 "ptr_code", "ptr_code_mode"))
})

test_that("id= prefixes instance-scoped rows and leaves no others", {
  out <- ptr_id_table(f_basic, id = "myplot")
  inst <- out[out$scope == "instance", , drop = FALSE]
  expect_true(all(startsWith(inst$id, "myplot-")))
  # bare-id form has no `myplot-` prefix
  out_bare <- ptr_id_table(f_basic)
  expect_false(any(startsWith(out_bare$id, "myplot-")))
})

test_that("every placeholder produces a sleeve (output_slot) + inner (input_widget) pair", {
  out <- ptr_id_table(f_basic)
  ph <- out[out$role == "placeholder", , drop = FALSE]

  sleeves <- ph[ph$kind == "output_slot", , drop = FALSE]
  inners  <- ph[ph$kind == "input_widget", , drop = FALSE]

  # All sleeves must include_in_ui = TRUE
  expect_true(all(sleeves$include_in_ui))
  # All inners must include_in_ui = FALSE
  expect_false(any(inners$include_in_ui))
  # Sleeve id is inner id + "_ui"
  expect_setequal(sleeves$id, paste0(inners$id, "_ui"))
})

test_that("ppUpload emits a source_companion row that include_in_ui = FALSE", {
  out <- ptr_id_table(f_basic)
  comp <- out[out$role == "source_companion", , drop = FALSE]
  expect_equal(nrow(comp), 1L)
  expect_false(comp$include_in_ui)
  expect_equal(comp$kind, "input_widget")
  expect_true(endsWith(comp$id, "_name"))
  # The corresponding source placeholder inner id exists
  expect_true(sub("_name$", "", comp$id) %in% out$id)
})

test_that("layer_checkbox is emitted only for layers with active_input_id (not ggplot)", {
  out <- ptr_id_table(f_basic)
  lcb <- out[out$role == "layer_checkbox", , drop = FALSE]
  expect_equal(lcb$layer, "geom_point")
  expect_equal(lcb$id, "geom_point_checkbox")
  expect_true(lcb$include_in_ui)
  expect_false("ggplot" %in% lcb$layer)
})

test_that("layer_content row is emitted for every layer", {
  out <- ptr_id_table(f_basic)
  lc <- out[out$role == "layer_content", , drop = FALSE]
  expect_setequal(lc$layer, c("ggplot", "geom_point"))
  expect_setequal(lc$id,
                  c("ptr_layer_content_ggplot",
                    "ptr_layer_content_geom_point"))
  expect_true(all(lc$kind == "output_slot"))
  expect_true(all(lc$include_in_ui))
})

test_that("layer_subtab is emitted only when the layer has pipeline placeholders", {
  # Literal data + literal-only children: no pipeline placeholders anywhere.
  out_no_pipe <- ptr_id_table(
    "ggplot(mtcars, aes(x = ppVar)) + geom_point(color = ppText)"
  )
  expect_false("layer_subtab" %in% out_no_pipe$role)

  # `ppUpload` as a layer's data_arg already counts as a pipeline placeholder
  # (the Data subtab hosts it), so f_basic DOES emit ggplot_subtab.
  out_basic <- ptr_id_table(f_basic)
  expect_true("ggplot_subtab" %in% out_basic$id)
  expect_false("geom_point_subtab" %in% out_basic$id)

  out_pipe <- ptr_id_table(f_pipeline)
  sub <- out_pipe[out_pipe$role == "layer_subtab", , drop = FALSE]
  expect_equal(sub$id, "ggplot_subtab")
  expect_equal(sub$layer, "ggplot")
})

test_that("stage_enabled rows appear once per pipeline stage", {
  out <- ptr_id_table(f_pipeline)
  st <- out[out$role == "stage_enabled", , drop = FALSE]
  expect_equal(nrow(st), 1L)
  expect_equal(st$id, "ggplot_2_stage_enabled")
  expect_true(st$include_in_ui)
  expect_equal(st$kind, "input_widget")
})

test_that("parent_call is the immediate enclosing call (aes / head / layer)", {
  out <- ptr_id_table(f_pipeline)
  ph <- out[out$role == "placeholder" & out$kind == "input_widget", , drop = FALSE]

  expect_equal(ph[ph$keyword == "ppUpload", "parent_call"], "ggplot")
  expect_equal(ph[ph$keyword == "ppNum",    "parent_call"], "head")
  expect_equal(ph[ph$keyword == "ppVar",    "parent_call"], "aes")

  out2 <- ptr_id_table(f_basic)
  ph2 <- out2[out2$role == "placeholder" & out2$kind == "input_widget", , drop = FALSE]
  # ppText is a direct child of geom_point() — parent_call = the layer name
  expect_equal(ph2[ph2$keyword == "ppText", "parent_call"], "geom_point")
})

test_that("repeated source references are de-duped (one sleeve / inner / companion per id)", {
  out <- ptr_id_table(f_basic)
  src <- out[out$keyword == "ppUpload" & out$role == "placeholder", , drop = FALSE]
  # one sleeve + one inner, not three of each (ppUpload referenced 3× in the tree)
  expect_equal(nrow(src), 2L)
  comp <- out[out$keyword == "ppUpload" & out$role == "source_companion", , drop = FALSE]
  expect_equal(nrow(comp), 1L)
})

test_that("shared placeholder rows carry shared key and use shared_<key> id", {
  out <- ptr_id_table(f_shared)
  sh <- out[!is.na(out$shared) & out$role == "placeholder", , drop = FALSE]
  expect_equal(nrow(sh), 2L)
  expect_setequal(sh$shared, "xcol")
  inner <- sh[sh$kind == "input_widget", "id"]
  sleeve <- sh[sh$kind == "output_slot", "id"]
  expect_equal(inner, "shared_xcol")
  expect_equal(sleeve, "shared_xcol_ui")
})

test_that("ggplot layer (no active_input_id) does not get a checkbox row", {
  out <- ptr_id_table(f_basic)
  expect_false("ggplot_checkbox" %in% out$id)
})

test_that("include_in_ui is FALSE only for inner placeholder widgets and source companions", {
  out <- ptr_id_table(f_basic)
  false_rows <- out[!out$include_in_ui, , drop = FALSE]
  expect_true(all(false_rows$role %in% c("placeholder", "source_companion")))
  expect_true(all(false_rows$kind == "input_widget"))
})

test_that("scope is only 'instance' under the single-formula contract", {
  # Forward-compat canary: the 'global' branch in ptr_id_table() will only
  # become reachable when multi-formula (ptr_shared) support extends this
  # helper. Until then every row must be instance-scoped — a future change
  # that silently flips a row to 'global' should fail this assertion and
  # be paired with an update to the roxygen 'Single-formula' section.
  for (formula in c(f_basic, f_shared, f_pipeline,
                    "ggplot(mtcars, aes(x = ppVar))")) {
    out <- ptr_id_table(formula)
    expect_equal(unique(out$scope), "instance")
  }
})
