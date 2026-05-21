# P7 — input-spec. One row per Shiny binding: placeholders, source
# companions, layer checkboxes, layer update-data buttons.

test_that("P7.1 one row per placeholder in formula order", {
  r <- ptr_translate("ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point(color = ppText)")
  spec <- ptr_runtime_input_spec(r)
  ph_rows <- spec[spec$role %in% "placeholder", ]
  expect_equal(nrow(ph_rows), 3L)
  expect_equal(ph_rows$keyword, c("ppVar", "ppVar", "ppText"))
})

test_that("P7.2 upload companion row follows upload row with same source_id", {
  r <- ptr_translate("ggplot(data = ppUpload)")
  spec <- ptr_runtime_input_spec(r)
  upload_rows <- spec[spec$keyword %in% "ppUpload", ]
  expect_equal(nrow(upload_rows), 2L)
  expect_equal(upload_rows$role, c("placeholder", "source_companion"))
  expect_equal(upload_rows$source_id[1], upload_rows$source_id[2])
})

test_that("P7.3 layer checkbox rows follow placeholder rows; ggplot has none", {
  r <- ptr_translate("ggplot(mtcars, aes(x = ppVar)) + geom_point() + geom_smooth()")
  spec <- ptr_runtime_input_spec(r)
  cb_rows <- spec[spec$role %in% "layer_checkbox", ]
  expect_equal(nrow(cb_rows), 2L)
  expect_equal(cb_rows$layer_name, c("geom_point", "geom_smooth"))
  ph_rows <- spec[spec$role %in% "placeholder", ]
  expect_true(max(which(spec$role %in% "placeholder")) <
                min(which(spec$role %in% "layer_checkbox")))
})

test_that("P7.5 empty placeholder formula returns 0-row data frame with all columns", {
  r <- ptr_translate("ggplot(mtcars, aes(x = mpg))")
  spec <- ptr_runtime_input_spec(r)
  expect_equal(nrow(spec), 0L)
  expect_equal(colnames(spec),
               c("input_id", "role", "layer_name", "keyword",
                 "param_key", "source_id", "shared"))
})

test_that("P7.6 deduplicated layer names preserved in rows", {
  r <- ptr_translate("ggplot() + geom_point() + geom_point() + geom_point(size = ppNum)")
  spec <- ptr_runtime_input_spec(r)
  num_rows <- spec[spec$keyword %in% "ppNum", ]
  expect_equal(num_rows$layer_name, "geom_point-3")
  cb <- spec[spec$role %in% "layer_checkbox", ]
  expect_equal(cb$layer_name, c("geom_point", "geom_point-2", "geom_point-3"))
})

test_that("P7.7 custom placeholder keywords surface", {
  suppressWarnings({
    ptr_define_placeholder_consumer(
      keyword = "numeric_col",
      build_ui = function(node, cols, ...) NULL,
      resolve_expr = function(value, node, ...) rlang::sym(value)
    )
  })
  on.exit(suppressWarnings(ptr_register_builtins()))
  r <- ptr_translate("ggplot(mtcars, aes(x = numeric_col))")
  spec <- ptr_runtime_input_spec(r)
  expect_true(any(spec$keyword %in% "numeric_col"))
})

test_that("P7.8 piped vs symbol-data formulas yield identical specs (shape)", {
  r1 <- ptr_translate("mtcars |> ggplot(aes(x = ppVar))")
  r2 <- ptr_translate("ggplot(mtcars, aes(x = ppVar))")
  s1 <- ptr_runtime_input_spec(r1)
  s2 <- ptr_runtime_input_spec(r2)
  expect_equal(s1$role, s2$role)
  expect_equal(s1$keyword, s2$keyword)
  expect_equal(s1$param_key, s2$param_key)
})
