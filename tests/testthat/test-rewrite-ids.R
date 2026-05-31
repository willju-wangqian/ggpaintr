# P4 — id-encoding. Raw ids of the form
# `<layer_name>_<underscore-joined-index-path>_<keyword>_<shared-or-NA>`.

test_that("P4.1 raw id format combines layer, path, keyword, shared", {
  r <- ptr_translate("ggplot(mtcars, aes(x = mpg)) + geom_point(color = ppText, size = ppNum)")
  texts <- find_nodes(r, function(x) is_ptr_ph_value(x) && x$keyword == "ppText")
  nums <- find_nodes(r, function(x) is_ptr_ph_value(x) && x$keyword == "ppNum")
  expect_match(texts[[1]]$id, "^geom_point_[0-9_]+_ppText_NA$")
  expect_match(nums[[1]]$id, "^geom_point_[0-9_]+_ppNum_NA$")
})

test_that("P4.2 ns_fn applied at UI emit (raw ids on tree, ns at render)", {
  ns <- shiny::NS("plot1")
  r <- ptr_translate(
    "ggplot(mtcars) + geom_point(size = ppNum)",
    ns_fn = ns
  )
  num <- find_nodes(r, function(x) is_ptr_ph_value(x) && x$keyword == "ppNum")[[1]]
  rendered <- ptr_render_id(num$id, ns)
  expect_match(rendered, "^plot1-")
})

test_that("P4.3 ns_fn = shiny::NS(NULL) is identity", {
  ns <- shiny::NS(NULL)
  r <- ptr_translate(
    "ggplot(mtcars) + geom_point(size = ppNum)",
    ns_fn = ns
  )
  num <- find_nodes(r, function(x) is_ptr_ph_value(x) && x$keyword == "ppNum")[[1]]
  rendered <- ptr_render_id(num$id, ns)
  expect_equal(rendered, num$id)
})

test_that("P4.4 distinct namespaces produce disjoint rendered ids", {
  ns1 <- shiny::NS("a")
  ns2 <- shiny::NS("b")
  r1 <- ptr_translate("ggplot(mtcars) + geom_point(size = ppNum)", ns_fn = ns1)
  r2 <- ptr_translate("ggplot(mtcars) + geom_point(size = ppNum)", ns_fn = ns2)
  n1 <- find_nodes(r1, function(x) is_ptr_ph_value(x) && x$keyword == "ppNum")[[1]]
  n2 <- find_nodes(r2, function(x) is_ptr_ph_value(x) && x$keyword == "ppNum")[[1]]
  rendered1 <- ptr_render_id(n1$id, ns1)
  rendered2 <- ptr_render_id(n2$id, ns2)
  expect_false(rendered1 == rendered2)
})

test_that("P4.5 shortcut id for upload derived from node id (registry shortcut=TRUE)", {
  r <- ptr_translate("ggplot(data = ppUpload)")
  src <- find_nodes(r, is_ptr_ph_data_source)[[1]]
  expect_equal(src$shortcut_id, paste0(src$id, "_shortcut"))
})

test_that("P4.8 non-function ns_fn rejected", {
  expect_error(ptr_translate("ggplot(mtcars) + geom_point()", ns_fn = "string"))
  expect_error(ptr_translate("ggplot(mtcars) + geom_point()", ns_fn = 42L))
  expect_error(ptr_translate("ggplot(mtcars) + geom_point()", ns_fn = NULL))
})

test_that("P4.9 input_spec returns raw ids regardless of ns", {
  r1 <- ptr_translate("ggplot(mtcars) + geom_point(size = ppNum)",
                      ns_fn = shiny::NS(NULL))
  r2 <- ptr_translate("ggplot(mtcars) + geom_point(size = ppNum)",
                      ns_fn = shiny::NS("plot1"))
  s1 <- ptr_runtime_input_spec(r1)
  s2 <- ptr_runtime_input_spec(r2)
  expect_equal(s1$input_id, s2$input_id)
  expect_false(any(grepl("^plot1-", s2$input_id)))
})

test_that("P4 placeholders carry param (enclosing arg name)", {
  r <- ptr_translate("ggplot(mtcars, aes(x = mpg)) + geom_point(color = ppText, size = ppNum)")
  text_node <- find_nodes(r, function(x) is_ptr_ph_value(x) && x$keyword == "ppText")[[1]]
  num_node <- find_nodes(r, function(x) is_ptr_ph_value(x) && x$keyword == "ppNum")[[1]]
  expect_equal(text_node$param, "color")
  expect_equal(num_node$param, "size")
})
