# P4 — id-encoding. Raw ids of the form
# `<layer_name>+<dot-joined-index-path>+<keyword>+<shared-or-NA>`.

test_that("P4.1 raw id format combines layer, path, keyword, shared", {
  r <- ptr_translate("ggplot(mtcars, aes(x = mpg)) + geom_point(color = text, size = num)")
  texts <- find_nodes(r, function(x) is_ptr_ph_value(x) && x$keyword == "text")
  nums <- find_nodes(r, function(x) is_ptr_ph_value(x) && x$keyword == "num")
  expect_match(texts[[1]]$id,
               "^geom_point\\+[0-9.]+\\+text\\+NA$")
  expect_match(nums[[1]]$id,
               "^geom_point\\+[0-9.]+\\+num\\+NA$")
})

test_that("P4.2 ns_fn applied at UI emit (raw ids on tree, ns at render)", {
  r <- ptr_translate(
    "ggplot(mtcars) + geom_point(size = num)",
    ns_fn = shiny::NS("plot1")
  )
  num <- find_nodes(r, function(x) is_ptr_ph_value(x) && x$keyword == "num")[[1]]
  rendered <- ptr_render_id(num$id, r$ns_fn)
  expect_match(rendered, "^plot1-")
})

test_that("P4.3 ns_fn = shiny::NS(NULL) is identity", {
  r <- ptr_translate(
    "ggplot(mtcars) + geom_point(size = num)",
    ns_fn = shiny::NS(NULL)
  )
  num <- find_nodes(r, function(x) is_ptr_ph_value(x) && x$keyword == "num")[[1]]
  rendered <- ptr_render_id(num$id, r$ns_fn)
  expect_equal(rendered, num$id)
})

test_that("P4.4 distinct namespaces produce disjoint rendered ids", {
  r1 <- ptr_translate("ggplot(mtcars) + geom_point(size = num)",
                      ns_fn = shiny::NS("a"))
  r2 <- ptr_translate("ggplot(mtcars) + geom_point(size = num)",
                      ns_fn = shiny::NS("b"))
  n1 <- find_nodes(r1, function(x) is_ptr_ph_value(x) && x$keyword == "num")[[1]]
  n2 <- find_nodes(r2, function(x) is_ptr_ph_value(x) && x$keyword == "num")[[1]]
  rendered1 <- ptr_render_id(n1$id, r1$ns_fn)
  rendered2 <- ptr_render_id(n2$id, r2$ns_fn)
  expect_false(rendered1 == rendered2)
})

test_that("P4.5 companion id for upload derived via registry's companion_id_fn", {
  r <- ptr_translate("ggplot(data = upload)")
  src <- find_nodes(r, is_ptr_ph_data_source)[[1]]
  expect_equal(src$companion_id, ptr_upload_name_id(src$id))
  expect_equal(src$companion_id, paste0(src$id, "_name"))
})

test_that("P4.6 layer-update-data id absent for non-pipeline layers", {
  r <- ptr_translate("ggplot(mtcars, aes(x = mpg)) + geom_point()")
  for (l in r$layers) {
    if (is_ptr_layer(l)) {
      expect_null(l$update_data_input_id)
    }
  }
})

test_that("P4.7 layer-update-data id present for pipeline layers", {
  r <- ptr_translate("mtcars |> filter(num > 0) |> ggplot(aes(x = var))")
  ggp <- r$layers[[1]]
  expect_equal(ggp$update_data_input_id, "ggplot_update_data")
})

test_that("P4.8 non-function ns_fn rejected", {
  expect_error(ptr_translate("ggplot(mtcars) + geom_point()", ns_fn = "string"))
  expect_error(ptr_translate("ggplot(mtcars) + geom_point()", ns_fn = 42L))
  expect_error(ptr_translate("ggplot(mtcars) + geom_point()", ns_fn = NULL))
})

test_that("P4.9 input_spec returns raw ids regardless of ns", {
  r1 <- ptr_translate("ggplot(mtcars) + geom_point(size = num)",
                      ns_fn = shiny::NS(NULL))
  r2 <- ptr_translate("ggplot(mtcars) + geom_point(size = num)",
                      ns_fn = shiny::NS("plot1"))
  s1 <- ptr_runtime_input_spec(r1)
  s2 <- ptr_runtime_input_spec(r2)
  expect_equal(s1$input_id, s2$input_id)
  expect_false(any(grepl("^plot1-", s2$input_id)))
})

test_that("P4 placeholders carry param (enclosing arg name)", {
  r <- ptr_translate("ggplot(mtcars, aes(x = mpg)) + geom_point(color = text, size = num)")
  text_node <- find_nodes(r, function(x) is_ptr_ph_value(x) && x$keyword == "text")[[1]]
  num_node <- find_nodes(r, function(x) is_ptr_ph_value(x) && x$keyword == "num")[[1]]
  expect_equal(text_node$param, "color")
  expect_equal(num_node$param, "size")
})
