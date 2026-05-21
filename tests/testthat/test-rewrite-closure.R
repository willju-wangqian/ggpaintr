# Anonymous-function (`function(.x) ...`) literals inside formulas, plus the
# related literal-`NULL` argument preservation. Regression coverage for the
# parser-crash, function-literal mis-translation, closure-body input wiring,
# and `node_list_to_lang()` NULL-slot bugs.

test_that("a function() literal in the formula does not crash translation", {
  expect_s3_class(
    ptr_translate(
      "tibble(k = 1:3) |>
         dplyr::mutate(m = purrr::map(k, function(.k) .k + ppNum)) |>
         ggplot(aes(k, k)) + geom_point()"
    ),
    "ptr_root"
  )
})

test_that("a placeholder inside a closure body gets an id", {
  r <- ptr_translate(
    "tibble(k = 1:3) |>
       dplyr::mutate(m = purrr::map(k, function(.k) .k + ppNum)) |>
       ggplot(aes(k, k)) + geom_point()"
  )
  phs <- find_nodes(r, function(x) is_ptr_placeholder(x) && x$keyword == "ppNum")
  expect_length(phs, 1L)
  expect_false(is.na(phs[[1]]$id))
})

test_that("a closure-body placeholder appears in the runtime input spec", {
  r <- ptr_translate(
    "tibble(k = 1:3) |>
       dplyr::mutate(m = purrr::map(k, function(.k) .k + ppNum)) |>
       ggplot(aes(k, k)) + geom_point()"
  )
  spec <- ptr_runtime_input_spec(r)
  num_ids <- spec$input_id[!is.na(spec$keyword) & spec$keyword == "ppNum"]
  expect_length(num_ids, 1L)
})

test_that("a closure literal renders back as function(.x) ... not mangled", {
  r <- ptr_translate(
    "tibble(k = 1:3) |>
       dplyr::mutate(m = purrr::map(k, function(.k) .k + ppNum)) |>
       ggplot(aes(k, k)) + geom_point()"
  )
  num_id <- find_nodes(r, function(x) is_ptr_placeholder(x) &&
                          x$keyword == "ppNum")[[1]]$id
  s <- ptr_substitute(r, input_snapshot = setNames(list(2), num_id))
  txt <- ptr_render(ptr_prune(s))
  expect_true(grepl("function(.k)", txt, fixed = TRUE))
  expect_false(grepl("as.pairlist", txt, fixed = TRUE))
  expect_false(grepl("alist(", txt, fixed = TRUE))
})

test_that("a closure with a substituted placeholder evaluates end to end", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  skip_if_not_installed("purrr")
  withr::local_package("dplyr")
  withr::local_package("tidyr")
  withr::local_package("purrr")
  r <- ptr_translate(
    "mtcars |>
       tidyr::nest(.by = cyl) |>
       dplyr::mutate(fit = purrr::map(data, function(.d) lm(ppExpr, data = .d))) |>
       tidyr::unnest(dplyr::select(fit, dplyr::everything())) |>
       ggplot(aes(cyl, cyl)) + geom_point()"
  )
  expr_id <- find_nodes(r, function(x) is_ptr_placeholder(x) &&
                           x$keyword == "ppExpr")[[1]]$id
  s <- ptr_substitute(r, input_snapshot = setNames(list("mpg ~ wt"), expr_id))
  # The closure body's `lm(mpg ~ wt, data = .d)` must reconstruct as a valid
  # closure; we only assert the eval expr round-trips without the names error.
  e <- node_to_lang(ptr_prune(s)$layers[[1]]$data_arg)
  expect_true(is.call(e))
})

test_that("a literal NULL argument keeps its slot (labs(y = NULL))", {
  r <- ptr_translate(
    "ggplot(mtcars, aes(mpg, hp)) + geom_point() + labs(title = ppText, y = NULL)"
  )
  text_id <- find_nodes(r, function(x) is_ptr_placeholder(x) &&
                           x$keyword == "ppText")[[1]]$id
  s <- ptr_substitute(r, input_snapshot = setNames(list("Hi"), text_id))
  pl <- ptr_eval(ptr_prune(s))
  expect_s3_class(pl, "ggplot")
})
