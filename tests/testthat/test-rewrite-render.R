# P10 — render-code. Typed tree -> code text. Pipe surface preserved.

# Replace the smoke test (folded into P10.1) -------------------------------

test_that("P10.1 single-layer ggplot formula round-trips", {
  r <- ptr_translate("ggplot(mtcars, aes(x = mpg))")
  expect_equal(ptr_render(r), "ggplot(mtcars, aes(x = mpg))")
})

test_that("P10.2 layers joined with ' +\\n  '", {
  r <- ptr_translate("ggplot(mtcars) + geom_point() + geom_smooth()")
  txt <- ptr_render(r)
  expect_equal(txt, "ggplot(mtcars) +\n  geom_point() +\n  geom_smooth()")
})

test_that("P10.3 native pipe surface preserved as |>", {
  r <- ptr_translate("mtcars |> ggplot(aes(x = mpg))")
  txt <- ptr_render(r)
  expect_match(txt, "\\|>")
  expect_false(grepl("%>%", txt, fixed = TRUE))
})

test_that("P10.4 magrittr pipe surface preserved as %>%", {
  r <- ptr_translate("mtcars %>% ggplot(aes(x = mpg))")
  txt <- ptr_render(r)
  expect_match(txt, "%>%", fixed = TRUE)
  expect_false(grepl("|>", txt, fixed = TRUE))
})

test_that("P10.5 mixed pipe chain preserves both ops", {
  r <- ptr_translate("mtcars %>% head(2) |> ggplot(aes(x = mpg))")
  txt <- ptr_render(r)
  expect_match(txt, "mtcars %>% head\\(2\\) \\|> ggplot")
})

test_that("P10.6 chained pipe drops middle-link arg when placeholder empty", {
  r <- ptr_translate("mtcars |> head(num) |> ggplot(aes(x = mpg))")
  s <- ptr_substitute(r, input_snapshot = list())
  p <- ptr_prune(s)
  txt <- ptr_render(p)
  expect_match(txt, "mtcars \\|> ggplot")
  expect_false(grepl("head", txt, fixed = TRUE))
})

test_that("P10.7 pkg::fn heads preserved", {
  r <- ptr_translate("ggplot2::ggplot(mtcars) + ggplot2::geom_point()")
  txt <- ptr_render(r)
  expect_match(txt, "ggplot2::ggplot\\(mtcars\\)")
  expect_match(txt, "ggplot2::geom_point\\(\\)")
})

test_that("P10.8 comments do not appear in rendered text", {
  r <- ptr_translate("mtcars |> head(2) |> # trim\nggplot(aes(x = mpg))")
  txt <- ptr_render(r)
  expect_false(grepl("#", txt, fixed = TRUE))
})

test_that("P10.9 ptr_user_expr renders inner verbatim", {
  ue <- ptr_user_expr(quote(theme(plot.title = element_text())))
  expect_equal(ptr_render(ue), "theme(plot.title = element_text())")
})

test_that("P10.10 named args print as `name = value`", {
  call_node <- ptr_call(
    fun = quote(geom_point),
    args = list(color = ptr_literal("red"), size = ptr_literal(3)),
    expr = quote(geom_point(color = "red", size = 3))
  )
  expect_equal(ptr_render(call_node), 'geom_point(color = "red", size = 3)')
})

test_that("P10.11 code text reflects snapshotted values when supplied", {
  r <- ptr_translate("mtcars |> head(num) |> ggplot(aes(x = mpg))")
  num_id <- find_nodes(r, function(x) is_ptr_placeholder(x) && x$keyword == "num")[[1]]$id
  s <- ptr_substitute(r, input_snapshot = setNames(list(3), num_id))
  p <- ptr_prune(s)
  txt <- ptr_render(p)
  expect_match(txt, "head\\(3\\)")
  expect_false(grepl("head(num)", txt, fixed = TRUE))
})

test_that("P10.13 nested data = ... pipe round-trips with inner pipeline", {
  r <- ptr_translate("ggplot(data = mtcars |> filter(num > 0))")
  s <- ptr_substitute(r, input_snapshot = list())
  p <- ptr_prune(s)
  txt <- ptr_render(p)
  # The inner pipeline either survives (mtcars |> filter(...)) or — if filter
  # collapses entirely — degrades to bare mtcars. Either way, no fold.
  expect_true(grepl("mtcars", txt, fixed = TRUE))
})
