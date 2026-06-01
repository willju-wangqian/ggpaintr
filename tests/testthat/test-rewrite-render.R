# P10 — render-code. Typed tree -> code text. Pipe surface preserved.

# Replace the smoke test (folded into P10.1) -------------------------------

test_that("P10.1 single-layer ggplot formula round-trips", {
  r <- ptr_translate("ggplot(mtcars, aes(x = mpg))")
  # data_arg is re-emitted named (`data = ...`) so the code is
  # round-trippable for layers whose first positional formal is `mapping`.
  expect_equal(ptr_render(r), "ggplot(data = mtcars, aes(x = mpg))")
})

test_that("P10.1c geom_* layer data renders as `data = ...`", {
  r <- ptr_translate("ggplot() + geom_point(data = iris, aes(x = Sepal.Length))")
  expect_match(ptr_render(r), "geom_point(data = iris, aes(x = Sepal.Length))",
               fixed = TRUE)
})

test_that("P10.2 layers joined with ' +\\n  '", {
  r <- ptr_translate("ggplot(mtcars) + geom_point() + geom_smooth()")
  txt <- ptr_render(r)
  expect_equal(txt, "ggplot(data = mtcars) +\n  geom_point() +\n  geom_smooth()")
})

test_that("P10.3 zero-verb chain (bare source piped into terminal) collapses to nested", {
  # ADR 0012 §1: the tree is semantic, not syntactic. `mtcars |> ggplot(...)`,
  # `ggplot(mtcars, ...)`, and the magrittr equivalent share one canonical
  # tree. There is no verb stage above the source — the layer's data_arg is
  # the bare-symbol `mtcars` (a `ptr_literal`), which `ptr_classify_calls`
  # leaves untouched (its `!is_ptr_call(da)` early-out). Renders as the
  # nested-call source form.
  r <- ptr_translate("mtcars |> ggplot(aes(x = mpg))")
  txt <- ptr_render(r)
  expect_equal(txt, "ggplot(data = mtcars, aes(x = mpg))")
  expect_false(grepl("%>%", txt, fixed = TRUE))
})

test_that("P10.4 magrittr pipe surface preserved as %>%", {
  r <- ptr_translate("mtcars %>% ggplot(aes(x = mpg))")
  txt <- ptr_render(r)
  expect_match(txt, "%>%", fixed = TRUE)
  expect_false(grepl("|>", txt, fixed = TRUE))
})

test_that("P10.5 mixed pipe chain renders all-%>% under first-pipe-op rule", {
  # PLAN-01 (ADR 0012 §5 OQ2): the first-pipe-op rule disambiguates a
  # mixed-op formula by picking whichever of `%>%` / `|>` appears first
  # in the source string. Here `%>%` wins (column 8 vs `|>` at column 20),
  # so the whole rendered chain emits `%>%` between every stage.
  r <- ptr_translate("mtcars %>% head(2) |> ggplot(aes(x = mpg))")
  txt <- ptr_render(r)
  expect_equal(
    txt,
    "mtcars %>%\n  head(2) %>%\n  ggplot(aes(x = mpg))"
  )
})

test_that("P10.6 chained pipe keeps middle-link call empty when placeholder empty", {
  # ADR 0012 §1: a single verb stage (head) above source (mtcars) below the
  # terminal layer (ggplot) lifts to a canonical `ptr_pipeline`. P9 still
  # holds: empty `ppNum` drops the arg, leaving `head()` (no args) which eval
  # resolves via head's default n = 6.
  r <- ptr_translate("mtcars |> head(ppNum) |> ggplot(aes(x = mpg))")
  s <- ptr_substitute(r, input_snapshot = list())
  p <- ptr_prune(s)
  txt <- ptr_render(p)
  expect_equal(
    txt,
    "ggplot(\n  data = mtcars |>\n           head(),\n  aes(x = mpg)\n)"
  )
})

test_that("P10.7 pkg::fn heads preserved", {
  r <- ptr_translate("ggplot2::ggplot(mtcars) + ggplot2::geom_point()")
  txt <- ptr_render(r)
  expect_match(txt, "ggplot2::ggplot\\(mtcars\\)")
  expect_match(txt, "ggplot2::geom_point\\(\\)")
})

test_that("P10.7b canonical tree invariant across surface forms", {
  # PLAN-01 / ADR 0012 §1: `mtcars %>% ggplot(...)`, `mtcars |> ggplot(...)`,
  # and `ggplot(mtcars, ...)` must produce one canonical typed tree.
  # `source_pipe_op` is a render-time annotation only — it must not affect
  # the class chain at any walked node. SC8 asserts identical class chains
  # on root, layer[[1]], layer[[1]]$data_arg, layer[[1]]$children; the only
  # divergence is the `source_pipe_op` annotation slot.
  r_pct <- ptr_translate("mtcars %>% ggplot(aes(x = mpg))")
  r_pipe <- ptr_translate("mtcars |> ggplot(aes(x = mpg))")
  r_nest <- ptr_translate("ggplot(mtcars, aes(x = mpg))")

  expect_identical(class(r_pct), class(r_pipe))
  expect_identical(class(r_pipe), class(r_nest))

  l_pct <- r_pct$layers[[1L]]
  l_pipe <- r_pipe$layers[[1L]]
  l_nest <- r_nest$layers[[1L]]

  expect_identical(class(l_pct), class(l_pipe))
  expect_identical(class(l_pipe), class(l_nest))

  expect_identical(class(l_pct$data_arg), class(l_pipe$data_arg))
  expect_identical(class(l_pipe$data_arg), class(l_nest$data_arg))

  expect_identical(
    lapply(l_pct$children, class),
    lapply(l_pipe$children, class)
  )
  expect_identical(
    lapply(l_pipe$children, class),
    lapply(l_nest$children, class)
  )

  expect_identical(l_pct$source_pipe_op, "%>%")
  expect_identical(l_pipe$source_pipe_op, "|>")
  expect_identical(l_nest$source_pipe_op, "|>")
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
  # ADR 0012 §1: single-stage chain lifts to a canonical `ptr_pipeline` and
  # renders as the |> pipe form. The snapshot-substitution contract is
  # unchanged — the live num value reaches the head() arg.
  r <- ptr_translate("mtcars |> head(ppNum) |> ggplot(aes(x = mpg))")
  num_id <- find_nodes(r, function(x) is_ptr_placeholder(x) && x$keyword == "ppNum")[[1]]$id
  s <- ptr_substitute(r, input_snapshot = setNames(list(3), num_id))
  p <- ptr_prune(s)
  txt <- ptr_render(p)
  expect_equal(
    txt,
    "ggplot(\n  data = mtcars |>\n           head(3),\n  aes(x = mpg)\n)"
  )
  expect_false(grepl("head(ppNum)", txt, fixed = TRUE))
})

test_that("P10.12 code text falls back to live input when no snapshot", {
  # ADR 0012 §1: single-stage chain lifts to a canonical `ptr_pipeline`.
  # The "snapshot" passed to ptr_substitute IS the live-input projection at
  # the caller boundary (server.R wires reactiveValuesToList(input) as the
  # snapshot). This test exercises both halves of the contract:
  #   (a) live input "num = 5" → code text contains head(5) in the pipe
  #   (b) no snapshot at all → the placeholder is missing → P9 drops the
  #       arg and the pipe shows the empty-form head().
  r <- ptr_translate("mtcars |> head(ppNum) |> ggplot(aes(x = mpg))")
  num_id <- find_nodes(r,
                       function(x) is_ptr_placeholder(x) && x$keyword == "ppNum")[[1]]$id

  txt_live <- ptr_render(ptr_prune(
    ptr_substitute(r, input_snapshot = stats::setNames(list(5), num_id))
  ))
  expect_equal(
    txt_live,
    "ggplot(\n  data = mtcars |>\n           head(5),\n  aes(x = mpg)\n)"
  )

  txt_none <- ptr_render(ptr_prune(ptr_substitute(r, input_snapshot = list())))
  expect_equal(
    txt_none,
    "ggplot(\n  data = mtcars |>\n           head(),\n  aes(x = mpg)\n)"
  )
  expect_false(grepl("head(ppNum)", txt_none, fixed = TRUE))
})

test_that("P10.13 nested data = ... pipe round-trips with inner pipeline", {
  r <- ptr_translate("ggplot(data = mtcars |> filter(ppNum > 0))")
  s <- ptr_substitute(r, input_snapshot = list())
  p <- ptr_prune(s)
  txt <- ptr_render(p)
  # The inner pipeline either survives (mtcars |> filter(...)) or — if filter
  # collapses entirely — degrades to bare mtcars. Either way, no fold.
  expect_true(grepl("mtcars", txt, fixed = TRUE))
})

test_that("P10.14 short calls stay inline (under RENDER_WIDTH)", {
  r <- ptr_translate("ggplot(mtcars) + geom_point(aes(x = mpg, y = wt), size = 2)")
  expect_equal(
    ptr_render(r),
    "ggplot(data = mtcars) +\n  geom_point(aes(x = mpg, y = wt), size = 2)"
  )
})

test_that("P10.15 over-wide calls expand one argument per line", {
  r <- ptr_translate(paste0(
    "ggplot(diamonds) + geom_histogram(aes(x = carat, fill = cut, weight = price), ",
    "bins = 40, position = position_dodge2(preserve = \"single\", padding = 0.1))"
  ))
  txt <- ptr_render(r)
  expect_match(txt, "geom_histogram(\n    aes(x = carat, fill = cut, weight = price),\n",
               fixed = TRUE)
  expect_match(txt, "\n    bins = 40,\n", fixed = TRUE)
  expect_match(txt, "\n  )", fixed = TRUE)  # closing paren on its own line, at the call indent
  # every line stays within the 80-column budget
  expect_true(all(nchar(strsplit(txt, "\n", fixed = TRUE)[[1]]) <= 80L))
})

test_that("P10.16 one-sided formula renders without redundant parens", {
  r <- ptr_translate("ggplot(mtcars) + facet_wrap(~drv)")
  expect_match(ptr_render(r), "facet_wrap(~drv)", fixed = TRUE)
})

test_that("P10.17 over-wide pipe chain breaks at each pipe operator", {
  # ADR 0012 §1: every chain with ≥1 verb stage above source lifts to a
  # canonical `ptr_pipeline` and renders one stage per line. The
  # `ggplot(...)` call itself stays over-wide and additionally breaks at
  # its arg boundaries. 80-column budget still holds.
  r <- ptr_translate(paste0(
    "iris |> head(200) |> ",
    "ggplot(aes(x = Sepal.Length, y = Sepal.Width, color = Species)) + geom_point()"
  ))
  txt <- ptr_render(r)
  expect_equal(
    txt,
    paste0(
      "ggplot(\n",
      "  data = iris |>\n",
      "           head(200),\n",
      "  aes(x = Sepal.Length, y = Sepal.Width, color = Species)\n",
      ") +\n",
      "  geom_point()"
    )
  )
  expect_true(all(nchar(strsplit(txt, "\n", fixed = TRUE)[[1]]) <= 80L))
})

test_that("P10.18 multi-stage pipe chains always break one stage per line", {
  # ADR 0012 §1: a single verb stage above source (one `head` call between
  # `mtcars` and the terminal `ggplot`) lifts to a canonical `ptr_pipeline`
  # and renders one stage per line, even when the resulting `ggplot(...)`
  # call would otherwise fit on one line.
  r <- ptr_translate("mtcars |> head(2) |> ggplot(aes(x = mpg))")
  expect_equal(
    ptr_render(r),
    "ggplot(\n  data = mtcars |>\n           head(2),\n  aes(x = mpg)\n)"
  )
})

test_that("P10.19 bracket / accessor heads render in syntactic form", {
  # ADR 0012 §1: every chain with ≥1 verb stage above source lifts. The
  # second assertion's chain has zero verb stages (bare `mtcars |>` into
  # `ggplot`) — the data_arg is the bare-symbol source (a `ptr_literal`)
  # which `ptr_classify_calls` leaves untouched, so it renders nested.
  # Both cases preserve bracket (`[`, `[[`) and `$` accessor syntax verbatim.
  expect_equal(
    ptr_render(ptr_translate(
      "mtcars |> dplyr::filter(mpg >= c(10, 20)[1]) |> ggplot(aes(x = mpg, y = hp)) + geom_point()"
    )),
    paste0(
      "ggplot(\n",
      "  data = mtcars |>\n",
      "           dplyr::filter(mpg >= c(10, 20)[1]),\n",
      "  aes(x = mpg, y = hp)\n",
      ") +\n",
      "  geom_point()"
    )
  )
  expect_equal(
    ptr_render(ptr_translate(
      'mtcars |> ggplot(aes(x = mtcars$mpg, y = mtcars[["hp"]])) + geom_point()'
    )),
    paste0(
      "ggplot(data = mtcars, aes(x = mtcars$mpg, y = mtcars[[\"hp\"]])) +\n",
      "  geom_point()"
    )
  )
})

test_that("P10.20 namespaced reference as an argument renders as pkg::name", {
  # ADR 0012 §1: every chain with ≥1 verb stage above source lifts. Both
  # `::` and `:::` namespacing on argument-position references survive the
  # lift round-trip into the rendered pipeline form.
  expect_equal(
    ptr_render(ptr_translate(
      "mtcars |> purrr::map(stats::coef) |> ggplot(aes(mpg, hp)) + geom_point()"
    )),
    paste0(
      "ggplot(\n",
      "  data = mtcars |>\n",
      "           purrr::map(stats::coef),\n",
      "  aes(mpg, hp)\n",
      ") +\n",
      "  geom_point()"
    )
  )
  expect_equal(
    ptr_render(ptr_translate(
      "mtcars |> purrr::map(stats:::coef) |> ggplot(aes(mpg, hp)) + geom_point()"
    )),
    paste0(
      "ggplot(\n",
      "  data = mtcars |>\n",
      "           purrr::map(stats:::coef),\n",
      "  aes(mpg, hp)\n",
      ") +\n",
      "  geom_point()"
    )
  )
})
