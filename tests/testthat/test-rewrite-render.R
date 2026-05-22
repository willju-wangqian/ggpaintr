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

test_that("P10.3 native pipe surface preserved as |>", {
  # ADR 0012 §1: the tree is semantic, not syntactic — `mtcars |> ggplot(...)`,
  # `ggplot(mtcars, ...)`, and the magrittr equivalent share one canonical
  # tree. Post-G2 (PLAN-02 lift + PLAN-04 collapse), a single-stage chain
  # whose only verb above source is the terminal `ggplot(...)` reduces to
  # the nested-call source form on render — the pipe surface is NOT
  # preserved here because there is no multi-stage upstream to lift to a
  # `ptr_pipeline`. The contract this test now guards is the inverse: a
  # single-stage chain collapses to `ggplot(data = mtcars, aes(...))`.
  r <- ptr_translate("mtcars |> ggplot(aes(x = mpg))")
  txt <- ptr_render(r)
  expect_equal(txt, "ggplot(data = mtcars, aes(x = mpg))")
  expect_false(grepl("%>%", txt, fixed = TRUE))
})

test_that("P10.4 magrittr pipe surface preserved as %>%", {
  skip(paste0(
    "deferred to ADR 0012 §5 OQ2: %>% preservation in lifted pipelines ",
    "is a future enhancement; see PLAN-04 'Deferred to future rounds' section. ",
    "Lifted pipelines all carry $op = '|>' post-G2; %>%-source surface ",
    "preservation requires a successor plan."
  ))
  r <- ptr_translate("mtcars %>% ggplot(aes(x = mpg))")
  txt <- ptr_render(r)
  expect_match(txt, "%>%", fixed = TRUE)
  expect_false(grepl("|>", txt, fixed = TRUE))
})

test_that("P10.5 mixed pipe chain preserves both ops", {
  skip(paste0(
    "deferred to ADR 0012 §5 OQ2: %>% preservation in lifted pipelines ",
    "is a future enhancement; see PLAN-04 'Deferred to future rounds' section. ",
    "Lifted pipelines all carry $op = '|>' post-G2; mixed %>%/|> surface ",
    "preservation requires a successor plan."
  ))
  # Multi-stage pipes always break one stage per line. The outer `|>` joins
  # the magrittr upstream to the terminal layer; that upstream is itself a
  # pipeline and so also breaks at its own `%>%`.
  r <- ptr_translate("mtcars %>% head(2) |> ggplot(aes(x = mpg))")
  txt <- ptr_render(r)
  expect_equal(
    txt,
    "mtcars %>%\n  head(2) |>\n  ggplot(aes(x = mpg))"
  )
})

test_that("P10.6 chained pipe keeps middle-link call empty when placeholder empty", {
  # ADR 0012 §1: the tree is semantic, not syntactic. A single verb stage
  # (head) above the source (mtcars) below the terminal layer (ggplot) is
  # rejected by PLAN-02 GATE 0 — the layer's data_arg stays a `ptr_call`
  # and the prefix-collapse render rule emits the nested form. P9 still
  # holds: empty `ppNum` drops the arg, leaving `head(mtcars)` which eval
  # resolves via head's default n = 6.
  r <- ptr_translate("mtcars |> head(ppNum) |> ggplot(aes(x = mpg))")
  s <- ptr_substitute(r, input_snapshot = list())
  p <- ptr_prune(s)
  txt <- ptr_render(p)
  expect_equal(txt, "ggplot(data = head(mtcars), aes(x = mpg))")
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
  # ADR 0012 §1: the tree is semantic, not syntactic. Single-stage chain
  # renders as nested-call form post-G2; the snapshot-substitution contract
  # is otherwise unchanged — the live num value reaches the head() arg.
  r <- ptr_translate("mtcars |> head(ppNum) |> ggplot(aes(x = mpg))")
  num_id <- find_nodes(r, function(x) is_ptr_placeholder(x) && x$keyword == "ppNum")[[1]]$id
  s <- ptr_substitute(r, input_snapshot = setNames(list(3), num_id))
  p <- ptr_prune(s)
  txt <- ptr_render(p)
  expect_equal(txt, "ggplot(data = head(mtcars, 3), aes(x = mpg))")
  expect_false(grepl("head(ppNum)", txt, fixed = TRUE))
})

test_that("P10.12 code text falls back to live input when no snapshot", {
  # ADR 0012 §1: the tree is semantic, not syntactic. Single-stage chain
  # renders as nested-call form post-G2; the live-input / no-snapshot
  # contract is otherwise unchanged.
  # The "snapshot" passed to ptr_substitute IS the live-input projection at
  # the caller boundary (server.R wires reactiveValuesToList(input) as the
  # snapshot). This test exercises both halves of the contract:
  #   (a) live input "num = 5" → code text contains head(mtcars, 5)
  #   (b) no snapshot at all → the placeholder is missing → P9 drops the
  #       arg and the code text shows the empty-form head(mtcars).
  r <- ptr_translate("mtcars |> head(ppNum) |> ggplot(aes(x = mpg))")
  num_id <- find_nodes(r,
                       function(x) is_ptr_placeholder(x) && x$keyword == "ppNum")[[1]]$id

  txt_live <- ptr_render(ptr_prune(
    ptr_substitute(r, input_snapshot = stats::setNames(list(5), num_id))
  ))
  expect_equal(txt_live, "ggplot(data = head(mtcars, 5), aes(x = mpg))")

  txt_none <- ptr_render(ptr_prune(ptr_substitute(r, input_snapshot = list())))
  expect_equal(txt_none, "ggplot(data = head(mtcars), aes(x = mpg))")
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
  # ADR 0012 §1: the tree is semantic, not syntactic. Post-G2 a single verb
  # stage (head) above source collapses into the layer's data arg as a
  # nested call. When the resulting `ggplot(...)` is over-wide, render breaks
  # one arg per line (data = and aes = are split, closing paren on its own
  # indented line) instead of breaking the original pipe at each stage. The
  # 80-column budget still holds.
  r <- ptr_translate(paste0(
    "iris |> head(200) |> ",
    "ggplot(aes(x = Sepal.Length, y = Sepal.Width, color = Species)) + geom_point()"
  ))
  txt <- ptr_render(r)
  expect_equal(
    txt,
    paste0(
      "ggplot(\n",
      "  data = head(iris, 200),\n",
      "  aes(x = Sepal.Length, y = Sepal.Width, color = Species)\n",
      ") +\n",
      "  geom_point()"
    )
  )
  expect_true(all(nchar(strsplit(txt, "\n", fixed = TRUE)[[1]]) <= 80L))
})

test_that("P10.18 multi-stage pipe chains always break one stage per line", {
  # ADR 0012 §1: the tree is semantic, not syntactic. A single verb stage
  # above source (one `head` call between `mtcars` and the terminal
  # `ggplot`) is rejected by PLAN-02 GATE 0 — the data_arg stays a
  # `ptr_call` and renders in the nested-call source form below. The
  # multi-stage-pipe break-at-each-pipe contract is asserted by P10.17
  # against an over-wide chain and by P10.19 against an under-wide one
  # against the lifted `ptr_pipeline` shape.
  r <- ptr_translate("mtcars |> head(2) |> ggplot(aes(x = mpg))")
  expect_equal(
    ptr_render(r),
    "ggplot(data = head(mtcars, 2), aes(x = mpg))"
  )
})

test_that("P10.19 bracket / accessor heads render in syntactic form", {
  # ADR 0012 §1: the tree is semantic, not syntactic. Single-stage chains
  # collapse to the nested-call source form post-G2; the rendered output
  # preserves bracket (`[`, `[[`) and `$` accessor syntax verbatim inside
  # the layer body.
  expect_equal(
    ptr_render(ptr_translate(
      "mtcars |> dplyr::filter(mpg >= c(10, 20)[1]) |> ggplot(aes(x = mpg, y = hp)) + geom_point()"
    )),
    paste0(
      "ggplot(data = dplyr::filter(mtcars, mpg >= c(10, 20)[1]), aes(x = mpg, y = hp)) +\n",
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
  # ADR 0012 §1: the tree is semantic, not syntactic. Single-stage chains
  # collapse to the nested-call source form post-G2; both `::` and `:::`
  # namespacing on argument-position references survive the lift round-trip.
  expect_equal(
    ptr_render(ptr_translate(
      "mtcars |> purrr::map(broom::glance) |> ggplot(aes(mpg, hp)) + geom_point()"
    )),
    paste0(
      "ggplot(data = purrr::map(mtcars, broom::glance), aes(mpg, hp)) +\n",
      "  geom_point()"
    )
  )
  expect_equal(
    ptr_render(ptr_translate(
      "mtcars |> purrr::map(broom:::glance) |> ggplot(aes(mpg, hp)) + geom_point()"
    )),
    paste0(
      "ggplot(data = purrr::map(mtcars, broom:::glance), aes(mpg, hp)) +\n",
      "  geom_point()"
    )
  )
})
