# Shared-coordinator `id` argument — multi-panel namespacing.
#
# Pins the BDD scenarios from
# dev/plans/shared-id-namespacing/01-shared-id-arg.html: when
# `ptr_shared(id = "<id>")` is supplied, every coordinator-owned id is
# prefixed `<id>-` (Shiny `NS()`-style); the default `id = NULL` keeps
# today's flat ids byte-for-byte.

render_html_str <- function(tag) {
  if (is.null(tag)) return("")
  paste(as.character(htmltools::renderTags(tag)$html), collapse = "\n")
}

# Two formulas that both reference shared key "k" (so it becomes a panel
# key). Used by the default-vs-prefixed scenarios. We use `ppNum(shared=)`
# so the rendered widget input id (`shared_k`) lands directly in the panel
# HTML -- consumer (`ppVar`) widgets sit inside a dynamic `uiOutput`
# container (`shared_k_ui`) and bind only at server runtime.
f_k1 <- 'ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point(alpha = ppNum(shared = "k"))'
f_k2 <- 'ggplot(mtcars, aes(x = wt, y = drat)) + geom_point(size = ppNum(shared = "k"))'

test_that("default id keeps today's flat ids", {
  obj <- ptr_shared(c(f_k1, f_k2))
  expect_null(obj$id)
  html <- render_html_str(ptr_shared_panel(obj))
  # Value placeholders (ppNum) are rendered via a `uiOutput(<id>_ui)`
  # container in the static panel HTML; the widget input itself (id
  # `shared_k`) is inserted at server runtime. The static panel emits the
  # `_ui` container and the panel-owned button/errors ids.
  expect_match(html, "id=\"shared_k_ui\"", fixed = TRUE)
  expect_match(html, "id=\"ptr_shared_draw_all\"", fixed = TRUE)
  expect_match(html, "id=\"ptr_shared_errors\"", fixed = TRUE)
  # No spurious prefixed variants.
  expect_false(grepl("panelA-", html, fixed = TRUE))
})

test_that("non-null id prefixes every panel-owned id", {
  obj <- ptr_shared(c(f_k1, f_k2), id = "panelA")
  expect_equal(obj$id, "panelA")
  html <- render_html_str(ptr_shared_panel(obj))
  expect_match(html, "id=\"panelA-shared_k_ui\"", fixed = TRUE)
  expect_match(html, "id=\"panelA-ptr_shared_draw_all\"", fixed = TRUE)
  expect_match(html, "id=\"panelA-ptr_shared_errors\"", fixed = TRUE)
  # The bare ids must not appear when a prefix is active.
  expect_false(grepl("id=\"shared_k_ui\"", html, fixed = TRUE))
  expect_false(grepl("id=\"ptr_shared_draw_all\"", html, fixed = TRUE))
  expect_false(grepl("id=\"ptr_shared_errors\"", html, fixed = TRUE))
})

test_that("server reads inputs at the prefixed slot", {
  obj <- ptr_shared(c(f_k1, f_k2), id = "panelA")
  shiny::testServer(function(input, output, session) {
    st <- ptr_shared_server(obj)
    session$setInputs(`panelA-shared_k` = 0.42,
                      `panelA-ptr_shared_draw_all` = 1L)
    expect_equal(shiny::isolate(st$shared$k()), 0.42)
    expect_equal(shiny::isolate(st$draw_trigger()), 1L)
    # Flat slots must NOT carry the value when a prefix is in force.
    session$setInputs(shared_k = 0.99)
    expect_equal(shiny::isolate(st$shared$k()), 0.42)
  }, {})
})

test_that("two coordinators in one session, overlapping keys, no collision", {
  panelA <- ptr_shared(c(f_k1, f_k2), id = "panelA")
  # Distinct formulas for panelB, same shared key "k" -> overlapping panel
  # key, but the namespacing prefix keeps the input slots disjoint.
  fB1 <- 'ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point(alpha = ppNum(shared = "k"))'
  fB2 <- 'ggplot(mtcars, aes(x = wt, y = drat)) + geom_point(size = ppNum(shared = "k"))'
  panelB <- ptr_shared(c(fB1, fB2), id = "panelB")
  shiny::testServer(function(input, output, session) {
    stA <- ptr_shared_server(panelA)
    stB <- ptr_shared_server(panelB)
    session$setInputs(`panelA-shared_k` = 0.25,
                      `panelB-shared_k` = 0.75)
    expect_equal(shiny::isolate(stA$shared$k()), 0.25)
    expect_equal(shiny::isolate(stB$shared$k()), 0.75)
  }, {})
})

test_that("shared-stage checkbox id is namespaced too", {
  # Orphan-stage formula: select() carries the only `ppVar(shared = "k")`,
  # so the panel renders a stage(verb) checkbox for key "k".
  orphan_f1 <- paste0(
    "mtcars |> dplyr::select(ppVar(shared = \"k\"), gear) |> ",
    "ggplot(aes(x = gear, y = mpg)) + geom_point()"
  )
  orphan_f2 <- paste0(
    "mtcars |> dplyr::select(ppVar(shared = \"k\"), gear) |> ",
    "ggplot(aes(x = gear, y = hp)) + geom_line()"
  )
  obj <- ptr_shared(c(orphan_f1, orphan_f2), id = "panelA")
  html <- render_html_str(ptr_shared_panel(obj))
  expect_match(html, "id=\"panelA-shared_k_stage_enabled\"", fixed = TRUE)
  expect_match(html, "id=\"panelA-shared_k_stage_enabled_stage_block\"",
               fixed = TRUE)
})

test_that("validation rejects bad ids", {
  expect_error(ptr_shared(c(f_k1, f_k2), id = ""), "id")
  expect_error(ptr_shared(c(f_k1, f_k2), id = "1bad"), "id")
  expect_error(ptr_shared(c(f_k1, f_k2), id = c("a", "b")), "id")
  expect_error(ptr_shared(c(f_k1, f_k2), id = NA_character_), "id")
  expect_error(ptr_shared(c(f_k1, f_k2), id = "has-dash"), "id")
})
