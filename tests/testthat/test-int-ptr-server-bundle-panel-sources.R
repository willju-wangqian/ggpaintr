# Integration regression: the convenience wrapper ptr_server() must forward
# shared_state$panel_sources into the per-instance state dots (mirroring the
# bundle->dots block ptr_server_internal() already projects). Without this,
# the embedder path (ptr_app() -> ptr_server(formula, id, shared_state = b))
# strips panel_sources before ptr_server_internal() runs, and per-instance
# panel-shared substitution silently no-ops at the user-visible boundary.

test_that("ptr_server() projects shared_state$panel_sources into per-instance state", {
  skip_if_not_installed("shiny")

  r1 <- shiny::reactiveVal(mtcars)
  bundle <- new_ptr_shared_state(
    shared = list(),
    draw_trigger = NULL,
    shared_resolutions = list(),
    panel_sources = list(shared_ds = r1)
  )

  captured <- NULL
  server <- function(input, output, session) {
    captured <<- ptr_server(
      "ggplot(ppUpload(shared = 'ds'), ppNum())",
      id = NULL,
      shared_state = bundle
    )
  }
  shiny::testServer(server, {})

  expect_identical(captured$panel_sources$shared_ds, r1)
  expect_identical(shiny::isolate(captured$panel_sources$shared_ds()), mtcars)
})

test_that("explicit ... entry wins over shared_state for panel_sources", {
  skip_if_not_installed("shiny")

  r_bundle <- shiny::reactiveVal(mtcars)
  r_explicit <- shiny::reactiveVal(iris)
  bundle <- new_ptr_shared_state(
    shared = list(),
    draw_trigger = NULL,
    shared_resolutions = list(),
    panel_sources = list(shared_ds = r_bundle)
  )

  captured <- NULL
  server <- function(input, output, session) {
    captured <<- ptr_server(
      "ggplot(ppUpload(shared = 'ds'), ppNum())",
      id = NULL,
      shared_state = bundle,
      panel_sources = list(shared_ds = r_explicit)
    )
  }
  shiny::testServer(server, {})

  expect_identical(captured$panel_sources$shared_ds, r_explicit)
  expect_identical(shiny::isolate(captured$panel_sources$shared_ds()), iris)
})
