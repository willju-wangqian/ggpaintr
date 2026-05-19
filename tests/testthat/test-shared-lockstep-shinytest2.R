# Step 02 (#P2) -- DOM-level complement to the app-level testServer
# lockstep in test-shared-server-partition.R. testServer proves the
# reactive partition behaviourally; only a real browser can assert the
# physical DOM claim in the BDD: a cross-formula key has EXACTLY ONE input
# element, and a formula-local key never appears bare in the standalone
# panel (it lives namespaced under its owning module).
#
# shinytest2 is a Suggests-only, browser-backed dependency; this test
# skips cleanly where it (or Chrome) is unavailable, e.g. local runs
# without it installed. The behavioural invariant is already gated by the
# always-run testServer suite.

test_that("S-P2.D exactly one #shared_B; #shared_A only under its module", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")

  # An earlier `css =` test may have left a dead `ggpaintr-user-*` resource
  # path (tempdir gone, registration lingers). shinytest2 replays the
  # parent's resourcePaths() into the app subprocess and a dead one aborts
  # app startup -> this test errors order-dependently in the full suite.
  prune_dead_ggpaintr_resource_paths()

  f1 <- 'ggplot(mtcars) + geom_point(aes(x = var(shared = "B"), y = mpg), size = num(shared = "A"))'
  f2 <- 'ggplot(mtcars) + geom_point(aes(x = var(shared = "B"), y = hp))'
  obj <- ptr_shared(c(f1, f2))
  e <- new.env(parent = globalenv())

  ui <- shiny::fluidPage(
    ptr_shared_panel(obj),
    ptr_ui(f1, "p1"),
    ptr_ui(f2, "p2")
  )
  server <- function(input, output, session) {
    ss <- ptr_shared_server(obj, envir = e)
    ptr_server(f1, "p1", envir = e, shared_state = ss)
    ptr_server(f2, "p2", envir = e, shared_state = ss)
  }
  # shinytest2 statically scans the server fn for globals; it trips on
  # ggpaintr's recursive tree-walk locals (e.g. `rec`) and warns
  # harmlessly. The app itself runs fine -- suppress just this
  # introspection noise, scoped to driver creation.
  app <- suppressWarnings(shinytest2::AppDriver$new(
    shiny::shinyApp(ui, server),
    name = "shared-partition-lockstep"
  ))
  withr::defer(app$stop())

  count_id <- function(id) {
    app$get_js(sprintf(
      "document.querySelectorAll(%s).length",
      jsonlite::toJSON(paste0("#", id), auto_unbox = TRUE)
    ))
  }

  # Step 02 owns the SERVER partition only; the inline-section UI that
  # renders a formula-local key under its module is Step 04/05 (the Step
  # 02 plan explicitly constrains "No UI work here"). So the DOM facts
  # this step can assert are exactly the partition's two physical claims:
  #
  #   1. the cross-formula key has exactly one physical widget, and it is
  #      the single panel-owned one;
  #   2. the formula-local key is NOT surfaced in the standalone panel
  #      (it is server-owned by its module, not the panel).
  #
  # The complementary "formula-local key appears under its module" DOM
  # claim (BDD scenario 2, UI half) is deferred with Steps 04/05; the
  # server half (module self-binds it, re-renders only that module) is
  # proven by the always-run testServer S-P2.2.
  expect_equal(count_id("shared_B"), 1)
  expect_equal(count_id("shared_A"), 0)
})
