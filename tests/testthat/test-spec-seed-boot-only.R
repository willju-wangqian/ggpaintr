# Latent bug: a `spec=`-seeded consumer/value widget must honour the user's
# LIVE pick across an upstream-triggered re-render. The spec seed is a
# BOOT-ONLY hint (ADR 0012 / PLAN-01 "Bug B"): it wins on the first /
# first-populated render so `spec = list(<id> = ...)` lands, but
# `state$spec_seed[[id]]` is written once at boot and never cleared -- so the
# `seed %||% current` precedence on the `has_rendered == TRUE` branch made the
# seed permanently override the user's pick on every re-fire (snap-back).
#
# Three seams, increasing fidelity:
#   1. unit  -- `consumer_seed_decision()` in the REAL regime (cols > 0, seed
#               non-NULL, has_rendered = TRUE). Not a `cols = 0` degenerate
#               mock (the regime where the bug is invisible).
#   2. e2e-ish testServer -- shared consumer renderUI driven through a real
#               populated first render (seed wins) -> user edit -> upstream
#               re-fire (panel source flip, columns held stable) -> the picker
#               must keep the user's pick.
#   3. e2e-ish testServer -- value renderUI (`ptr_setup_value_uis`).
#   4. e2e-ish testServer -- custom source renderUI (`ptr_setup_source_uis`):
#               a `selected`-accepting source keyword seeded via `spec=`, then
#               edited, then re-rendered by a tree rebuild. The source binder
#               had NO has_rendered latch (it did `seed %||% current`
#               unconditionally), so the seed won on every re-render.
#
# All four widget kinds now share one boot-only precedence helper,
# `boot_seed_selected()`. Boot precedence (seed wins on first render) is
# asserted in every scenario so the fix cannot regress the ADR-0012 boot race.


# ---- Seam 1: the decision helper, in the real regime ----------------------

test_that("consumer_seed_decision: post-boot re-render keeps the user pick, not the boot seed", {
  cols <- c("Species", "body_mass_g", "island")

  # Boot / first-populated render (has_rendered = FALSE): seed must win so a
  # spec= entry lands. This is the boot race the seed-first order protects.
  boot <- ggpaintr:::consumer_seed_decision(
    has_rendered = FALSE, seed = "Species", current = NULL,
    default = NULL, cols = cols
  )
  expect_identical(boot$selected, "Species")
  expect_true(boot$mark_rendered)

  # Post-boot re-render (has_rendered = TRUE): the user has since picked
  # "body_mass_g". The never-cleared boot seed must NOT override it.
  live <- ggpaintr:::consumer_seed_decision(
    has_rendered = TRUE, seed = "Species", current = "body_mass_g",
    default = NULL, cols = cols
  )
  expect_identical(
    live$selected, "body_mass_g",
    label = "post-boot re-render keeps the user's live pick (not the boot seed)"
  )

  # User-emptied widget stays empty even with a seed present (no snap-back to
  # the seed when current is character(0)).
  emptied <- ggpaintr:::consumer_seed_decision(
    has_rendered = TRUE, seed = "Species", current = character(0),
    default = NULL, cols = cols
  )
  expect_identical(emptied$selected, character(0))
})


# ---- Seam 1b: post-boot new-source clear ----------------------------------
#
# ADR 0025 contract (ii), user-locked 2026-05-28: when a NEW source arrives
# AFTER boot (a file uploaded via ppUpload's fileInput, or the shortcut
# renamed to a different dataset), the consumer picker is CLEARED -- a new
# upload is new work, so neither the formula default NOR a stale prior pick
# carries onto it. The stateful "did the source identity change since the last
# render" detection lives in the caller (a per-picker closure); the decision
# helper just takes the resulting boolean. At boot nothing clears (default
# seeds, spec overrides -- Seam 1) so this flag defaults to FALSE.
test_that("consumer_seed_decision: clear_for_new_source empties the picker, overriding all else", {
  cols <- c("mpg", "wt")

  # A boot-default that was echoed back as `current` (the case C carry bug)
  # is wiped on the new upload.
  carried <- ggpaintr:::consumer_seed_decision(
    has_rendered = TRUE, seed = NULL, current = "mpg",
    default = "mpg", cols = cols, clear_for_new_source = TRUE
  )
  expect_identical(carried$selected, character(0))
  expect_true(carried$mark_rendered)

  # A genuine prior user pick (case D) is also cleared -- "new upload = new
  # work" (user-locked rationale), so prior selection does not survive.
  prior_pick <- ggpaintr:::consumer_seed_decision(
    has_rendered = TRUE, seed = NULL, current = "wt",
    default = "mpg", cols = cols, clear_for_new_source = TRUE
  )
  expect_identical(prior_pick$selected, character(0))

  # Even a spec seed does not resurrect a value on a post-boot new source
  # (spec is boot-only).
  with_seed <- ggpaintr:::consumer_seed_decision(
    has_rendered = TRUE, seed = "wt", current = "wt",
    default = "mpg", cols = cols, clear_for_new_source = TRUE
  )
  expect_identical(with_seed$selected, character(0))
})

test_that("consumer_seed_decision: clear_for_new_source defaults to FALSE (back-compat)", {
  cols <- c("mpg", "wt")
  base     <- ggpaintr:::consumer_seed_decision(TRUE, NULL, "wt", "mpg", cols)
  explicit <- ggpaintr:::consumer_seed_decision(
    TRUE, NULL, "wt", "mpg", cols, clear_for_new_source = FALSE
  )
  expect_identical(base, explicit)
  expect_identical(base$selected, "wt")
})


# ---- Shared setup for the testServer scenarios (mirrors the panel-sources
# ---- dep test's helper) ----------------------------------------------------

ssbo_consumer_setup <- function(formula) {
  tree <- ggpaintr:::ptr_translate(formula)
  list(
    tree         = tree,
    resolutions  = ggpaintr:::ptr_resolve_shared_consumers(tree),
    rep_nodes    = ggpaintr:::shared_consumer_representatives(tree),
    cid          = ggpaintr:::canonical_shared_id("col"),
    out_id       = ggpaintr:::consumer_output_id(
      ggpaintr:::canonical_shared_id("col")
    )
  )
}


# ---- Seam 2: shared consumer picker, full renderUI re-fire ----------------

test_that("shared consumer: spec seed wins at boot, user pick persists across an upstream re-render", {
  s <- ssbo_consumer_setup(
    'ggplot(ppUpload(shared = "ds"), aes(x = ppVar(shared = "col"))) + geom_point()'
  )
  cid <- s$cid

  # spec = list(<cid> = "cyl") -> apply_spec_at_boot would write this into the
  # host's spec_seed env. We hand the binder that env directly via spec_seed=.
  seed_env <- new.env(parent = emptyenv())
  seed_env[[cid]] <- "cyl"

  # panel-owned source: flipping the reactiveVal re-fires the consumer
  # renderUI. We flip between two frames with IDENTICAL columns (mtcars and
  # head(mtcars)) so the user's pick stays a valid column across the re-fire --
  # a dataset swap would drop the pick via intersect() and confound the test.
  panel_rv <- shiny::reactiveVal(mtcars)
  panel_sources <- list(shared_ds = shiny::reactive(panel_rv()))

  captured <- list()
  invoke_spy <- function(node, ui_text, layer_name, ns_fn, extra,
                         param_override = NULL, label_suffix = NULL,
                         label_override = NULL, ...) {
    captured[[length(captured) + 1L]] <<- extra
    shiny::tags$div(id = node$id, class = "stub-picker")
  }
  testthat::local_mocked_bindings(invoke_build_ui = invoke_spy, .package = "ggpaintr")

  server <- function(input, output, session) {
    ggpaintr:::ptr_bind_shared_consumer_uis(
      output = output, input = input, ns = identity,
      resolutions = s$resolutions,
      representative_nodes = s$rep_nodes,
      eval_env = globalenv(),
      panel_sources = panel_sources,
      spec_seed = seed_env
    )
    shiny::outputOptions(output, s$out_id, suspendWhenHidden = FALSE)
  }

  shiny::testServer(server, {
    # Name the panel source so its frame binds and the picker resolves a
    # POPULATED column list (mtcars). Without this we'd test the cols = 0
    # degenerate regime where the bug is invisible.
    session$setInputs(`shared_ds_shortcut` = "ds_df")
    session$flushReact()
    n1 <- length(captured)
    expect_gte(n1, 1L)
    # Sanity: the picker resolved real columns -> the has_rendered latch flipped.
    expect_gt(length(captured[[n1]]$cols), 0L)
    # Boot precedence (ADR 0012): the spec seed wins on the first populated render.
    expect_identical(
      captured[[n1]]$selected, "cyl",
      label = "spec seed wins at boot (first populated render)"
    )

    # User picks a different (valid) column.
    session$setInputs(`shared_col` = "hp")
    # Upstream re-render: same columns, fresh object -> renderUI re-fires.
    panel_rv(head(mtcars))
    session$flushReact()
    n2 <- length(captured)
    expect_gt(n2, n1)
    last <- captured[[n2]]
    expect_true("selected" %in% names(last))
    expect_identical(
      last$selected, "hp",
      label = "post-edit upstream re-render keeps the user's pick (not the boot seed)"
    )
  })
})


# ---- Seam 3: value widget, full renderUI re-fire --------------------------

test_that("value widget: spec seed wins at boot, user value persists across an upstream re-render", {
  formula <- 'ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point(size = ppNum(3))'
  tree <- ggpaintr:::ptr_translate(formula)
  vnodes <- ggpaintr:::find_nodes(tree, ggpaintr:::is_ptr_ph_value)
  expect_gte(length(vnodes), 1L)
  raw_id <- vnodes[[1L]]$id

  seed_env <- new.env(parent = emptyenv())
  seed_env[[raw_id]] <- 9  # spec= boot value

  tree_rv  <- shiny::reactiveVal(tree)
  stage_rv <- shiny::reactiveVal(list())  # re-fire signal; value loop only deps on it

  state <- list(
    tree              = tree_rv,
    stage_enabled     = stage_rv,
    server_ns_fn      = identity,
    ui_ns_fn          = identity,
    effective_ui_text = NULL,
    spec_seed         = seed_env
  )

  captured <- list()
  invoke_spy <- function(node, ui_text, layer_name, ns_fn, extra,
                         param_override = NULL, label_suffix = NULL,
                         label_override = NULL, ...) {
    captured[[length(captured) + 1L]] <<- extra
    shiny::tags$div(id = node$id, class = "stub-value")
  }
  testthat::local_mocked_bindings(invoke_build_ui = invoke_spy, .package = "ggpaintr")

  server <- function(input, output, session) {
    ggpaintr:::ptr_setup_value_uis(state, input, output, session)
  }

  shiny::testServer(server, {
    session$flushReact()
    n1 <- length(captured)
    expect_gte(n1, 1L)
    # Boot precedence: first render seeds from spec.
    expect_identical(
      captured[[n1]]$selected, 9,
      label = "value widget spec seed wins at boot (first render)"
    )

    # User edits the value.
    do.call(session$setInputs, stats::setNames(list(4), raw_id))
    # Upstream re-render (layer toggle / tree rebuild signal).
    stage_rv(list(toggled = TRUE))
    session$flushReact()
    n2 <- length(captured)
    expect_gt(n2, n1)
    expect_identical(
      captured[[n2]]$selected, 4,
      label = "post-edit upstream re-render keeps the user's value (not the boot seed)"
    )
  })
})


# ---- Seam 4: custom source widget, full renderUI re-fire ------------------

test_that("source widget: spec seed wins at boot, user pick persists across an upstream re-render", {
  ggpaintr:::ptr_registry_clear()
  withr::defer(ggpaintr:::ptr_registry_clear())

  captured <- list()
  # A custom SOURCE keyword whose build_ui accepts `selected` (ppUpload does
  # not, so the built-in source can't exhibit this -- a custom source can).
  ppSeedSrc <- ggpaintr::ptr_define_placeholder_source(
    keyword      = "ppSeedSrc",
    build_ui     = function(node, label = NULL, selected = NULL, ...) {
      captured[[length(captured) + 1L]] <<- selected
      shiny::selectInput(
        node$id, label = label %||% "Src",
        choices = c("iris", "mtcars", "ToothGrowth"),
        selected = selected %||% node$default %||% "iris"
      )
    },
    resolve_data = function(value, ...) {
      if (is.null(value) || !nzchar(value)) return(NULL)
      get(value, envir = asNamespace("datasets"))
    },
    resolve_expr = function(value, node, ...) rlang::sym(value),
    runtime      = function(x, ...) get(x, envir = asNamespace("datasets")),
    default_arg  = ggpaintr::ptr_default_string()
  )

  formula <- 'ggplot(ppSeedSrc("iris"), aes(x = mpg, y = hp)) + geom_point()'
  tree   <- ggpaintr:::ptr_translate(formula)
  snodes <- ggpaintr:::find_nodes(tree, ggpaintr:::is_ptr_ph_data_source)
  expect_gte(length(snodes), 1L)
  raw_id <- snodes[[1L]]$id

  seed_env <- new.env(parent = emptyenv())
  seed_env[[raw_id]] <- "mtcars"  # spec= boot value (differs from formula "iris")

  tree_rv <- shiny::reactiveVal(tree)
  state <- list(
    tree              = tree_rv,
    server_ns_fn      = identity,
    ui_ns_fn          = identity,
    effective_ui_text = NULL,
    spec_seed         = seed_env,
    panel_sources     = list()
  )

  server <- function(input, output, session) {
    ggpaintr:::ptr_setup_source_uis(state, input, output, session)
  }

  shiny::testServer(server, {
    session$flushReact()
    n1 <- length(captured)
    expect_gte(n1, 1L)
    # Boot precedence: first render seeds from spec (not the formula's "iris").
    expect_identical(
      captured[[n1]], "mtcars",
      label = "source widget spec seed wins at boot (first render)"
    )

    # User picks a different dataset.
    do.call(session$setInputs, stats::setNames(list("ToothGrowth"), raw_id))
    # Upstream re-render: a tree rebuild re-fires the renderUI. The source
    # renderUI uses `state$tree()` only as a dependency trigger (it reads the
    # node from its closure), and reactiveVal compares with identical(), so we
    # tag a fresh attribute to force a genuinely different value -> invalidate.
    refired <- tree
    attr(refired, "ssbo_refire") <- 1L
    tree_rv(refired)
    session$flushReact()
    n2 <- length(captured)
    expect_gt(n2, n1)
    expect_identical(
      captured[[n2]], "ToothGrowth",
      label = "post-edit tree rebuild keeps the user's source pick (not the boot seed)"
    )
  })
})
