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


# ---- Seam 1c: a valid user pick survives a NON-LANDING formula default -----
#
# Bug (2026-05-29): a consumer whose formula default is NOT a column of the
# uploaded / shortcut-swapped dataset never latched `has_rendered`.
# `default_landed` was FALSE (the default is not in the new cols) and the first
# populated render came up unselected (no spec seed), so `mark_rendered` stayed
# FALSE *forever*. Every later render -- including the Update Plot click -- then
# ran the `has_rendered == FALSE` branch, which discards `current` in favour of
# the (empty) boot seed, so the user's pick was reset on every Update.
# Repro: geom_rug(data = ppUpload()) + ppVar(carb); type "iris" in the shortcut,
# pick Sepal.Length, click Update -> picker blanks. The `default_landed` gate
# (commit cddc46e) conflated "default not landable YET" (a transient worth
# deferring for) with "default will NEVER land on this dataset" (the swap case).
# Distinguisher: in the deferral case the user has not picked yet (current
# NULL); a genuine valid pick must be honoured now and must flip the latch.
test_that("consumer_seed_decision: a valid user pick survives a non-landing default before the latch flips", {
  iris_cols <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")

  # has_rendered still FALSE (the latch never flipped -- `carb` is not an iris
  # column), but the user HAS picked a valid column. The pick must be honoured
  # NOW (not discarded for the empty seed) AND must flip `has_rendered`.
  picked <- ggpaintr:::consumer_seed_decision(
    has_rendered = FALSE, seed = NULL, current = "Sepal.Length",
    default = "carb", cols = iris_cols
  )
  expect_identical(
    picked$selected, "Sepal.Length",
    label = "user pick honoured even though the formula default never lands"
  )
  expect_true(
    picked$mark_rendered,
    label = "a valid user pick flips has_rendered so the next render keeps it"
  )

  # Deferral the `default_landed` gate protects (derived-column default /
  # post-boot transient): user has NOT picked (current NULL) and the default is
  # not yet a column -> keep deferring (selected NULL -> omit -> re-inject the
  # default) and do NOT flip the latch.
  waiting <- ggpaintr:::consumer_seed_decision(
    has_rendered = FALSE, seed = NULL, current = NULL,
    default = "adj", cols = iris_cols
  )
  expect_null(waiting$selected)
  expect_false(waiting$mark_rendered)

  # A purely-stale `current` (no element valid for the new cols) is NOT a pick
  # -- behaves like the waiting case (default re-injects, latch stays down).
  stale <- ggpaintr:::consumer_seed_decision(
    has_rendered = FALSE, seed = NULL, current = "carb",
    default = "carb", cols = iris_cols
  )
  expect_null(stale$selected)
  expect_false(stale$mark_rendered)

  # Boot-only spec precedence preserved: a spec seed still wins on the first
  # render even if the user already has a (valid) pick.
  seeded <- ggpaintr:::consumer_seed_decision(
    has_rendered = FALSE, seed = "Species", current = "Sepal.Length",
    default = "carb", cols = iris_cols
  )
  expect_identical(seeded$selected, "Species")
})


# ---- Seam 1d: a spec seed naming a DERIVED column survives until it lands ---
#
# Bug (2026-05-29 #2): a consumer seeded over a column that does not yet exist
# in `cols` on the first render -- because an upstream `mutate(adj = ppExpr(...))`
# has not been evaluated until its ppExpr input binds -- lost the seed forever.
# `mark_rendered` flipped the `has_rendered` latch on the strength of
# `!is.null(selected)` (the seed is non-null), even though the downstream
# `intersect(selected, cols)` in `ptr_builtin_var_build_ui()` silently drops a
# seed whose column is absent. So the latch flipped on the empty first render;
# when `adj` finally appeared in `cols` (ncol 11 -> 12), `has_rendered` was
# already TRUE and `boot_seed_selected()` returned the empty `current`. The
# DEFAULT path already guards this via `default_landed`; the SEED path had no
# symmetric `seed_landed` gate. Repro:
#   aes(y = ppVar(adj)) over mutate(adj = ppExpr(mpg/wt + hp/10)),
#   spec = list(ggplot_1_2_ppVar_NA = "adj")  ->  y picker boots EMPTY.
# (e2e: test-spec-seed-derived-column-browser.R)
test_that("consumer_seed_decision: a spec seed for a derived column defers the latch until the column lands", {
  pre_cols  <- c("mpg", "wt", "hp")          # render 1: upstream mutate not yet evaluated
  post_cols <- c("mpg", "wt", "hp", "adj")   # render 2: `adj` has appeared

  # Render 1 -- `adj` not in cols yet. The seed is still returned (it will be
  # intersect-dropped downstream), but the latch must NOT flip: a seed that
  # cannot land is not "applied".
  r1 <- ggpaintr:::consumer_seed_decision(
    has_rendered = FALSE, seed = "adj", current = character(0),
    default = "adj", cols = pre_cols
  )
  expect_identical(r1$selected, "adj")
  expect_false(
    r1$mark_rendered,
    label = "latch must defer while the seeded column is absent from cols"
  )

  # Render 2 -- latch still down (r1 deferred), `adj` now present. The seed
  # lands and flips the latch.
  r2 <- ggpaintr:::consumer_seed_decision(
    has_rendered = FALSE, seed = "adj", current = character(0),
    default = "adj", cols = post_cols
  )
  expect_identical(r2$selected, "adj")
  expect_true(
    r2$mark_rendered,
    label = "a landed seed flips has_rendered so the pick persists"
  )

  # A never-landing seed (e.g. a typo) must not TRAP the picker: once the user
  # makes a genuine valid pick, the latch flips so their pick sticks next render.
  trapped <- ggpaintr:::consumer_seed_decision(
    has_rendered = FALSE, seed = "adjX", current = "mpg",
    default = "adjX", cols = post_cols
  )
  expect_true(
    trapped$mark_rendered,
    label = "a valid user pick flips the latch even under a never-landing seed"
  )
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
    out_id       = ggpaintr:::placeholder_output_id(
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
    default_arg  = ggpaintr::ptr_arg_string()
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
