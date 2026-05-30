# GREEN top-level regression guards for the partition_view refactor
# (ADR 0026 rank 11 / plan 0026-partition-view).
#
# COUNTERPART to the RED test-partition-view.R. The view refactor is
# behavior-PRESERVING: it centralises the three host_owned_keys derivations +
# the render_shared_section boolean behind one view, changing no observable
# top-level behavior. So these tests pin TODAY's surface behavior and must stay
# GREEN through the refactor; they are the regression net that flips RED if a
# surface miscomputes ownership (the 2026-05-22 partition-binder bug shape).
#
# Unlike the red-first contract tests, these are intentionally green now (they
# characterise behavior that already exists). Their TEETH are proven at
# integration time by mutation: set R/paintr-app.R:169 render_shared_section to
# FALSE, or app.R:339 host_owned_keys to a non-empty set, and the matching test
# below must turn RED. (That mutation belongs to the impl/integration phase, not
# here -- this file only establishes the green baseline.)
#
# Channel coverage -- the THREE surfaces the view absorbs:
#   * panel-host  (R/paintr-shared-ui.R:416)  -- already guarded GREEN by
#       test-shared-server-partition.R S-P2.4(b): panel_html owns "shared_B",
#       expect_no_match "shared_A"/"shared_C". NOT duplicated here.
#   * embed-instance (R/paintr-app.R:1164)    -- already guarded GREEN by
#       test-shared-server-partition.R S-P2.2 / S-P2.4(c): grid_html has
#       "plot_1-shared_A" but not "plot_2-shared_A"; formula-local "A" drives
#       only p1. NOT duplicated here.
#   * single-instance (R/paintr-app.R:169 render_shared_section=TRUE +
#       :339 host_owned_keys=character(0)) -- the gap with NO non-browser guard.
#       THIS FILE fills it.
#
# Faithful harness: ptr_app_components(f)$ui builds the REAL single-instance
# producer UI that ptr_app() ships (render_shared_section=TRUE, no coordinator).
#
# Traces to: plan Success Criterion "Behavioural invariant preserved
# (regression target)" + BDD S5 (single-instance facet) + CONTEXT.md
# "single-instance ... No coordinator, no panel -- ever".

# One formula; key "g" reused across two aes slots => single-instance, no
# coordinator. Per the contract this surface owns ALL shared keys and renders
# them in its INLINE shared section (never a standalone panel).
single_shared_formula <-
  'ggplot(mtcars) + geom_point(aes(x = ppVar(shared = "g"), y = mpg - ppVar(shared = "g")))'

test_that("REG-1 single-instance renders the shared widget in its inline section [guards render_shared_section=TRUE @ app.R:169]", {
  html <- paste(
    as.character(ptr_app_components(single_shared_formula)$ui),
    collapse = ""
  )
  # The single-instance inline shared widget id is the canonical `shared_g`
  # (ns identity -- no module prefix). It is emitted ONLY inside the inline
  # shared section, so its presence == the section rendered. If the producer
  # regressed render_shared_section to FALSE, this id would vanish.
  expect_match(html, "shared_g", fixed = TRUE)
})

test_that("REG-2 single-instance has NO standalone shared panel -- owns all inline [guards single-instance 'no coordinator, no panel -- ever']", {
  html <- paste(
    as.character(ptr_app_components(single_shared_formula)$ui),
    collapse = ""
  )
  # The coordinator panel's draw-all button id (`<id>_ptr_shared_draw_all`,
  # CONTEXT.md) exists ONLY when a standalone shared panel is built. A single
  # instance must never promote to a panel, so this id must be absent while the
  # inline `shared_g` widget is present.
  expect_match(html, "shared_g", fixed = TRUE)
  expect_no_match(html, "ptr_shared_draw_all", fixed = TRUE)
})

test_that("REG-3 single-instance ptr_app server binds every shared key without error [guards host_owned_keys=character(0) @ app.R:339]", {
  # host_owned_keys=character(0) means the single-instance binder owns (binds)
  # every shared consumer key locally. Driving the real producer server through
  # a flush must construct and bind silently -- a regressed non-empty
  # host_owned_keys would leave a declared key unbound (the silent bug class).
  parts <- ptr_app_components(single_shared_formula)
  expect_no_error(shiny::testServer(parts$server, {
    session$flushReact()
  }))
})
