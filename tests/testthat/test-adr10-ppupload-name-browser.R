# test-adr10-ppupload-name-browser.R — shinytest2 driver for the
# adr10-ppupload-name fixture. Covers the browser-level BDD scenarios of
# PLAN-03 (ADR 0010): companion textInput seeded at boot and plot renders
# via caller-env auto-resolve without any file upload.
#
# ADR 0022: the previous preserve-mode round-trip assertions (formula text
# panel echoing `ppUpload(<companion-value>)`) were retired with the UI's
# preserve-mode choice. The underlying invariant — `ptr_render(...,
# preserve_placeholders = TRUE)` honours the snapshot stamped on a data-
# source companion — is preserved as a direct unit test below, which keeps
# the dormant render code covered without depending on the UI.
#
# Gate pattern + boot scaffolding follow CLAUDE.md "Browser e2e (shinytest2)
# — hard-won gotchas": app-dir + pkgload::load_all in the fixture, never
# get_values(), set placeholder inputs with wait_=FALSE then explicit Draw.

test_that("adr10-ppupload-name: companion textInput seeded, plot auto-resolves without upload", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")

  pkg <- normalizePath(test_path("..", ".."), mustWork = FALSE)
  skip_if(
    !file.exists(file.path(pkg, "DESCRIPTION")),
    "adr10 browser test needs the package source root (pkgload::load_all); absent under .Rcheck sandbox"
  )
  withr::local_envvar(GGP_PKG = pkg)

  app_dir <- test_path("fixtures", "vignette-apps", "adr10-ppupload-name")
  app <- suppressWarnings(
    shinytest2::AppDriver$new(
      app_dir,
      name = "adr10-ppupload-name",
      load_timeout = 60 * 1000,
      timeout = 30 * 1000
    )
  )
  withr::defer(app$stop())

  # Discover the companion id from the translated tree (same convention used
  # in app.R's formula): `ppUpload(penguins) |> dplyr::filter(...) |>
  # ggplot(aes(x = ppVar(bill_length_mm), y = ppVar(bill_depth_mm))) + geom_point()`.
  # The first ppUpload data-source node's shortcut_id is
  # paste0(node$id, "_shortcut") = "ggplot_1_ppUpload_NA_shortcut".
  shortcut_id <- "ggplot_1_ppUpload_NA_shortcut"

  # Sanity: standard ptr_app() DOM landmarks are present.
  expect_dom_id(app, "ptr_update_plot")
  expect_dom_id(app, "ptr_code")
  expect_dom_id(app, "ptr_code_mode")
  expect_dom_id(app, shortcut_id)

  # (a) Companion textInput is seeded with "penguins" at boot (PLAN-01).
  expect_equal(
    app$get_value(input = shortcut_id), "penguins",
    label = "companion textInput is seeded with bareword from formula"
  )

  # Drive the first render. ppVar pickers live under the layer's Data
  # subtab; their renderUI is suspended until shown -- but for THIS test the
  # var pickers are not user-driven, they're picked from the formula
  # bareword `bill_length_mm` / `bill_depth_mm` (the ppVar(default = <sym>)
  # path established in ADR 0009). A bare Draw click is enough.
  app$click("ptr_update_plot")
  app$wait_for_idle(timeout = 25 * 1000)

  # (c) Plot output is non-empty -- auto-resolve found `penguins` via the
  # eval_env parent chain without any file upload (Clarification C3).
  plot_html <- app$get_html("#ptr_plot")
  expect_true(
    !is.null(plot_html) && nzchar(plot_html),
    label = "plot output container is present after Draw"
  )
  expect_match(plot_html, "<img", fixed = TRUE)

  # (d) Narrowing fidelity (absorbed from test-ppupload-name-e2e.R:98 +
  # :75): the substituted code body MUST contain the literal
  # `dplyr::filter(species == "Adelie")` chained off the bare `penguins`
  # symbol. This pins the contract that test-ppupload-name-e2e.R:75
  # asserted at the R level (`ptr_substitute` emits bare symbol at the
  # data slot) AND :98 asserted at the eval level (live-eval narrows to
  # the Adelie row via the chained filter). The literal here proves the
  # filter call survived translate -> substitute with both its function
  # spelling AND its argument; a regression that dropped the filter
  # stage or rebuilt it from scratch would change this literal.
  code_text <- app$get_value(output = "ptr_code")
  expect_match(
    code_text, "penguins |>", fixed = TRUE,
    label = "substituted body uses bare `penguins` (no string literal, no internal upload id)"
  )
  expect_match(
    code_text,
    "dplyr::filter(species == \"Adelie\")",
    fixed = TRUE,
    label = "dplyr::filter narrowing call survives translate+substitute with its argument intact"
  )
})

# ADR 0010 PLAN-02 round-trip (ADR 0022 disposition): preserve-mode formula
# render still emits `ppUpload(<companion-value>)` when the companion's
# current value is stamped on the data-source node. The UI no longer
# surfaces this text, but the render path is still live and reachable
# (e.g. by a future ptr_inject_spec() helper). These assertions exercise
# ptr_render(... preserve_placeholders = TRUE) directly on a translated
# tree, replacing the pre-ADR-0022 browser-driven assertions.
test_that("adr10 PLAN-02 round-trip: preserve-mode render stamps companion bareword", {
  formula <- "ppUpload(penguins) |> dplyr::filter(species == \"Adelie\") |> ggplot(aes(x = ppVar(bill_length_mm), y = ppVar(bill_depth_mm))) + geom_point()"
  # ptr_translate(annotate = TRUE) already runs ptr_assign_ids internally,
  # so the tree's data-source node has the placeholder id stamped (and the
  # ppUpload shortcut_id `<id>_shortcut` derived) by the time we stamp picks.
  tree <- ggpaintr:::ptr_translate(formula)

  # Initial render with the companion seeded to "penguins" (matches the
  # boot-time companion textInput value the fixture asserts above).
  snap_penguins <- list(ggplot_1_ppUpload_NA_shortcut = "penguins")
  text_penguins <- ggpaintr:::ptr_render(
    ggpaintr:::stamp_current_pick_walk(tree, snap_penguins),
    preserve_placeholders = TRUE
  )
  expect_true(
    grepl("ppUpload(penguins)", text_penguins, fixed = TRUE),
    label = "preserve-mode render emits ppUpload(penguins) with seeded companion"
  )

  # After the user edits the companion to "iris", preserve-mode render
  # must reflect the new bareword and drop the old one.
  snap_iris <- list(ggplot_1_ppUpload_NA_shortcut = "iris")
  text_iris <- ggpaintr:::ptr_render(
    ggpaintr:::stamp_current_pick_walk(tree, snap_iris),
    preserve_placeholders = TRUE
  )
  expect_true(
    grepl("ppUpload(iris)", text_iris, fixed = TRUE),
    label = "preserve-mode render emits ppUpload(iris) after companion edit"
  )
  expect_false(
    grepl("ppUpload(penguins)", text_iris, fixed = TRUE),
    label = "preserve-mode render no longer mentions penguins after companion edit"
  )
})

# Empty-string and absent-key companion snapshots: when no current pick
# is stamped on the node, preserve-mode render emits the bare `ppUpload`
# symbol rather than `ppUpload()` parens (ADR 0012 follow-up,
# vignette-review commit 8235d8a). These assertions absorb the unit-level
# branches in test-ppupload-preserve-mode.R:35 / :82, which exercised the
# same stamp_current_pick_walk + render_placeholder_preserved code paths
# on synthetic ppupload_node()s.
test_that("adr10 PLAN-02 round-trip: empty / absent snapshot renders bare ppUpload (no parens)", {
  formula <- "ppUpload(penguins) |> dplyr::filter(species == \"Adelie\") |> ggplot(aes(x = ppVar(bill_length_mm), y = ppVar(bill_depth_mm))) + geom_point()"
  tree <- ggpaintr:::ptr_translate(formula)

  # Empty-string snapshot — .snapshot_value_is_set treats "" as not-set,
  # so current_pick stays NULL; render_placeholder_preserved emits the
  # bare placeholder symbol.
  text_empty <- ggpaintr:::ptr_render(
    ggpaintr:::stamp_current_pick_walk(
      tree, list(ggplot_1_ppUpload_NA_shortcut = "")
    ),
    preserve_placeholders = TRUE
  )
  expect_true(
    grepl("ppUpload", text_empty, fixed = TRUE),
    label = "empty snapshot still renders the placeholder identifier"
  )
  expect_false(
    grepl("ppUpload(", text_empty, fixed = TRUE),
    label = "empty companion value renders bare ppUpload (no parens)"
  )

  # Absent-key snapshot — stamp walker never enters the read branch;
  # current_pick stays NULL; same bare render.
  text_absent <- ggpaintr:::ptr_render(
    ggpaintr:::stamp_current_pick_walk(tree, list()),
    preserve_placeholders = TRUE
  )
  expect_false(
    grepl("ppUpload(", text_absent, fixed = TRUE),
    label = "absent companion key renders bare ppUpload (no parens)"
  )
})
