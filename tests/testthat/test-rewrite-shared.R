# P3 — shared-binding. Groups placeholders by `shared` key; assigns one
# canonical id per group; rewrites every member's `id`.

test_that("P3.1 two var nodes with same shared key share one canonical id", {
  r <- ptr_translate(
    'ggplot(aes(x = var(shared = "axis"), y = var(shared = "axis"))) + geom_point()'
  )
  consumers <- find_nodes(r, is_ptr_ph_data_consumer)
  expect_equal(length(consumers), 2L)
  expect_equal(consumers[[1]]$id, consumers[[2]]$id)
  expect_equal(consumers[[1]]$id, "shared_axis")
})

test_that("P3.2 different shared keys remain distinct", {
  r <- ptr_translate(
    'ggplot(aes(x = var(shared = "x"), y = var(shared = "y"))) + geom_point()'
  )
  consumers <- find_nodes(r, is_ptr_ph_data_consumer)
  ids <- vapply(consumers, function(c) c$id, character(1))
  expect_equal(length(unique(ids)), 2L)
})

test_that("P3.3 shared metadata surfaces in runtime input spec", {
  r <- ptr_translate(
    'ggplot(mtcars, aes(x = mpg)) + geom_point(size = num(shared = "size_filter"))'
  )
  spec <- ptr_runtime_input_spec(r)
  ph_rows <- spec[spec$role %in% "placeholder", ]
  size_rows <- ph_rows[ph_rows$keyword == "num", ]
  expect_equal(nrow(size_rows), 1L)
  expect_equal(size_rows$shared, "size_filter")
})

test_that("P3.4 upload companion row carries shared", {
  r <- ptr_translate('ggplot(data = upload(shared = "ds"))')
  spec <- ptr_runtime_input_spec(r)
  upload_rows <- spec[spec$keyword %in% "upload", ]
  expect_equal(nrow(upload_rows), 2L)  # placeholder + companion
  expect_true(all(upload_rows$shared == "ds"))
})

test_that("P3.5 bare-symbol placeholder has shared = NA in input spec", {
  r <- ptr_translate("ggplot(aes(x = var)) + geom_point()")
  spec <- ptr_runtime_input_spec(r)
  var_rows <- spec[spec$keyword %in% "var", ]
  expect_equal(nrow(var_rows), 1L)
  expect_true(is.na(var_rows$shared))
})

test_that("P3 bare-symbol placeholders pass through unchanged", {
  r <- ptr_translate("ggplot(mtcars, aes(x = var)) + geom_point()")
  consumers <- find_nodes(r, is_ptr_ph_data_consumer)
  expect_equal(length(consumers), 1L)
  expect_null(consumers[[1]]$shared)
  # Bare-symbol id is the raw layer-encoded id, NOT prefixed with "shared_".
  expect_false(startsWith(consumers[[1]]$id, "shared_"))
})

# ---- P3.6+ — ptr_validate_shared_bindings + runtime fallback ----

test_that("P3.6 missing shared binding falls back to ptr_missing at substitute", {
  # Formula carries `shared = "axis"` but caller supplies no matching
  # entry in shared_bindings → P8 should treat the placeholder as missing.
  r <- ptr_translate(
    'ggplot(mtcars, aes(x = var(shared = "axis"))) + geom_point()'
  )
  s <- ptr_substitute(r, input_snapshot = list(), shared_bindings = list())
  missings <- find_nodes(s, is_ptr_missing)
  expect_gte(length(missings), 1L)
})

test_that("P3.7 ptr_validate_shared_bindings accepts NULL and empty list", {
  expect_identical(ptr_validate_shared_bindings(NULL), list())
  expect_identical(ptr_validate_shared_bindings(list()), list())
})

test_that("P3.8 ptr_validate_shared_bindings rejects non-list", {
  expect_error(ptr_validate_shared_bindings("not-a-list"), "list")
})

test_that("P3.9 ptr_validate_shared_bindings requires names", {
  unnamed <- list(shiny::reactive(1), shiny::reactive(2))
  expect_error(ptr_validate_shared_bindings(unnamed), "name")
})

test_that("P3.10 ptr_validate_shared_bindings rejects duplicate names", {
  dup <- list(a = shiny::reactive(1), a = shiny::reactive(2))
  expect_error(ptr_validate_shared_bindings(dup), "duplicate|unique")
})

test_that("P3.11 ptr_validate_shared_bindings rejects non-reactive values", {
  bad <- list(a = 1, b = "two")
  expect_error(ptr_validate_shared_bindings(bad), "reactive")
})

# ---- Plan 06 — shared section + dedup + cross-check ----

.shared_test_env <- function(extras = list()) {
  list2env(c(list(mtcars = mtcars), extras), parent = globalenv())
}

# (a) Single-plot auto-render: one widget per key in the shared section,
#     none inside layer tabs.
test_that("P06.a single-plot renders shared widget once in shared section", {
  parts <- ptr_app_components(
    'ggplot(data = mtcars, aes(x = mpg, y = hp)) + geom_point(size = num(shared = "sz"))',
    envir = .shared_test_env()
  )
  ui_html <- as.character(parts$ui)
  # The shared widget id appears exactly once in the rendered DOM.
  matches <- gregexpr('id="shared_sz"', ui_html, fixed = TRUE)[[1L]]
  expect_equal(sum(matches > 0L), 1L)
})

# (b) B1 regression — shared widget value drives the runtime.
test_that("P06.b shared input value flows into rendered code", {
  parts <- ptr_app_components(
    'ggplot(data = mtcars, aes(x = mpg, y = hp)) + geom_point(size = num(shared = "sz"))',
    envir = .shared_test_env()
  )
  shiny::testServer(parts$server, {
    session$setInputs(shared_sz = 7)
    session$setInputs(ptr_update_plot = 1L)
    expect_match(output$ptr_code, "size = 7", fixed = TRUE)
  })
})

# (c) Validation: extra binding key (typo) raises error when tree is supplied.
test_that("P06.c validation errors on extra binding key (typo)", {
  tree <- ptr_translate(
    'ggplot(data = mtcars, aes(x = mpg)) + geom_point(size = num(shared = "axis"))'
  )
  expect_error(
    ptr_validate_shared_bindings(
      list(axsi = shiny::reactive(1)),
      tree = tree
    ),
    "axsi"
  )
})

# (c') Validation: missing-from-bindings key raises error in strict mode.
test_that("P06.c' validation errors on missing binding key (strict)", {
  tree <- ptr_translate(
    'ggplot(data = mtcars, aes(x = mpg)) + geom_point(size = num(shared = "sz"))'
  )
  expect_error(
    ptr_validate_shared_bindings(list(), tree = tree, strict_missing = TRUE),
    "sz"
  )
  # Single-plot host (strict_missing = FALSE) tolerates the empty bindings.
  expect_silent(
    ptr_validate_shared_bindings(list(), tree = tree, strict_missing = FALSE)
  )
})

# (d) M1 dedup — same shared key referenced from two layers renders once.
test_that("P06.d dedup: same key in two layers renders once", {
  parts <- ptr_app_components(
    paste0(
      'ggplot(data = mtcars, aes(x = mpg, y = hp)) + ',
      'geom_point(size = num(shared = "sz")) + ',
      'geom_line(size = num(shared = "sz"))'
    ),
    envir = .shared_test_env()
  )
  ui_html <- as.character(parts$ui)
  matches <- gregexpr('id="shared_sz"', ui_html, fixed = TRUE)[[1L]]
  expect_equal(sum(matches > 0L), 1L)
})

# (e) Translate-clean assertion: no shared placeholder leaks into the
#     per-layer placeholder list.
test_that("P06.e find_layer_placeholders excludes shared placeholders", {
  tree <- ptr_translate(
    'ggplot(data = mtcars, aes(x = mpg, y = hp)) + geom_point(size = num(shared = "sz"))'
  )
  expect_gte(length(collect_shared_placeholders(tree)), 1L)
  for (layer in tree$layers) {
    phs <- find_layer_placeholders(layer$children)
    expect_true(all(vapply(phs, function(p) is.null(p$shared), logical(1))))
    pipe_phs <- find_layer_placeholders_with_stage(layer$data_arg)
    expect_true(all(vapply(pipe_phs,
                           function(e) is.null(e$ph$shared),
                           logical(1))))
  }
})

# (f) Grid auto-render: `ptr_app_grid` with no `shared_ui` for a key
#     referenced in formulas auto-renders the default widget at the
#     top-level shared panel (mirrors single-plot's auto-bind).
test_that("P06.f grid auto-renders default widget for unbound shared key", {
  parts <- ptr_app_grid_components(
    plots = list(
      "ggplot(data = mtcars, aes(x = mpg, y = hp)) + geom_point(size = num(shared = \"sz\"))",
      "ggplot(data = mtcars, aes(x = wt,  y = qsec)) + geom_point(alpha = num(shared = \"sz\")/10)"
    ),
    shared_ui = list(),
    envir = .shared_test_env()
  )
  ui_html <- as.character(parts$ui)
  expect_match(ui_html, 'id="sz"', fixed = TRUE)
  # The bare-key widget appears once at the top-level panel, not inside
  # any module (modules don't render shared widgets at all).
  bare_matches <- gregexpr('id="sz"', ui_html, fixed = TRUE)[[1L]]
  expect_equal(sum(bare_matches > 0L), 1L)
})

# (g) Grid still rejects extra keys in shared_ui that no formula uses.
test_that("P06.g grid rejects shared_ui keys not present in any formula", {
  expect_error(
    ptr_app_grid_components(
      plots = list(
        "ggplot(data = mtcars, aes(x = mpg)) + geom_point(size = num(shared = \"sz\"))"
      ),
      shared_ui = list(typo = function(id) shiny::sliderInput(id, "x", 1, 10, 5)),
      envir = .shared_test_env()
    ),
    "typo"
  )
})

# (h) Single-plot shared `var`: top-level uiOutput emits a picker
#     populated with columns from the resolved upstream.
test_that("P06.h shared `var` in single-plot renders picker with upstream cols", {
  parts <- ptr_app_components(
    'ggplot(data = mtcars, aes(x = var(shared = "axis"), y = mpg)) + geom_point()',
    envir = .shared_test_env()
  )
  ui_html <- as.character(parts$ui)
  expect_match(ui_html, 'id="shared_axis_ui"', fixed = TRUE)
  shiny::testServer(parts$server, {
    session$flushReact()
    rendered <- output$`shared_axis_ui`
    html_str <- if (is.list(rendered) && !is.null(rendered$html)) rendered$html else as.character(rendered)
    expect_match(html_str, "mpg")
    expect_match(html_str, "cyl")
  })
})

# (i) Single-plot shared `var` with diverging sources: top-level renderUI
#     emits an alert AND the error panel mirrors it.
test_that("P06.i diverging sources surface as alert + error-panel entry", {
  parts <- ptr_app_components(
    'ggplot(data = mtcars, aes(x = var(shared = "v"))) + geom_point(data = iris, aes(y = var(shared = "v")))',
    envir = .shared_test_env(list(iris = iris))
  )
  shiny::testServer(parts$server, {
    session$flushReact()
    rendered <- output$`shared_v_ui`
    html_str <- if (is.list(rendered) && !is.null(rendered$html)) rendered$html else as.character(rendered)
    expect_match(html_str, "alert")
    expect_match(html_str, "different source")
    err <- output$ptr_error
    err_html <- if (is.list(err) && !is.null(err$html)) err$html else as.character(err)
    expect_match(err_html, "different source")
  })
})

# (j) Same source, different upstream: falls back to source columns.
test_that("P06.j same source, different upstreams falls back to source", {
  parts <- ptr_app_components(
    paste0(
      'ggplot(data = mtcars |> dplyr::select(mpg, cyl), aes(x = var(shared = "v"))) + ',
      'geom_point(data = mtcars |> dplyr::select(hp, wt), aes(y = var(shared = "v")))'
    ),
    envir = .shared_test_env()
  )
  shiny::testServer(parts$server, {
    session$flushReact()
    rendered <- output$`shared_v_ui`
    html_str <- if (is.list(rendered) && !is.null(rendered$html)) rendered$html else as.character(rendered)
    # Source = mtcars; should expose every mtcars column, not just the
    # intersection of the two select() narrowings.
    for (col in c("mpg", "cyl", "hp", "wt", "qsec", "gear")) {
      expect_match(html_str, col)
    }
  })
})
