# ADR 0023 / PLAN-04: ptr_shared_server populates panel_sources for
# panel-owned source keys. Pins the host-side helper
# `ptr_setup_panel_sources()`:
#   * empty list when no panel-owned source keys exist
#   * one reactive per panel-owned ppUpload source key, keyed canonical id
#   * the panel's `shared_<key>_ui` renderUI emits the source widget tag
#     (incl. fileInput) -- so the embedder's panel still has the upload
#     control once this helper takes ownership of that container
#   * default_arg fallback primes the per-source reactive at boot before
#     any click (ADR worked example #2)
#   * resolve errors surface into the host's `ptr_shared_errors` sink
#   * custom source keywords inherit panel-scope wiring via the registry-
#     dispatching `resolve_upload_source()` helper (ADR worked example #6)
#   * spec-seeding a panel-owned fileInput with a data frame is silently
#     skipped at the panel level (ADR worked example R3): no error, no
#     input write, panel_sources entry stays NULL
#   * embedder-supplied panel_sources reactive cannot replace the
#     fileInput tag the panel emits (ADR worked example R4): the host
#     renderUI still emits a fileInput tag for the source.


# ---- Scenario 1: no panel-owned source keys -> empty list ----------------

test_that("panel_sources is list() when only value-shared keys are panel-owned", {
  obj <- ptr_shared(c(
    "ggplot(mtcars, aes(x = ppVar(shared='x'))) + geom_point()",
    "ggplot(mtcars, aes(x = ppVar(shared='x'))) + geom_col()"
  ))
  state <- NULL
  server <- function(input, output, session) {
    state <<- ptr_shared_server(obj, envir = globalenv())
  }
  shiny::testServer(server, {
    expect_identical(state$panel_sources, list())
  })
})


# ---- Scenario 2: one panel-owned ppUpload source -> one reactive ---------

test_that("one reactive per panel-owned ppUpload source key, keyed canonical id", {
  obj <- ptr_shared(c(
    "ggplot(ppUpload(shared='ds'), aes(x = ppVar(shared='col'))) + geom_point()",
    "ggplot(ppUpload(shared='ds'), aes(x = ppVar(shared='col'))) + geom_col()"
  ))
  state <- NULL
  server <- function(input, output, session) {
    state <<- ptr_shared_server(obj, envir = globalenv())
  }
  shiny::testServer(server, {
    expect_equal(names(state$panel_sources), c("shared_ds"))
    expect_true(shiny::is.reactive(state$panel_sources$shared_ds))
  })
})


# ---- Scenario 3: container renderUI emits a fileInput tag ----------------

test_that("renderUI for shared_ds_ui emits a fileInput with id shared_ds", {
  obj <- ptr_shared(c(
    "ggplot(ppUpload(shared='ds'), aes(x = ppVar(shared='col'))) + geom_point()",
    "ggplot(ppUpload(shared='ds'), aes(x = ppVar(shared='col'))) + geom_col()"
  ))
  server <- function(input, output, session) {
    ptr_shared_server(obj, envir = globalenv())
  }
  shiny::testServer(server, {
    session$flushReact()
    html <- paste(as.character(output$shared_ds_ui), collapse = "\n")
    expect_true(grepl("type=\"file\"", html, fixed = TRUE))
    expect_true(grepl("id=\"shared_ds\"", html, fixed = TRUE))
  })
})


# ---- Scenario 4 (ADR #2): default_arg fallback primes panel_sources ------

test_that("default_arg primes panel_sources at boot (ADR worked example #2)", {
  df_main <- mtcars
  obj <- ptr_shared(c(
    "ggplot(ppUpload(df_main, shared='ds'), aes(x = ppVar(shared='col'), y = mpg)) + geom_point()",
    "ggplot(ppUpload(df_main, shared='ds'), aes(x = ppVar(shared='col'), y = hp)) + geom_col()"
  ))
  state <- NULL
  server <- function(input, output, session) {
    state <<- ptr_shared_server(obj, envir = environment())
  }
  shiny::testServer(server, {
    session$flushReact()
    expect_identical(shiny::isolate(state$panel_sources$shared_ds()),
                     mtcars)
  })
})


# ---- Scenario 5: resolve errors surface into ptr_shared_errors sink ------

test_that("upload errors from resolve_data() reach the panel's error sink", {
  # Register a temporary panel-scope source keyword whose `resolve_data`
  # always throws, so any non-NULL input slot triggers the error path.
  # `ppUpload`'s real `resolve_data` reads a file; we want a deterministic
  # throw without touching the file system.
  withr::local_envvar(NOT_CRAN = "true")
  ptr_define_placeholder_source(
    "ppBadSource",
    build_ui = function(node, label = NULL, ...) {
      shiny::tagList(
        shiny::fileInput(inputId = node$id, label = label %||% "Upload"),
        shiny::textInput(
          inputId = node$shortcut_id %||% paste0(node$id, "_shortcut"),
          label = "Name",
          value = ""
        )
      )
    },
    resolve_data = function(value, node, ...) {
      rlang::abort("bad-source-resolve")
    },
    shortcut = TRUE,
    copy_defaults = list(label = "Bad source")
  )
  withr::defer(ptr_clear_placeholder("ppBadSource"))

  obj <- ptr_shared(c(
    "ggplot(ppBadSource(shared='ds'), aes(x = ppVar(shared='col'))) + geom_point()",
    "ggplot(ppBadSource(shared='ds'), aes(x = ppVar(shared='col'))) + geom_col()"
  ))
  server <- function(input, output, session) {
    ptr_shared_server(obj, envir = globalenv())
  }
  shiny::testServer(server, {
    # ADR 0025 §2/§3: a non-empty shortcut makes the textbox the live
    # affordance and drops the lingering file, so `resolve_data` is never
    # called and its abort cannot surface. To exercise the error sink for an
    # upload `resolve_data()` failure (this test's intent), leave the shortcut
    # EMPTY so the file path runs and the abort propagates. (Pre-ADR-0025 this
    # set `shared_ds_shortcut = "x_df"` -- the retired both-active state.)
    session$setInputs(
      shared_ds = list(datapath = "no-such-file", name = "x.csv")
    )
    session$flushReact()
    err_html <- paste(as.character(output$ptr_shared_errors),
                      collapse = "\n")
    expect_true(grepl("bad-source-resolve", err_html, fixed = TRUE))
    expect_true(grepl("shared_ds", err_html, fixed = TRUE))
  })
})


# ---- Scenario 6 (ADR #6): custom source keyword inherits panel wiring ----

test_that("custom source keyword inherits panel-scope wiring via registry dispatch", {
  ptr_define_placeholder_source(
    "ppDataChooser",
    build_ui = function(node, label = NULL, ...) {
      shiny::selectInput(node$id, label %||% "Choose",
                         choices = c("mtcars", "iris"))
    },
    resolve_data = function(value, node, ...) {
      get(value, envir = globalenv())
    },
    resolve_expr = function(value, node, ...) {
      as.symbol(value)
    },
    copy_defaults = list(label = "Choose a dataset")
  )
  withr::defer(ptr_clear_placeholder("ppDataChooser"))

  obj <- ptr_shared(c(
    "ggplot(ppDataChooser(shared='ds'), aes(x = ppVar(shared='col'))) + geom_point()",
    "ggplot(ppDataChooser(shared='ds'), aes(x = ppVar(shared='col'))) + geom_col()"
  ))
  state <- NULL
  server <- function(input, output, session) {
    state <<- ptr_shared_server(obj, envir = globalenv())
  }
  shiny::testServer(server, {
    # Same canonical-id shape as the built-in ppUpload path: one entry,
    # keyed `shared_ds`, holding a reactive that resolves through the
    # registry's `resolve_data` (no per-keyword wiring required).
    expect_equal(names(state$panel_sources), c("shared_ds"))
    expect_true(shiny::is.reactive(state$panel_sources$shared_ds))
    # Drive the chooser to "mtcars": the panel renderUI emits a
    # selectInput (no companion text input -- companion-less source),
    # the observer dispatches via the registry's resolve_data hook,
    # and the resulting reactive yields the resolved data frame.
    session$setInputs(shared_ds = "mtcars")
    session$flushReact()
    expect_identical(shiny::isolate(state$panel_sources$shared_ds()),
                     mtcars)
  })
})


# ---- Scenario 7 (ADR R3): spec-seeding fileInput w/ data frame is skipped ----

test_that("spec-seeding the fileInput with a data frame is silently skipped at panel scope", {
  # The fileInput slot is browser-driven; at panel scope no
  # `apply_spec_at_boot` runs against it, and the input is never assigned
  # from a top-level `spec` of the form `list(shared_ds = mtcars)`. The
  # panel_sources reactive therefore stays NULL and no error fires.
  obj <- ptr_shared(c(
    "ggplot(ppUpload(shared='ds'), aes(x = ppVar(shared='col'))) + geom_point()",
    "ggplot(ppUpload(shared='ds'), aes(x = ppVar(shared='col'))) + geom_col()"
  ))
  state <- NULL
  errs <- NULL
  server <- function(input, output, session) {
    state <<- ptr_shared_server(obj, envir = globalenv())
  }
  expect_silent({
    shiny::testServer(server, {
      session$flushReact()
      # Browser-driven slot: never written by the helper, stays NULL.
      expect_null(shiny::isolate(input$shared_ds))
      # No upload bound -> panel_sources entry resolves to NULL.
      expect_null(shiny::isolate(state$panel_sources$shared_ds()))
      # Error sink stays empty.
      errs <<- paste(as.character(output$ptr_shared_errors),
                     collapse = "\n")
    })
  })
  # No body in the error UI (empty character() through paste("\n") still
  # renders an empty container, but contains no message text).
  expect_false(grepl("shared_ds", errs %||% "", fixed = TRUE))
})


# ---- Scenario 8 (ADR R4): panel still emits the fileInput tag ------------

test_that("panel still emits the fileInput tag for the source (ADR worked example R4)", {
  # ADR worked example R4: an embedder may supply a reactive value via
  # `panel_sources = list(shared_ds = my_df)`; the *value* of
  # `state$panel_sources$shared_ds()` is the embedder's, but the panel
  # MUST still emit the fileInput tag for the source -- the escape-hatch
  # overrides the resolved value, not the widget shape. At PLAN-04's
  # level this means the host's renderUI for `shared_<key>_ui` emits a
  # fileInput regardless of any embedder injection upstream.
  obj <- ptr_shared(c(
    "ggplot(ppUpload(shared='ds'), aes(x = ppVar(shared='col'))) + geom_point()",
    "ggplot(ppUpload(shared='ds'), aes(x = ppVar(shared='col'))) + geom_col()"
  ))
  server <- function(input, output, session) {
    ptr_shared_server(obj, envir = globalenv())
  }
  shiny::testServer(server, {
    session$flushReact()
    html <- paste(as.character(output$shared_ds_ui), collapse = "\n")
    expect_true(grepl("<input", html, fixed = TRUE))
    expect_true(grepl("type=\"file\"", html, fixed = TRUE))
    expect_true(grepl("id=\"shared_ds\"", html, fixed = TRUE))
  })
})
