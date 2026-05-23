# ADR 0012 / PLAN-01 (Bug B) — SC-1
#
# The structural fix: spec= entries for custom-registered placeholder
# keywords apply at boot via the uniform `state$spec_seed` path. The
# pre-PLAN-01 code dispatched on placeholder keyword inside
# `apply_spec_entry()` and fell through to `FALSE` for any keyword
# without a built-in `updateXyz` partner, silently aggregating custom
# entries into the "skipped invalid value" warning. After PLAN-01 the
# value reaches the widget through the same `build_ui` hook that
# rendered it in the first place: `state$spec_seed[[bare_id]]` is the
# seed, `invoke_build_ui()`'s `extra$selected` is the carrier, the
# hook's `selected` formal is the sink.
#
# Why testServer and not the browser harness? The seed write happens
# synchronously inside `apply_spec_at_boot()`, and the renderUI's first
# fire reads `isolate(state$spec_seed[[raw_id]])`. testServer's
# `session$flushReact()` drives that first fire deterministically, so
# we can assert the rendered HTML without a browser round-trip.
#
# Why HTML and not `input[[id]]`? MockShinySession (testServer's
# session backend) does NOT auto-populate `input[[id]]` from a widget
# emitted via `renderUI` — in a real browser the JS round-trips the
# selection back to the server, but testServer has no such cycle. The
# architectural contract — "spec value reaches the widget" — is
# observable in the rendered HTML (the seeded option carries the
# `selected` attribute) and that is what we assert. The browser-side
# round-trip is covered by `test-adr12-spec-roundtrip.R` (shinytest2).

.test_env <- function(extras = list()) {
  list2env(c(list(mtcars = mtcars), extras), parent = globalenv())
}

test_that("ADR 0012 / PLAN-01 (Bug B): custom-keyword spec entry applies via state$spec_seed at boot", {
  # Register a custom value placeholder with a `selected` formal so it
  # honors `extra$selected` from invoke_build_ui. Three discrete choices
  # so the assertion isn't sensitive to free-form coercion.
  ptr_define_placeholder_value(
    keyword = "ppCustomChoice",
    build_ui = function(node, label, selected = NULL, ...) {
      shiny::selectInput(
        inputId = node$id,
        label = label,
        choices = c("a", "b", "c"),
        selected = selected
      )
    },
    resolve_expr = function(value, node, ...) {
      if (is.null(value) || !nzchar(value)) return(NULL)
      value
    }
  )
  withr::defer(ptr_clear_placeholder("ppCustomChoice"))

  e <- .test_env()
  server <- function(input, output, session) {
    session$userData$state <- ptr_server_internal(
      input, output, session,
      "ggplot(mtcars, aes(ppCustomChoice, mpg))",
      envir = e,
      spec = list(ggplot_1_1_ppCustomChoice_NA = "b")
    )
  }
  shiny::testServer(server, {
    # Drive the first flush: this runs the deferred apply_spec_at_boot
    # `onFlushed` callback AND the renderUI bodies that read the seed.
    # The seed write itself happens synchronously inside
    # apply_spec_at_boot, BEFORE the onFlushed registration, so the
    # renderUI's first fire sees the seed regardless of callback order.
    session$flushReact()

    state <- session$userData$state

    # Seed is the bare-id-keyed map. After `apply_spec_at_boot()` it
    # contains the spec value for our custom keyword (custom keywords
    # are NOT excluded — the exclusion is only for ppUpload, whose
    # fileInput is unsettable).
    expect_equal(
      state$spec_seed[["ggplot_1_1_ppCustomChoice_NA"]],
      "b",
      label = "state$spec_seed seeded by apply_spec_at_boot"
    )

    # Bug B fix proper: the renderUI's emitted HTML carries the spec
    # value as the `selected` option of the selectInput. Pre-PLAN-01 the
    # input was never seeded (`apply_spec_entry()` fell through to FALSE
    # for custom keywords). Post-PLAN-01 the renderUI mounts a
    # selectInput with `selected = "b"`, observable in the HTML as the
    # `selected` attribute on the matching `<option>`.
    out <- session$getOutput("ggplot_1_1_ppCustomChoice_NA_ui")
    html <- paste(as.character(out), collapse = "\n")
    expect_match(
      html,
      '<option value="b" selected>',
      fixed = TRUE,
      label = "custom-keyword widget HTML carries seeded selection"
    )
  })
})

test_that("ADR 0012 / PLAN-01 (Bug B): state$spec_seed is empty when no spec= is passed", {
  # Negative control: without a spec= argument the seed map stays empty
  # — invariant of `ptr_init_state()`'s default. Guards against future
  # refactors that might inadvertently populate spec_seed from defaults.
  e <- .test_env()
  server <- function(input, output, session) {
    session$userData$state <- ptr_server_internal(
      input, output, session,
      "ggplot(mtcars, aes(ppText, mpg))",
      envir = e
    )
  }
  shiny::testServer(server, {
    session$flushReact()
    state <- session$userData$state
    expect_true(is.environment(state$spec_seed))
    expect_equal(length(ls(state$spec_seed, all.names = TRUE)), 0L)
  })
})
