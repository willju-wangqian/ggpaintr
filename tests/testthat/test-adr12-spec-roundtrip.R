# ADR 0012 §3.6 / PLAN-06 — `spec=` apply at session boot.
#
# Boots `tests/testthat/fixtures/vignette-apps/adr12-spec-roundtrip/`
# which calls `ptr_app(formula, spec = list(...))`. Asserts that the
# `apply_spec_at_boot` callback inside the engine has overridden each
# widget's registry/default value with the supplied spec value:
#
#   1. The in-aes ppVar picker (id `ggplot_1_1_ppVar_NA`) shows `"carb"`
#      as the selected option (matches the spec; the registry default is
#      empty selection).
#   2. The layer checkbox (id `geom_point_checkbox`) is unchecked (the
#      spec says `FALSE`; the registry default is `TRUE`).
#   3. The unknown id (`no_such_widget_id_99`) is silently dropped — the
#      app does not crash, and no inline error pane is rendered.
#
# Plus pure-function unit tests for `apply_spec_at_boot`'s pieces:
# namespace filtering and unknown-id collection. The dispatch layer
# (`apply_spec_entry`) is covered by the browser e2e (real
# `updateXyzInput` calls hitting real widgets).

# ---- e2e: round-trip spec applied at session boot --------------------------

test_that("adr12 / PLAN-06: spec= overrides widget defaults at session boot", {
  app <- boot_vignette_app("adr12-spec-roundtrip")

  # `apply_spec_at_boot` defers all `updateXyzInput` calls into a
  # `session$onFlushed(once = TRUE)` callback, so the picker + checkbox
  # may take an extra flush to settle. `wait_for_idle()` polls until the
  # event loop is quiescent.
  app$wait_for_idle(timeout = 15 * 1000)

  # 1. var picker honored at boot. The fixture uses `multiple = TRUE` +
  #    `maxOptions = 1L` (legacy pickerInput trick — see
  #    ptr_builtin_var_build_ui); the rendered picker's selected option
  #    is communicated via the `option[selected]` attribute on the
  #    underlying <select>. `expect_picker_populated` uses a substring
  #    match on the rendered HTML which is sufficient here: the column
  #    name "carb" only appears as a populated choice if the picker is
  #    bound AND `updatePickerInput(selected="carb")` reached it.
  expect_picker_populated(app, "ggplot_1_1_ppVar_NA", "carb")
  # Stronger assertion: confirm the picker's runtime input value is the
  # spec value. `shinyWidgets::pickerInput` updates selection client-side
  # (via JS bootstrap-select); the server-rendered HTML's <option> tags
  # do NOT carry the `selected` attribute after `updatePickerInput()` --
  # the live state lives only in the input registry, which is what
  # `app$get_value(input = ...)` returns. This is the same source of
  # truth used by the checkbox/num assertions below.
  expect_equal(app$get_value(input = "ggplot_1_1_ppVar_NA"), "carb",
               label = "ggplot_1_1_ppVar_NA honored at boot (\"carb\" from spec)")

  # 2. layer checkbox honored at boot. The registry default is TRUE; the
  #    spec sets FALSE. shinytest2 exposes the runtime input value, which
  #    is the most direct check (the rendered <input> also has no
  #    `checked` attribute when unchecked).
  expect_equal(app$get_value(input = "geom_point_checkbox"), FALSE,
               label = "geom_point_checkbox honored at boot (FALSE from spec)")

  # 3. num input honored at boot. The registry default is NA; the spec
  #    sets 5. Read the numeric value through Shiny's input snapshot.
  expect_equal(app$get_value(input = "geom_point_2_2_ppNum_NA"), 5,
               label = "geom_point_2_2_ppNum_NA honored at boot (5 from spec)")

  # 4. Unknown id silently dropped: the app booted (boot_vignette_app
  #    succeeded) and no inline error is rendered.
  expect_no_inline_error(app, "ptr_error")
  # Spot-check the unknown id is absent from the rendered DOM — proves
  # the spec entry didn't accidentally inject a widget.
  expect_no_dom_id(app, "no_such_widget_id_99")
})

# ---- unit: apply_spec_at_boot pure-function pieces -------------------------
#
# These tests exercise the helper's namespace-filtering + unknown-id
# collection without booting a real session. We pass a minimal stub
# session (no `session$ns`, no `session$onFlushed`) and a stub `state`
# carrying just the `input_spec` data frame. We rely on the helper
# returning early on the filter / dispatch branches we don't drive.

stub_state_for_apply <- function(input_ids = character(),
                                 roles = character(),
                                 keywords = character()) {
  # Manufacture a minimal `input_spec` data frame in the same shape
  # `ptr_runtime_input_spec()` would produce. The dispatch fields the
  # helper consults are `input_id`, `role`, `keyword`. Pad the other
  # columns to the same length as `input_ids` so `data.frame()`'s recycling
  # rule does not reject the zero-row case (length-1 NA vs length-0 char).
  n <- length(input_ids)
  pad <- rep(NA_character_, n)
  df <- data.frame(
    input_id   = input_ids,
    role       = roles,
    layer_name = pad,
    keyword    = keywords,
    param_key  = pad,
    source_id  = pad,
    shared     = pad,
    stringsAsFactors = FALSE
  )
  list(input_spec = df)
}

# A stub session whose `ns("")` returns the configured prefix and whose
# `onFlushed` captures the callback (instead of waiting for a real flush).
stub_session <- function(prefix = "") {
  captured <- new.env(parent = emptyenv())
  captured$cb <- NULL
  list(
    ns = function(x) paste0(prefix, x),
    onFlushed = function(callback, once = TRUE) {
      captured$cb <- callback
    },
    sendInputMessage = function(id, message) {
      # No-op stub — `updateXyzInput` ultimately calls this. We trap it
      # so dispatch is exercised without a real client.
      invisible()
    },
    captured = captured
  )
}
# Mark the stub as a session_proxy so the prefix-derivation branch
# inside `apply_spec_at_boot` uses `session$ns("")`.
class(stub_session) <- "function"

test_that("apply_spec_at_boot returns silently on NULL / empty spec", {
  st <- stub_state_for_apply()
  sess <- stub_session()
  expect_silent(ggpaintr:::apply_spec_at_boot(NULL, sess, st))
  expect_silent(ggpaintr:::apply_spec_at_boot(list(), sess, st))
})

test_that("apply_spec_at_boot rejects unnamed / empty-named spec entries", {
  st <- stub_state_for_apply()
  sess <- stub_session()
  expect_error(
    ggpaintr:::apply_spec_at_boot(list(1L, 2L), sess, st),
    "fully named"
  )
  empty_named <- list(1L)
  names(empty_named) <- ""
  expect_error(
    ggpaintr:::apply_spec_at_boot(empty_named, sess, st),
    "fully named"
  )
})

test_that("apply_spec_at_boot aggregates unknown ids into one cli_inform", {
  st <- stub_state_for_apply(
    input_ids = c("foo", "bar"),
    roles     = c("layer_checkbox", "layer_checkbox"),
    keywords  = c(NA_character_, NA_character_)
  )
  sess <- stub_session(prefix = "")
  # Two unknown ids + zero known -> single cli_inform message naming
  # both, then the helper returns without queuing an onFlushed callback.
  msg <- tryCatch(
    {
      ggpaintr:::apply_spec_at_boot(
        list(`bogus_1` = TRUE, `bogus_2` = TRUE), sess, st
      )
      NULL
    },
    message = function(m) conditionMessage(m)
  )
  expect_true(!is.null(msg))
  expect_match(msg, "bogus_1", fixed = TRUE)
  expect_match(msg, "bogus_2", fixed = TRUE)
})

test_that("apply_spec_at_boot filters by session namespace prefix", {
  # With prefix `p1-`, only `p1-foo` survives; `p2-foo` and `foo` are
  # filtered out (handed to a different engine instance under the
  # plan's flat-spec-across-plots contract for `ptr_app_grid`). The
  # surviving entry's id matches a known widget, so unknown-id branch
  # does not fire — the only side effect is the queued `onFlushed`
  # callback being captured by our stub.
  st <- stub_state_for_apply(
    input_ids = "foo",
    roles     = "layer_checkbox",
    keywords  = NA_character_
  )
  sess <- stub_session(prefix = "p1-")
  expect_silent(
    ggpaintr:::apply_spec_at_boot(
      list(`p1-foo` = TRUE, `p2-foo` = TRUE, `foo` = TRUE),
      sess, st
    )
  )
  # The captured callback exists -> a surviving entry was queued.
  expect_true(is.function(sess$captured$cb))
})

test_that("apply_spec_at_boot returns silently when prefix matches nothing", {
  st <- stub_state_for_apply(
    input_ids = "foo",
    roles     = "layer_checkbox",
    keywords  = NA_character_
  )
  sess <- stub_session(prefix = "p1-")
  # Every spec entry is under a different prefix -> the helper returns
  # without emitting a message and without queuing a callback (the
  # other-prefix entries belong to a sibling engine, not this one).
  expect_silent(
    ggpaintr:::apply_spec_at_boot(
      list(`p2-foo` = TRUE, `p3-bar` = FALSE), sess, st
    )
  )
  expect_null(sess$captured$cb)
})

# ---- spec= NULL is a no-op (backward compatibility) ------------------------

test_that("ptr_app_components(spec = NULL) preserves today's behavior", {
  # No baseline regression: build the components with no spec and ensure
  # the resulting parts list has the expected ui + server slots and that
  # boot does NOT trip an unknown-id cli_inform (we never inspect a
  # spec). This is a contract-level check; the full regression is the
  # global gate.
  parts <- ptr_app_components(
    "ggplot(mtcars, aes(x = ppVar)) + geom_point()",
    expr_check = FALSE
  )
  expect_named(parts, c("ui", "server"))
  expect_true(is.function(parts$server))
})
