# test-adr26-host-env-rename-clear.R -- ADR 0026.
#
# The host UPLOAD clear (ADR 0025 item #7) and the single-instance env-rename
# clear both work. This file is the regression for the one corner neither
# covers: a HOST-scope shared env source (`ptr_bind_shared_consumer_uis`,
# state = NULL) whose typed shortcut is RENAMED to a different env frame with
# no upload at either end. The consumer-clear identity
# (`consumer_upstream_source_state`) is `paste0(sid, "#", dp, "#", bn)`; at
# host scope `dp = ""` (no upload) AND `bn = ""` (no `state$bound_names`), so
# the identity is constant across the rename, `consumer_clear_for_new_source`
# never trips, and a stale column pick rides onto the new frame.
#
# Protocol: project memory `shinytest2-appdir-pkgload` + helper-vignette-apps.R
# (`boot_vignette_app` = skip guards + GGP_PKG + AppDriver + wait_for_idle +
# defer). Never app$get_values(). set_input waits for the binding; wait_for_idle
# after every set. The shortcut rising edge + the resolve path are debounced
# 400ms, so `type_shortcut()` sleeps past the window then re-idles (same
# ordering caution as test-shared-host-source-reset.R).

# Set the host shortcut textbox and settle past the 400ms rising-edge / resolve
# debounce so the bump + re-resolve have fired before asserting.
type_shortcut <- function(app, id, value) {
  set_input(app, id, value)
  Sys.sleep(0.6)
  app$wait_for_idle(timeout = 15 * 1000)
}

# ---------------------------------------------------------------------------
# Core ADR 0026 regression: type env frame A -> pick the OVERLAPPING column ->
# rename the shortcut to env frame B (no upload). The host consumer picker must
# CLEAR (new user-supplied source = new work, ADR 0025 §3b). Baseline (the bug)
# rides the stale `body_mass_g`. The overlapping column is the load-bearing
# proxy-trap guard; the `grp`/`trt` populated checks prove each frame actually
# loaded so the clear assertion isn't vacuously reading a still-frame-A picker.
# ---------------------------------------------------------------------------
test_that("host: env->env shortcut rename clears the consumer picker (overlapping column)", {
  app <- boot_vignette_app("adr26-host-env-rename-clear")

  expect_dom_id(app, "shared_ds_shortcut")

  # Type the first env frame; pick the column present in BOTH frames.
  type_shortcut(app, "shared_ds_shortcut", "chickish")
  expect_picker_populated(app, "shared_col", "grp")          # grp unique -> frame A loaded
  expect_picker_populated(app, "shared_col", "body_mass_g")
  set_input(app, "shared_col", "body_mass_g")
  app$wait_for_idle(timeout = 10 * 1000)
  expect_equal(app$get_value(input = "shared_col"), "body_mass_g",
               label = "pre-rename pick is body_mass_g")

  # RENAME to a DIFFERENT env frame -- no file upload at either end.
  type_shortcut(app, "shared_ds_shortcut", "plantish")
  expect_picker_populated(app, "shared_col", "trt")          # trt unique -> rename loaded

  sel <- app$get_value(input = "shared_col")
  expect_true(
    is.null(sel) || length(sel) == 0L || identical(sel, ""),
    label = paste0("host consumer picker cleared after env->env rename ",
                   "(got: ", deparse(sel), "); body_mass_g exists in BOTH ",
                   "frames so this is NOT a selectInput auto-drop")
  )

  # Cleared != broken: the picker offers the new frame's columns and both
  # plots still draw once the user re-picks. The explicit value assertion
  # settles the ADR 0026 latch question -- after the clear edge the host
  # `cleared_for_identity` latch must NOT re-fire and re-clobber a legitimate
  # re-pick on the (now-stable) new identity.
  set_input(app, "shared_col", "trt")
  app$wait_for_idle(timeout = 10 * 1000)
  expect_equal(
    app$get_value(input = "shared_col"), "trt",
    label = "re-pick after the clear survives (host latch does not re-clobber)"
  )
  draw(app, "p1-ptr_update_plot")
  draw(app, "p2-ptr_update_plot")
  expect_no_inline_error(app, "p1-ptr_error")
  expect_no_inline_error(app, "p2-ptr_error")
})

# ---------------------------------------------------------------------------
# Boundary guard (ADR 0026 §scope): an env name that EQUALS the default at boot
# is not "new work" and must NOT clear. Here the rename is back to the SAME
# frame (chickish -> chickish): identity is genuinely unchanged, so a correct
# fix leaves the pick intact. Protects against an over-eager fix that clears on
# every shortcut keystroke regardless of whether the frame actually changed.
# ---------------------------------------------------------------------------
test_that("host: re-typing the SAME env frame does not clear the pick", {
  app <- boot_vignette_app("adr26-host-env-rename-clear")

  type_shortcut(app, "shared_ds_shortcut", "chickish")
  expect_picker_populated(app, "shared_col", "body_mass_g")
  set_input(app, "shared_col", "body_mass_g")
  app$wait_for_idle(timeout = 10 * 1000)
  expect_equal(app$get_value(input = "shared_col"), "body_mass_g")

  # Re-type the identical name: same frame, same data -> not new work.
  type_shortcut(app, "shared_ds_shortcut", "chickish")
  expect_equal(
    app$get_value(input = "shared_col"), "body_mass_g",
    label = "re-typing the same env frame leaves the pick intact (not new work)"
  )
})
