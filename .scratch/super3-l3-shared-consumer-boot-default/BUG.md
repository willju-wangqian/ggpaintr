# BUG: L3 multi-cell host-scope shared consumer discards its formula default at boot

Status: RESOLVED (2026-05-28) — see DIAGNOSIS.md. Root cause was NOT the host-scope binder/`has_rendered` (that hypothesis was refuted by probe). The symptom came from the fixture's custom `shared_ui` override on a `var` consumer key, which rendered a static `selectInput(names(mtcars))` with no `selected=`, bypassing `ppVar`'s default injection. Resolution (per user decision): `shared_ui` was removed as a supported argument of `ptr_shared()` / `ptr_app_grid()` — widget customization now comes from each placeholder's own `build_ui`. super-3's `linked` auto-renders from `ppVar`'s `build_ui` and seeds `cyl`; the boot-oracle xfail pins were removed and the assertions are now active green.

## Summary

In an L3 multi-cell layout (`ptr_app_grid` / multiple `ptr_ui` cells coordinated by a host-owned shared key), a **shared consumer** placeholder such as `color = ppVar(cyl, shared = "linked")` boots to the **first column of the data** (`mpg`) instead of its **formula default** (`cyl`).

This is the *same bug class* fixed at `cddc46e` ("honour shared-consumer formula default at boot; unify the two consumer binders"), but in a **third seeding path** that the unification missed: `cddc46e` routed `ptr_setup_consumer_uis()` (non-shared) and `ptr_bind_shared_consumer_uis()` (shared, single-app) through `consumer_seed_decision()`, but the L3 / **host-scope** shared-consumer invocation still flips `has_rendered` on the empty-`cols` first fire and never re-injects the default.

Single-app fixtures seed correctly (verified): super-1 `shared_grp` → `cyl`, super-2b `shared_fac` → `Species`. Only the multi-cell host-scope path regresses.

## Repro (boot state, no widget interaction)

Fixture: `tests/testthat/fixtures/vignette-apps/super-3-l3-multi-shared-plotly/`. Both cells' formulas declare `color = ppVar(cyl, shared = "linked")` (default `cyl`); `reference.R` (Path-B) renders `color = cyl`.

```r
NOT_CRAN=true Rscript -e '
  suppressMessages(devtools::load_all("."))
  testthat::test_file("tests/testthat/test-boot-reference-oracle.R")'
```

Observed at boot (first render, before any widget is driven), in BOTH cells:

- `app$get_value(input = "shared_linked")` → `"mpg"`  (expected `"cyl"`)
- `plot1-ptr_code` / `plot2-ptr_code` final-mode code → `aes(x = ..., y = ..., color = mpg)`  (expected `color = cyl`)

Two independent signals (picker value + rendered code) agree, and super-1 / super-2b pass the identical boot-oracle procedure, ruling out a timing/regex artifact.

## Why the existing suite missed it

`test-super-pressure.R` super-3 drives `shared_linked` → `"gear"` *before* asserting `color = gear` in both cells. Driving the widget makes `current` non-NULL and masks the boot-default seeding path — the bug lived only in the untouched boot state. (This is gap #1 from the test-hardening handoff: "tests drive every widget before asserting; the untouched boot state is never asserted.")

## Candidate location

`R/paintr-server.R`. `consumer_seed_decision()` (~2350) is the unified contract; `ptr_setup_consumer_uis()` (~2366) and `ptr_bind_shared_consumer_uis()` (~2600) route through it. The L3/host-scope path to inspect:

- the **state-less / host-scope invocation** of `ptr_bind_shared_consumer_uis()` (host-owned shared widget, no per-cell `state`), and/or
- `ptr_bind_local_shared_consumers()` (referenced in `test-consumer-picker-panel-sources-dep.R`).

Hypothesis: the host-scope binder flips `has_rendered` on the `cols = character()` first fire (upstream data not yet resolved across cells), then takes the `seed %||% current %||% character(0)` branch with `current = NULL` → `selected = character(0)` → `invoke_build_ui` auto-selects the first column. This is exactly the pre-`cddc46e` shape, just unported to the host-scope path. The fix is likely to route this path through `consumer_seed_decision()` too (the `default_landed` deferral), mirroring `cddc46e`.

## Regression test already in place (pinned)

`tests/testthat/test-boot-reference-oracle.R` super-3 asserts the boot-oracle. The shared-`linked` slot + `shared_linked` picker assertions are PINNED as known-failures via `expect_boot_matches_reference(..., xfail_shared_keys = "linked")` (`testthat::expect_failure()` in `helper-super-pressure.R`). The gate stays FAIL 0 / SKIP 0. **When this bug is fixed, those `expect_failure()` pins flip RED** — remove `xfail_shared_keys = "linked"` from the super-3 test at that point so the assertions become active green.

The non-shared x/y mappings (mpg/wt, hp/qsec) are asserted actively and pass today.

## Scope note

Surfaced by the boot-reference-oracle test-hardening work (handoff: close the e2e seeding-gap). That work was test-only by constraint; this product bug is handed off here per "if it surfaces a new product bug, hand that off separately rather than expanding scope."
