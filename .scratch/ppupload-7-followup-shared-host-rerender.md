# Follow-up (surfaced by ADR 0025 item #7): rising-edge fileInput pill-clear NOT extended to the shared-coordinator host-scope source path

**Status:** DONE (2026-05-29) — host source flag + visual reset ported; attended consumer-clear handled alongside. See "## Resolution" at the bottom.
**Surfaced:** 2026-05-29, during ADR 0025 item #7 implementation (commit on branch `ppUpload-bug`).
**Scope discipline:** flagged, NOT silently fixed/widened — per the plan's "surface, don't absorb" clause and `dev/plans/0025-item7-fileinput-reset/plan.html` §10.

## What #7 did (and where)

Item #7 made the shortcut `textInput` framework-owned + **static**, and re-renders only the source widget (`fileInput`) on the shortcut's **rising edge** (empty → non-empty), clearing a stale filename pill once a typed shortcut takes over. The data side keys off a per-source `state$source_file_reset` flag (set on that rising edge, cleared on a genuine new upload) so the env-load gate AND vacate-on-empty work without depending on the unreliable server-side `input[[src_id]]` value.

This was implemented for the **single-instance / formula-local** path:

- `build_ui_for.ptr_ph_data_source` (R/paintr-build-ui.R) — emits the static `textInput`.
- `ptr_setup_source_uis` (R/paintr-server.R) — the rising-edge re-render + the `state$source_file_reset` flag.
- `resolve_upload_source` (R/paintr-server.R) — honours `file_reset`.

## The gap

The **multi-instance shared-coordinator host-scope** source path is a PARALLEL implementation in `R/paintr-shared-ui.R` (`ptr_setup_panel_sources`, ~lines 504-655):

- Its own `renderUI` (`output[[output_id]] <- renderUI({...})`, ~532-579) renders the source widget via the registry `build_ui` (now fileInput-only, correct).
- Its `shortcut` `textInput` is now correctly emitted STATICally by the shared **panel body** `shared_panel_body_tag` (R/paintr-shared-coordinator.R) → `build_ui_for(node, ...)`, because `rewrite_shared` (R/paintr-shared.R:337-341) keeps `node$shortcut_id = paste0(canonical, "_shortcut")` in sync. So the textbox is **preserved / relocated, NOT lost** — verified by the green `test-shared-source-panel-multi-instance.R` cluster.
- BUT this host path has **no rising-edge re-render** and does **not** set/read `state$source_file_reset`. It calls `ptr_bind_source_mutex(input_id, shortcut_input_id, input, session)` (shared-ui.R:647) — which after #7 only does file→text (clear textbox on file pick).

### Consequence (not a crash, not a data regression)

For a **panel-owned shared `ppUpload(shared=...)` under a coordinator** (the multi-instance case): typing the shortcut no longer **visually** clears the host-scope fileInput pill (the buggy `ptr_reset_file_input` JS that previously *attempted* this was removed in #7). The data gate is unchanged (`resolve_upload_source`'s `shortcut_active` branch still forces the env-load), so **data correctness is intact** — only the cosmetic pill-clear and the `file_reset`-based vacate-on-empty are not extended here.

This was NOT a behavioural regression of a *working* feature (the removed JS was characterised as buggy/ineffective in the plan), but #7's benefit is simply not yet ported to the host path.

## To port it (if desired)

Mirror the three single-instance pieces into `ptr_setup_panel_sources` (shared-ui.R):
1. Add a per-source rising-edge re-render (`source_bump` dep on the host-scope debounced shortcut) to that path's `renderUI`.
2. Create + drive a `state`-level (or host-state-level) `source_file_reset` flag keyed by the canonical source id; clear it on a genuine new upload.
3. Pass `file_reset = ...` into the `resolve_upload_source(...)` call at shared-ui.R:602.

Watch the **bound_names key duality** (memory `bound-names-key-duality`): key the flag by the canonical source id, not the bound_names key.

## Why deferred

- The plan (`dev/plans/0025-item7-fileinput-reset/plan.html`) scoped item #7 to the single-instance/formula-local path; the prototype validated that path only.
- The §6 repro and all Success Criteria are single-instance.
- Porting to the host path is additional ADR-0023-area work that should be its own scoped change (and prototyped/tested for the multi-host case).

## Resolution (2026-05-29)

Implemented per `dev/plans/0025-item7-followup-host-shared-consumer-clear/plan.html` (implementable: PASS).

**Host source path (`ptr_setup_panel_sources`, R/paintr-shared-ui.R)** — ported the three #7 pieces into the single `local()` scope, byte-shape-equal to `ptr_setup_source_uis()`:
- **P1** `source_bump <- reactiveVal(0L)` + `source_bump()` dep on the first line of the host `renderUI` body → rising-edge re-render of the fileInput (clears the stale filename pill). One-directional (empty→non-empty only) so a file-pick does NOT wipe the just-uploaded display.
- **P2** `file_reset_rv <- reactiveVal(FALSE)`: 400ms-debounced `observeEvent` on the shortcut sets it TRUE + bumps on the empty→non-empty edge; `observeEvent(input[[input_id]])` sets it FALSE on a genuine new file pick. Kept local (read directly at the in-scope resolve call) — no `state$source_file_reset` publish needed (unlike single-instance), keyed by the canonical source id.
- **P3** `file_reset = isTRUE(file_reset_rv())` passed into the in-scope `resolve_upload_source()` call → env-load gate + vacate-on-empty key off the flag, not the stale server-side `input[[input_id]]`.

**Attended consumer clear (`ptr_bind_shared_consumer_uis`, R/paintr-server.R, HOST scope `state = NULL` only)** — added two per-picker latches (`cleared_for_identity`, `last_user_supplied`). At host scope the identity snapshot is bound-name-blind (`bn` always `""`), so it stabilises across the upload's trailing double-render and the base clear was reverted by a stale `current`. The latch re-asserts the clear on the one trailing render (then consumes itself so a later legitimate re-pick is not clobbered), and a supply→no-supply `vacate_edge` folds the P3 vacate into the same clear trigger. **Per-instance scope (`state != NULL`) is gated out → byte-equal → single-instance unaffected.** `consumer_seed_decision` and the non-shared binder `ptr_setup_consumer_uis` were NOT touched.

**Design note (deviation from plan's literal wording, intentional):** the plan said "hold selected=character(0) … until current is observed emptied." I instead **consume the latch after exactly one trailing re-assert** (the probe established the upload fires a *double* render — two beats — so one held render covers it). Holding "until emptied" risks a persistent latch wrongly clearing a later legitimate re-pick on the same data when no intervening empty-current render occurs; consume-after-one eliminates that with strictly less regression risk while still surviving the double-render. Verified by the S-N1 test.

**Tests** — `tests/testthat/test-shared-host-source-reset.R` (+ fixture `fixtures/vignette-apps/adr25-host-shared-source-reset/app.R`): P3 env-load gate, P3 vacate (discriminating), S-N1 consumer clear (overlapping `body_mass_g` — proxy-trap guard), S-N2 vacate-suppress, W1 single-instance guard (re-uses adr25-upload-default-clear). **Discrimination proven**: with the R fix stashed the new file is 2 FAIL / 15 PASS (vacate + S-N1); with the fix 17 PASS / 0/0/0.

**P1 (pill clear) is harness-blind** (CDP can't see the native file widget pill) → validated by eyeball, per plan §7 / Gate 0.
