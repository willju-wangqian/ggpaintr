# ADR 0028 — Implementation plans

Source: [dev/adr/0028-plotly-linked-selection-helpers.html](../../adr/0028-plotly-linked-selection-helpers.html)
Generated: 2026-06-10 by /decision-to-plan
Orchestrator entry point: `/exec-plan dev/plans/0028-plotly-linked-selection-helpers/` (run under `/goal`)

## Merge order

`01 → 02 → 03 → 04`

(No parallel groups — the chain is linear: 02 extends 01's file and store; 03's fixture consumes both helpers; 04 injects against 03's fixture and appends to its test file.)

## Parallel groups

- **G1**: 01
- **G2**: 02   (same source file as 01 — sequential by construction)
- **G3**: 03
- **G4**: 04   (appends to 03's test file)

## Plans

| #  | Slug | Group | Depends on | Status | One-line summary |
|----|------|-------|------------|--------|------------------|
| 01 | plotly-ggplotly-core | G1 | — | PASS | New `R/paintr-plotly.R`: call-time plotly guard, per-draw key minting (+ `aes(key=)` warn-once override, silent `.ptr_row` overwrite), derived source ids, `session$userData` store, `ptr_ggplotly()` returning a plain plotly object; tier-1 unit tests. |
| 02 | plotly-selection | G2 | 01 | PASS | `ptr_plotly_selection(state, mode = c("rows","flag"))`: rows/flag projections of the one selection, zero-row-same-columns empty state, silent `.ptr_selected` overwrite, reset-on-draw + deselect-clears, keyless-event empty state; tier-1 unit tests + the two negative-example pins. |
| 03 | plotly-e2e-boot | G3 | 01, 02 | PASS | e2e fixture `adr28-plotly-linked` encoding the ADR headline composition (live mode, plotly-rendered instance 1, selection-fed formula instance 2, rows table); boot assertions: widget renders, no inline errors, zero-row table, pickers seeded. |
| 04 | plotly-e2e-roundtrip | G4 | 03 | PASS | Tier-3 round-trip via `app$set_inputs("plotly_selected-ptr_e2e", allow_no_input_binding_ = TRUE)`: table fill, flag-mode redraw, deselect-clears, redraw-resets; plus `adr28-plotly-trigger-misuse` fixture pinning `draw_trigger = sel` as silently inert. |

All four: /implementable PASS (stamped) + per-plan drift audit PASS (D1–D7; 01/02/04 fixed-and-re-audited once, 03 clean first pass) + global worked-example coverage PASS (2026-06-10).

## Blocked plans

(none — all four reached PASS within the retry caps.)

## Worked-example coverage

Global coverage audit: **PASS** (2026-06-10). All 3 positive + 4 negative ADR worked examples map to ≥ 1 concrete BDD `Then` in the plan union; negative examples assert their exact rejection signal.

| anchor | kind | summary (one line) | status | plan#:scenario |
|---|---|---|---|---|
| what becomes possible #1 | positive | Brush selection drives a second ggpaintr formula instance (highlight in the formula, zero trigger wiring) | COVERED | plan-04: "the brush drives a flag-mode redraw of the selection-fed instance" — Then: instance 2's plot HTML changes from its pre-injection snapshot (poll_html changed-detector) + no inline error |
| what becomes possible #2 | positive | Selected slice as an ordinary table; zero-row-same-columns empty state makes the render unconditional | COVERED (both halves) | empty-state half — plan-03: "the rows table renders unconditionally" — Then: rendered table with zero data rows. Table-fill half — plan-04: "selected rows appear in the rows-mode table" — Then: exactly 2 data rows after injecting keys c(2, 5) |
| what becomes possible #3 | positive | `ptr_ggplotly()` returns a plain plotly object; user keeps piping plotly verbs after it | COVERED | plan-01: "composing extra plotly layout after the helper" — Then: inherits class "plotly" AND the `plotly::layout()` pipe succeeds, still "plotly" |
| what is rejected by design #1 | negative | Brushing a smooth/post-stat layer yields nothing — selection stays at its empty state | COVERED | plan-02: "selecting on a stat-transformed layer yields nothing" — Then: zero-row same-columns rows projection + all-FALSE flag (returned-value sentinel) |
| what is rejected by design #2 | negative | Keys are per-draw; any new draw resets the selection to empty | COVERED | plan-04: "a redraw resets the selection" — Then: table returns to zero data rows, old selection NOT carried over (plan-02 additionally pins it at the unit tier) |
| what is rejected by design #3 | negative | `draw_trigger = sel` silently never redraws (click-counter contract) | COVERED | plan-04: "the selection reactive as draw_trigger silently never redraws" — Then: instance 2's plot HTML identical to pre-injection snapshot, with the live-mode redraw scenario as named positive control |
| what is rejected by design #4 | negative | A bare reactive as the pipeline head (missing `()`) sends a closure into `ggplot()` | COVERED | plan-02: "a bare reactive as the pipeline head is an error" — Then: error message contains the exact substring `` `data` cannot be a function `` |

## Uncovered worked examples

(none.)

## Deferred to future rounds (per the ADR — no plans produced)

- `renderer = "plotly"` backend on `ptr_app()`/`ptr_ui()` and a `state$selection()` slot on `ptr_state` (Decision 1, explicitly rejected for v1).
- A dedicated `ggpaintr-plotly` vignette with fixture mirror + build gate (Decision 7 — "deferred, not rejected").
- Per-instance `gate_draw` override for mixed apps (Consequences — "future work").

Note: the two doc fixes in the ADR's Consequences (the `draw_trigger` click-counter roxygen and the `gate_draw = TRUE` qualification in `.claude/rules/testing.md`) landed WITH the ADR commit (verified at `7360fb7`: `R/paintr-server.R:45`, `.claude/rules/testing.md:64-68`) — no plan needed.

## Notes for the orchestrator

- Each plan merges in numeric order once it has PASSed in-worktree audit (no parallel groups in this set).
- Worktree base: each plan's worktree is branched off the orchestrator HEAD *after* the previous plan was merged.
- Definition-of-Done command is in every plan (CLAUDE.md §"Authoritative gate"); run it on the orchestrator branch after every merge.
- PLAN-04's injected-input contract is internal to plotly — its sanctioned fallback (drive the selection reactive directly) must be *reported* if taken, never silent.
<!-- implementable: PASS date=2026-06-10 gate="Rscript -e 'devtools::test(reporter=\"progress\", stop_on_failure=FALSE)'" hash=2748c238bb75 -->
