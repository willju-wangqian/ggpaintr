# ADR 0025 — Implementation plans

Source: [dev/adr/0025-source-shortcut-rename-and-upload-auto-name.html](../../adr/0025-source-shortcut-rename-and-upload-auto-name.html)
Generated: 2026-05-26 by /decision-to-plan
Orchestrator entry point: `/exec-plan dev/plans/0025-source-shortcut-rename-and-upload-auto-name/` (run under `/goal`)

## Merge order

`01 → {02, 03} → {04, 05, 06} → 07`

(Curly braces = parallel group, mergeable in numeric order once all members PASS in-worktree audit.)

## Parallel groups

- **G1**: 01 — rename foundation, no deps
- **G2**: 02, 03 — both depend only on 01; touch disjoint regions of `R/paintr-server.R` (02 owns binding-name + auto-name stamp; 03 owns the per-source observer setup closure for the mutex)
- **G3**: 04, 05, 06 — all depend on 02; 05 also depends on 03 + 04; touch disjoint regions of `R/paintr-server.R` (04 owns `format_code_with_extras` + prologue helper; 05 owns `resolve_upload_source` no-file branch + debounce wrap; 06 owns the snapshot-write loop)
- **G4**: 07 — docs only; depends on 01–06

## Plans

| #  | Slug                                    | Group | Depends on    | Status | One-line summary                                                                                                       |
|----|-----------------------------------------|-------|---------------|--------|------------------------------------------------------------------------------------------------------------------------|
| 01 | rename-companion-to-shortcut            | G1    | —             | PASS   | Hard rename `companion_id_fn` → `shortcut` (boolean) + `node$companion_id` → `node$shortcut_id` + reserved-key validator. |
| 02 | auto-name-derivation                    | G2    | 01            | PASS   | Stamp `node$auto_name` at translate time + bind uploads under it when shortcut textbox is empty.                       |
| 03 | ui-mutex-autoclear                      | G2    | 01            | PASS   | Replace `ptr_upload_autoname` with Q3-B mutex: typing clears file, file-pick clears textbox.                            |
| 04 | code-panel-prologue                     | G3    | 02            | PASS   | Emit `<auto-name> <- read.<ext>(...)` prologue in the code panel for every active upload-bound source.                 |
| 05 | vacate-on-empty-and-debounce            | G3    | 02, 03, 04    | PASS   | A1: `rm()` eval_env binding + NULL `bound_names` on textbox-empty fall-through. A2: 400ms `shiny::debounce` on input read. |
| 06 | spec-roundtrip-bound-names-fallback     | G3    | 01, 02        | PASS   | Snapshot-write loop falls back to `state$bound_names[[key]]()` when textbox is empty, so spec round-trip carries the auto-name. |
| 07 | docs-context-and-vignettes              | G4    | 01–06         | PASS   | Rewrite `vignettes/ggpaintr-customization.Rmd` lines 297/502 + amend `CONTEXT.md` line 48 + any other "exactly one per page" site + add NEWS.md entry. |

## Blocked plans

(Empty as of draft.)

## Worked-example coverage

(Populated by the worked-example coverage sub-agent after the audit reaches PASS.)

| ADR anchor                                                | kind     | summary (one line)                                                                                | Plan#:scenario asserting it                                                                       |
|-----------------------------------------------------------|----------|---------------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------|
| Worked examples — what becomes possible #1                | positive | Custom source opts in to env-shortcut via `shortcut = TRUE` + `textInput(node$shortcut_id, …)`.   | 01: "ADR worked example #1 — custom source opts in to env-shortcut surface" (mechanism)  +  03: "ADR worked example #1 — env-shortcut binds caller-env frame and downstream picker populates" (observable: picker + plot)  |
| Worked examples — what becomes possible #2                | positive | Multi-panel app: two coordinators each get `<obj$id>_<key>` auto-names, no collision.             | 02: "ADR worked example #2 (part A)" + "(part B)" (stamp)  +  04: "ADR worked example #2 — two coordinators on one page bind distinct shared canonicals with no collision" (observable: code panels + prologue) |
| Worked examples — what becomes possible #3                | positive | Spec round-trip reproduces an uploaded-data session via dumped auto-name + caller-env binding.    | 06: "ADR worked example #3 — full round-trip via auto-name"                                       |
| Worked examples — what is rejected by design #4           | negative | Shared key `'shortcut'` rejected at translate time with a named-conflict error.                   | 01: "ADR worked example #4 — shared key 'shortcut' is rejected at translate time"                 |
| Worked examples — what is rejected by design #5           | negative | Concurrent file + typed-text is structurally impossible (mutex auto-clears the inactive side).    | 03: "ADR worked example #5 — typing in textbox resets the sibling fileInput"                      |
| Worked examples — what is rejected by design #6           | negative | `shortcut = TRUE` with no `textInput` in `build_ui` silently has no shortcut input (author trap). | 01: "ADR worked example #6 — shortcut=TRUE without textInput is a silent author trap"             |

## Uncovered worked examples

(Empty as of draft. Populated by the worked-example coverage sub-agent if any example remains UNCOVERED after retry cap.)

## Notes for the orchestrator

- Each parallel group merges in numeric order once all its members have PASSed in-worktree audit.
- Worktree base: each plan's worktree is branched off the orchestrator HEAD *after* the previous group's plans are merged. Plans in the same parallel_group all branch off the same point (the latest merged commit before the group).
- Definition-of-Done command is in every plan (refers to `CLAUDE.md`); run it on the orchestrator branch after every merge.
- The pre-existing `ppUpload(<name>)` boot-empty-consumer-picker bug (ADR 0025 "Known precondition") is OUT OF SCOPE for every plan. Track via `/diagnose` in a separate PR.
- The shinytest2 timing flake (per project memory `shinytest2-appdir-pkgload`) is environmental — re-run before attributing any FAIL to a plan.

<!-- implementable: PASS date=2026-05-26 gate="N/A (manifest)" hash=fcfb5ef7f676 -->

