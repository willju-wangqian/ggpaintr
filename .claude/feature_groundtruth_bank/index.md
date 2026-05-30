# Feature Groundtruth Bank — Index

The map of every feature section. Each section owns one Gherkin `.feature` file under `sections/` that captures the ground-truth behavior of its public entry points, verified against source. This file is **generated** — edit `sections.tsv` to add/rename sections, then re-run the skill or a script. Status is maintained by `update-feature-groundtruth` (sets fresh) and `mark_stale.sh` (sets stale).

| Section | Title | Source paths | Feature file | Status | Last verified |
|---|---|---|---|---|---|
| `rank-1` | Rank 1 (most used) — ptr_app, pp* placeholders, ptr_define_placeholder_* | R/paintr-app.R R/paintr-builtins.R R/paintr-registry.R | [sections/rank-1.feature](sections/rank-1.feature) | ⚠️ STALE | `03781b8` |
| `rank-2` | Rank 2 — ptr_server, ptr_ui (compose-your-own-layout server/UI pair) | R/paintr-app.R R/paintr-server.R R/paintr-build-ui.R | [sections/rank-2.feature](sections/rank-2.feature) | ⚠️ STALE | `—` |
| `rank-3` | Rank 3 — ptr_shared & the shared-coordinator surface | R/paintr-shared-coordinator.R R/paintr-shared-ui.R R/paintr-shared.R | [sections/rank-3.feature](sections/rank-3.feature) | — never — | `—` |
| `rank-4` | Rank 4 — ptr_ui_* composable UI pieces | R/paintr-app.R R/paintr-build-ui.R R/paintr-copy.R | [sections/rank-4.feature](sections/rank-4.feature) | ⚠️ STALE | `—` |
| `rank-5` | Rank 5 — everything else (defaults, extractors, options, init/state, utils) | R/paintr-default-args.R R/paintr-server.R R/paintr-input-spec.R R/paintr-data.R R/paintr-options.R R/paintr-copy.R R/paintr-registry.R R/paintr-render.R R/paintr-eval.R | [sections/rank-5.feature](sections/rank-5.feature) | ⚠️ STALE | `—` |
| `rank-6` | Rank 6 — ptr_llm_* primer registry | R/paintr-llm.R | [sections/rank-6.feature](sections/rank-6.feature) | — never — | `—` |
| `rank-7` | Rank 7 — ptr_app_grid, ptr_app_bslib | R/paintr-app.R R/paintr-app-bslib.R | [sections/rank-7.feature](sections/rank-7.feature) | — never — | `—` |

## Stale details

- **`rank-1`** — 2 changed file(s): R/paintr-builtins.R, R/paintr-registry.R
- **`rank-2`** — 2 changed file(s): R/paintr-build-ui.R, R/paintr-server.R
- **`rank-4`** — 1 changed file(s): R/paintr-build-ui.R
- **`rank-5`** — 2 changed file(s): R/paintr-registry.R, R/paintr-server.R
