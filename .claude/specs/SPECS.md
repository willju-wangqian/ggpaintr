# Specs

Active in-flight decisions only. Use the `brainstorming` skill to create new
decisions; use `/specs` to manage lifecycle (complete → move to `archive/specs/`).

Reference material lives elsewhere:

- conventions & commands → `CLAUDE.md`
- agent-injected rules → `.claude/rules/`
- API surface → `NAMESPACE` + roxygen in `R/paintr-*.R`
- historical decisions → `archive/specs/`

## Active Decisions

- [core-rewrite](core-rewrite.md) — typed AST + 12 visitor passes; replaces flat-map runtime, splits source/consumer placeholder roles, makes pipes structural (linked BDD: [core-rewrite-bdd.md](core-rewrite-bdd.md))
- [checkbox-defaults](checkbox-defaults.md) — initial layer-checkbox state via `checkbox_defaults` arg on the 4 formula-taking exported functions
- [p9-relax](p9-relax.md) — positional missing drops arg, not whole call; rename `default_safe_to_remove` → `default_drop_when_empty` (linked BDD: P9, P12, G2, G6)
- [stage-disable-checkbox](stage-disable-checkbox.md) — per-data-manipulation-call checkbox as gate-2 runtime safety valve composing with relaxed P9; new `R/paintr-disable.R` + P4 stage-id hook + `disable_walk` before substitute (linked BDD: new §G11/§P13)
