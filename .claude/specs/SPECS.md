# Specs

Active in-flight decisions only. Use the `brainstorming` skill to create new
decisions; use `/specs` to manage lifecycle (complete → move to `archive/specs/`).

Reference material lives elsewhere:

- conventions & commands → `CLAUDE.md`
- agent-injected rules → `.claude/rules/`
- API surface → `NAMESPACE` + roxygen in `R/paintr-*.R`
- historical decisions → `archive/specs/`

## Active Decisions

- [checkbox-defaults](checkbox-defaults.md) — initial layer-checkbox state via `checkbox_defaults` arg on the 4 formula-taking exported functions
