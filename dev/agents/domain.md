# Domain Docs

How the engineering skills should consume this repo's domain documentation when exploring the codebase.

## Before exploring, read these

- **`CLAUDE.md`** at the repo root — this repo has no separate `CONTEXT.md`; the domain overview, conventions, and architecture notes live in `CLAUDE.md` (and its Codex twin `AGENTS.md`).
- **`dev/adr/`** — read the ADRs that touch the area you're about to work in.

If `dev/adr/` is empty or absent, **proceed silently**. Don't flag its absence or suggest creating ADRs upfront; the producer skill (`/grill-with-docs`) creates them lazily when decisions actually get resolved.

## File structure

Single-context repo:

```
/
├── CLAUDE.md          ← domain overview + conventions (no separate CONTEXT.md)
├── AGENTS.md          ← Codex twin of CLAUDE.md
├── dev/adr/
│   ├── 0001-polish-default-fluidpage-ui.md
│   └── 0002-public-api-tiering-and-trims.md
└── R/
```

ADRs live under `dev/adr/`, not `docs/adr/` — `docs/` is owned by pkgdown's generated site and is gitignored.

## Use the project's vocabulary

When your output names a domain concept (an issue title, a refactor proposal, a hypothesis, a test name), use the term as it's used in `CLAUDE.md` and the existing `R/paintr-*.R` source. Don't drift to synonyms. If the concept you need isn't established anywhere, that's a signal — either you're inventing language the project doesn't use (reconsider) or there's a real gap (note it).

## Flag ADR conflicts

If your output contradicts an existing ADR, surface it explicitly rather than silently overriding:

> _Contradicts ADR-0002 (public API tiering) — but worth reopening because…_
