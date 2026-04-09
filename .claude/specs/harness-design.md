---
name: harness-design
type: decision
status: accepted
scope: [harness, workflow, agents, rules]
created: 2026-04-09
---

# Claude Code Harness Design

## Understanding Summary

- **What:** A reusable Claude Code harness — structured system of rules, workflows, agents, and a dispatcher that standardizes how Claude behaves across projects.
- **Why:** Current workflow is ad-hoc (prompt contracts, manually sequenced agents, scattered conventions). Needs consistency, discoverability, and portability.
- **Who:** The user, and eventually other Claude Code users who install the harness.
- **Key constraints:** Language-agnostic; lives at `~/.claude/harness/`; designed to evolve into a distributable package later; YAML frontmatter + prose body for rules; dispatcher skill (`/workflow <name>`) orchestrates workflows; hybrid autonomy (structural + compliance gates); rules injected into agent prompts by default with post-step compliance for critical rules.
- **Non-goals:** Not a language-specific linter; not replacing existing skills (brainstorming, graphify, specs); not building a package manager yet.

## Decision

### Architecture: 3-Layer Model

| Layer | Purpose | Location |
|---|---|---|
| Rules (reusable) | Coding standards, testing procedures, review checklists | `~/.claude/harness/rules/` |
| Workflows (reusable) | Sequenced steps referencing rules and agent roles | `~/.claude/harness/workflows/` |
| Agents (reusable templates + project overrides) | Agent behavior definitions | `~/.claude/harness/agents/` + `.claude/agents/` |

### Directory Structure

```
~/.claude/harness/
├── HARNESS.md              # Entry point
├── rules/
│   ├── coding.md
│   ├── testing.md
│   ├── review.md
│   └── knowledge.md
├── workflows/
│   ├── add-feature.md
│   ├── refine-feature.md
│   ├── fact-check.md
│   ├── knowledge-update.md
│   ├── clean-up.md
│   └── init.md
└── agents/
    ├── coder.md
    ├── reviewer.md
    ├── tester.md
    └── compliance.md
```

### Dispatcher

Single skill at `~/.claude/skills/workflow/SKILL.md`. Invoked via `/workflow <name> [description]`. Reads workflow YAML, resolves rules, spawns agents with injected rules, manages gates.

### Rule Schema

YAML frontmatter (name, description, scope, severity, inject_into, verify) + prose body. Severity `default` = injected into agent prompts. Severity `critical` = post-step compliance check via compliance agent.

### Workflow Schema

YAML frontmatter (name, description, trigger) + step list. Each step: name, agent (or skill), gate type (structural/auto), rules list, description.

### Gate Types

- **structural**: Workflow-defined. Dispatcher pauses for user confirmation.
- **compliance**: Severity-driven. Auto-triggered when step rules include `severity: critical`. Compliance agent runs verification.
- **auto**: No pause unless step fails.

### Agent Override & Extension

- Agents: project `.claude/agents/` overrides harness. Supports `extends: harness:<name>`.
- Rules: harness + project concatenated (both apply).
- Workflows: project replaces harness entirely (no merge).

### Rule Injection

Agent prompts built in order: template base → Active Rules (full prose) → Prior Steps (compressed summaries) → Task → Project Context (from CLAUDE.md Harness Config).

### Compliance Flow

After step completion → filter step rules to `severity: critical` → if any, spawn compliance agent (haiku, read-only) → run verify fields → pass = proceed, fail = gate for user.

### Project Integration Levels

1. **Zero config**: Dispatcher works with generic defaults, asks inline for missing values.
2. **Init'd**: `/workflow init` adds `## Harness Config` section to CLAUDE.md.
3. **Customized**: Project-level agents (extends), rules (supplements), workflows (overrides).

### Prompt Contract Migration

Existing ggpaintr prompt contracts ("update knowledge", "fact-check docs", "clean up repo") migrate into harness workflows. CLAUDE.md retains architecture, conventions, and Harness Config only.

## Alternatives Considered

1. **Hook-driven orchestration**: Workflows triggered via hooks and shell scripts. Rejected: hooks can't express agent sequencing, debugging is painful, brittle across Claude Code updates.
2. **Per-workflow skills**: Each workflow as its own skill file. Rejected: not discoverable, duplicates dispatcher logic, harder to maintain.
3. **Separate harness.yml per project**: Rejected in favor of CLAUDE.md Harness Config section to avoid extra files.
4. **Standalone doc-writer agent**: Rejected — too narrow. Doc-writing absorbed into coding rule applied to coder agent.
5. **Full post-step checks for all rules**: Rejected — context bloat. Only critical rules get compliance checks.

## Acceptance Criteria

- [ ] `~/.claude/harness/` contains all rule, workflow, and agent files
- [ ] `/workflow list` shows all available workflows
- [ ] `/workflow init` bootstraps a project's CLAUDE.md with Harness Config
- [ ] `/workflow add-feature` completes full pipeline: plan → code → review → test
- [ ] Structural gates pause for user confirmation
- [ ] Critical rules trigger compliance checks
- [ ] ggpaintr prompt contracts removed from CLAUDE.md and replaced by workflows
- [ ] A new project can use the harness with only `/workflow init`

## Decision Log

| # | Decision | Alternatives | Rationale |
|---|---|---|---|
| 1 | Language-agnostic harness | R-specific; mixed with extensions | Harness governs Claude's process, not tooling |
| 2 | Dispatcher skill (`/workflow`) | Prompt contracts; per-workflow skills | Single entry point, discoverable, composable |
| 3 | Hybrid autonomy (structural + compliance gates) | Fully autonomous; fully gated | Low-risk steps run fast; high-risk steps get human eyes |
| 4 | YAML frontmatter + prose rules | Pure prose; executable rules | Queryable metadata without requiring verification commands for every rule |
| 5 | `~/.claude/harness/` now, package later | Git repo with symlinks; package from day 1 | Start simple, structured format enables future packaging |
| 6 | Project config in CLAUDE.md only | Separate harness.yml; both | Zero extra files; CLAUDE.md is already read by Claude |
| 7 | Prompt contracts migrate to workflows | Keep both; keep contracts only | Single source of truth for procedural behavior |
| 8 | Rules injected into agents; compliance for critical only | All post-step checks; agent-only | Balances enforcement rigor with context efficiency |
| 9 | Agents: project overrides; Rules: concatenated; Workflows: project replaces | All-or-nothing override | Agents need full control; rules need both layers; workflows are atomic sequences |
| 10 | Reviewer is new agent; doc-writer absorbed into coding rule | Standalone doc-writer agent | Review is a genuine gap; doc-writing is too narrow for its own agent |
