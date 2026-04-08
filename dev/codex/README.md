# Codex Publication Workflow

This directory contains the unattended Codex CLI workflow for publication-readiness work on `ggpaintr`.

## What it runs

`dev/codex/run-publication-workflow.sh` runs:

1. `dev/notes/prompts/publication-improvements-batch-1-prompt.md`
2. a structured reassessment using `dev/notes/prompts/publication-readiness-cran-prep-prompt.md`
3. `dev/notes/prompts/publication-improvements-batch-2-3-prompt.md`
4. another structured reassessment
5. a follow-up unattended publication loop using `dev/codex/run-publication-loop.sh`

Each phase writes its prompt, JSONL event stream, and structured result into a timestamped directory under `dev/codex/runs/`.

## Local usage

Dry-run the workflow shape:

```bash
CODEX_WORKFLOW_DRY_RUN=1 ./dev/codex/run-publication-workflow.sh
```

Run it locally with sandboxed automatic execution:

```bash
CODEX_LOOP_MODE=safe ./dev/codex/run-publication-workflow.sh
```

Run it in a fully external sandboxed environment:

```bash
CODEX_LOOP_MODE=danger ./dev/codex/run-publication-workflow.sh
```

Useful environment variables:

- `CODEX_LOOP_AGENT_COUNT`
- `CODEX_WORKFLOW_FOLLOWUP_MAX_PASSES`
- `CODEX_LOOP_MAX_PASSES`
- `CODEX_LOOP_MODEL`
- `CODEX_LOOP_ENABLE_SEARCH=1`
- `CODEX_WORKFLOW_SKIP_FOLLOWUP_LOOP=1`
- `CODEX_WORKFLOW_RUN_DIR=/custom/path`

## Docker usage

Build the image:

```bash
UID="$(id -u)" GID="$(id -g)" \
docker compose -f docker-compose.codex.yml build
```

Run the full unattended workflow:

```bash
UID="$(id -u)" GID="$(id -g)" \
docker compose -f docker-compose.codex.yml run --rm codex-publication
```

Open a shell in the same environment:

```bash
UID="$(id -u)" GID="$(id -g)" \
docker compose -f docker-compose.codex.yml run --rm codex-publication shell
```

The compose file mounts:

- the repo at `/workspace/ggpaintr`
- `${HOME}/.codex` at `/tmp/.codex` for Codex credentials/config

Passing `UID` and `GID` from the host is recommended so pkgdown and other
verification steps write files with your host user instead of leaving behind
hard-to-edit permission states in `docs/`.

If you prefer API-key auth, export `OPENAI_API_KEY` before launching the container.

## Artifacts

Each run creates a timestamped directory like:

`dev/codex/runs/20260408-120000/`

Look at:

- `prompt.md` for the exact prompt given to Codex
- `events.jsonl` for the JSONL event stream
- `result.json` for the structured final output

## Skill note

No additional skill installation is required for this workflow. It uses the repo-local skill at `.agents/skills/publication-loop/SKILL.md`.

If you later want the same workflow pattern in other repositories, the next step would be to promote that repo-local skill into a reusable installed skill, but this setup works as-is.
