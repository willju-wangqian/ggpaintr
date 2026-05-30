---
name: harvest-finding
description: The regular verify-and-store pass over knowledge stamps. Collect /export-ed conversation transcripts, extract every ⟦FINDING⟧ stamp, verify each against current source, then save the verified knowledge as a timestamped JSON under .claude/harvest-findings/raw_knowledge/. Use when the user says /harvest-finding, "harvest the stamps", "process exported conversations", or wants to turn collected findings into verified project knowledge. Also mines unstamped old sessions by token (fallback).
---

# harvest-finding

Turns raw stamped transcripts into **verified knowledge**, saved as one JSON file. This is the pass `/stamp` and `/summarize-knowledge` defer verification to. Contract + grammar: `.claude/rules/knowledge-stamps.md`. Scripts in `scripts/`.

## Folders (all under `.claude/harvest-findings/`)

- `exports/` — the **`/export` inbox**: the user runs `/export .claude/harvest-findings/exports/<date>-<topic>.txt` (plain text, use a `.txt`/`.md` extension or the parser skips it). Gitignored.
- `raw_conversation/` — pooled corpus for a harvest run (collect.sh dest). Gitignored.
- `raw_knowledge/` — the verified-knowledge JSON output. **Tracked.**

## Workflow

1. **Collect** — pool the exported conversations from the inbox into the corpus:
   `scripts/collect.sh .claude/harvest-findings/exports`
   (default `--dest` is `.claude/harvest-findings/raw_conversation`).
2. **Extract** — parse all stamps, dedupe by `id` (newest transcript wins):
   `scripts/extract-stamps.py .claude/harvest-findings/raw_conversation/ > /tmp/stamp-ledger.json`
   (`--md` for a human view). Reports count + origin transcript per stamp.
3. **Verify** — for **each** stamp, open every ref in `source=` against *current* code and judge the `claim`/`detail`. Load-bearing step — a stamp is a cross-session claim, not proof (project anti-hallucination rules apply). Assign a verdict:
   - `verified` — refs exist and back the claim.
   - `contradicted` — refs exist but the claim is wrong/stale. Keep it (with the correction), never silently drop.
   - `unconfirmable` — refs moved/gone or too vague to check.
4. **Save** — write the verified knowledge as ONE JSON file:
   ```
   d=$(date +%Y-%m-%d-%H%M)
   mkdir -p .claude/harvest-findings/raw_knowledge
   # write .claude/harvest-findings/raw_knowledge/$d.json   (schema below)
   ```
5. **Report** — table of `id` → verdict, and the path written. Surface every `contradicted`/`unconfirmable` for follow-up.

### Output JSON schema (`.claude/harvest-findings/raw_knowledge/<date>-<time>.json`)

```json
{
  "harvested_at": "2026-05-29T14:32",
  "corpus": ".claude/harvest-findings/raw_conversation/ (12 transcripts)",
  "counts": { "verified": 7, "contradicted": 1, "unconfirmable": 2 },
  "findings": [
    {
      "id": "shared-source-emission-topology",
      "claim": "single declarative sentence",
      "detail": "optional longer context",
      "scope": "durable",
      "feature": "",
      "source": "R/paintr-shared-ui.R; app.R:959",
      "verdict": "verified",
      "verification": "what you checked and what you found (1-2 lines)",
      "checked_refs": ["R/paintr-shared-ui.R:120-140", "app.R:959"],
      "origin_transcript": ".claude/harvest-findings/raw_conversation/20260529T0727__chat.txt",
      "origin_status": "derived-unverified"
    }
  ]
}
```

- One file per harvest run (append-only history; never overwrite a prior `<date>-<time>.json`).
- `raw_knowledge/` is the staging output — a *later* pass promotes verified findings into project memory / `.scratch/` / ADRs. harvest-finding stops at this JSON.
- Preserve every stamp field; only `verdict`, `verification`, `checked_refs` are added by this pass.

## Fallback — mine an UNSTAMPED past session by token

For findings from sessions predating stamping (no `⟦FINDING⟧`):
1. `scripts/locate.sh "<distinctive token>" [path-filter]` → ranked candidate `.jsonl` logs (all worktrees; excludes current session).
2. Extract the block:
   `jq -r 'select(.type=="assistant")|.message.content[]?|select(.type=="text").text' <file> | grep -n "<token>"`, then read around it.
3. Treat the extracted text as one finding, synthesize the same fields (id/claim/source/scope), then run **Verify → Save** (steps 3–4) into the same JSON.

## Notes

- The current session's own log self-matches if a finding was pasted into chat — `locate.sh` drops it when `$CLAUDE_SESSION_ID` is set.
- `/export` writes markdown; `extract-stamps.py` is format-agnostic (greps the sentinels in any text), so it works wherever `/export` lands.
