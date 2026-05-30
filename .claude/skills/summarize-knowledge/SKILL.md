---
name: summarize-knowledge
description: Summarize what you learned THIS session as a batch of inline ⟦FINDING⟧ stamps, best-effort and UNVERIFIED, so the user can /export before /clear and harvest them later. Takes an optional <topic> argument to focus the summary on specific areas/questions; with no argument, sweeps the whole session. Use when the user says /summarize-knowledge [topic], or "summarize what you learned about X / stamp everything before I clear". Explicitly does NOT verify — a later harvest pass does that.
---

# /summarize-knowledge [topic]

End-of-session knowledge dump. The user invokes this **right before `/clear`**; you emit what you figured out this session as `⟦FINDING⟧` stamps so `/export` captures them. Contract + grammar: `.claude/rules/knowledge-stamps.md`.

## Scope: topic vs full sweep

- **`/summarize-knowledge <topic>`** — the argument names the area(s)/question(s) to focus on (it may be phrased as one or more questions). Summarize **only what you learned about those topics this session**; ignore unrelated findings. If a named topic is one you learned nothing concrete about this session, say so explicitly rather than padding with a vague stamp. Tag each stamp with `tags="<topic-slug>"` so a topic's findings group in the harvest.
- **`/summarize-knowledge`** (no argument) — sweep the whole session for every distinct finding.

## Do

1. **Gather** the relevant source-grounded understandings: invariants, control-flow/topology facts, gotchas, confirmed/refuted hypotheses, "why it's built this way" — anything derived from reading source or running the app. Restrict to the `<topic>` if one was given.
2. **Apply the bar** (`.claude/rules/knowledge-stamps.md`): keep non-obvious + reusable + grounded; drop trivia, restated docs, and transient task mechanics.
3. **Dedupe** into one stamp per distinct fact; give each a stable kebab `id`.
4. For each: `claim:` (one sentence) + `source="…"` (the refs you recall touching) + `scope` (+ `feature=` if campaign) + `tags="<topic-slug>"` when invoked with a topic.
5. Emit all blocks, each `status=derived-unverified`.

## Hard rules

- **Do NOT verify.** Do not re-open source to confirm. Just try your best from session memory — accuracy is the later harvest pass's job ("verified by another pass regularly").
- If `source` for a finding is fuzzy, still emit it with your best-recollection refs; the verify pass will confirm or flag it.
- Best-effort completeness > precision here. Better to over-emit a borderline finding than lose it.
- Output is stamps in the chat only — no file writes.

## Shape

A short lead line (`N findings stamped, unverified — /export to capture`) then the `⟦FINDING⟧ … ⟦/FINDING⟧` blocks back to back.
