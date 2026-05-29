---
name: summarize-knowledge
description: Sweep the whole current session and dump every source-grounded understanding as a batch of inline ⟦FINDING⟧ stamps, best-effort and UNVERIFIED, so the user can /export before /clear and harvest them later. Use when the user says /summarize-knowledge, or "summarize what you learned / stamp everything before I clear". Explicitly does NOT verify — a later harvest pass does that.
---

# /summarize-knowledge

End-of-session knowledge dump. The user invokes this **right before `/clear`**; you scan everything you figured out this session and emit it as `⟦FINDING⟧` stamps so `/export` captures them. Contract + grammar: `.claude/rules/knowledge-stamps.md`.

## Do

1. **Sweep the session** for distinct source-grounded understandings: invariants, control-flow/topology facts, gotchas, confirmed/refuted hypotheses, "why it's built this way" — anything derived from reading source or running the app.
2. **Apply the bar** (`.claude/rules/knowledge-stamps.md`): keep non-obvious + reusable + grounded; drop trivia, restated docs, and transient task mechanics.
3. **Dedupe** into one stamp per distinct fact; give each a stable kebab `id`.
4. For each: `claim:` (one sentence) + `source="…"` (the refs you recall touching) + `scope` (+ `feature=` if campaign).
5. Emit all blocks, each `status=derived-unverified`.

## Hard rules

- **Do NOT verify.** Do not re-open source to confirm. Just try your best from session memory — accuracy is the later harvest pass's job ("verified by another pass regularly").
- If `source` for a finding is fuzzy, still emit it with your best-recollection refs; the verify pass will confirm or flag it.
- Best-effort completeness > precision here. Better to over-emit a borderline finding than lose it.
- Output is stamps in the chat only — no file writes.

## Shape

A short lead line (`N findings stamped, unverified — /export to capture`) then the `⟦FINDING⟧ … ⟦/FINDING⟧` blocks back to back.
