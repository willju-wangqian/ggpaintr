---
name: stamp
description: Emit ONE inline knowledge stamp (⟦FINDING⟧) for a source-grounded understanding, on demand. Use when the user says /stamp, "stamp this", "mark this finding", or points at a conclusion worth saving so a later harvest pass can verify+store it. Does NOT verify — emits status=derived-unverified.
---

# /stamp

Emit a single `⟦FINDING⟧` stamp for the finding the user is pointing at (or, if unspecified, the most recent source-grounded understanding in the conversation). The full contract + bar lives in `.claude/rules/knowledge-stamps.md` — this skill just produces one well-formed stamp.

## Do

1. Identify the one finding to capture. If ambiguous, ask which.
2. Choose a **stable kebab `id`** (so re-stamping the same fact later dedupes, not duplicates).
3. Write `claim:` as one standalone declarative sentence.
4. Fill `source="…"` with the exact file/line refs that back it — this is what the verify pass checks. If you cannot name a source ref, the finding isn't ready to stamp (it's a guess) — say so instead.
5. Pick `scope`: `durable` (invariant/architecture → project memory), `campaign` (in-flight effort → add `feature=<slug>`), or `decision` (feeds an ADR).
6. Emit the block verbatim in the canonical grammar, `status=derived-unverified`. **Never verify here.**

## Output shape

```
⟦FINDING id=<slug> status=derived-unverified scope=<…> source="<refs>"⟧
claim: <one sentence>
detail: <optional context>
⟦/FINDING⟧
```

That's it — one block, in the chat, so `/export` captures it. No file writes, no source re-reading to "confirm".
