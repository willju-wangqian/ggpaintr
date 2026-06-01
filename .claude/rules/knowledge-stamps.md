## Knowledge Stamps — the contract

When you derive a **reusable, source-grounded understanding** while working this repo, emit an inline **stamp** so it can be harvested later: `/summarize-knowledge` writes your stamps into `.claude/harvest-findings/raw_conversation/`, `harvest-finding` greps + verifies them into `raw_knowledge/`, and `/process-finding` files them by topic into the index. This is how hard-won findings survive past a single session instead of evaporating on `/clear`.

### Grammar (canonical — the parser keys off this exactly)

```
⟦FINDING id=<kebab-slug> status=derived-unverified scope=<durable|campaign|decision> source="<file[:lines]; …>"⟧
claim: <ONE declarative sentence — the reusable fact, standalone>
detail: <optional, may span multiple lines until the close tag>
⟦/FINDING⟧
```

- Open tag `⟦FINDING …⟧` and close tag `⟦/FINDING⟧` each on **their own line**. Sentinels are U+27E6 `⟦` / U+27E7 `⟧` (grep target: `⟦FINDING`).
- **Attributes** (on the open tag): `id` (required — stable kebab key; harvest dedupes by it, latest wins), `status` (always `derived-unverified` when you emit — verification is a *separate* pass, never inline), `scope` (routing hint, see below), `source` (required, quoted — the load-bearing file/line refs the verify pass will check). Optional: `feature=<slug>` (campaign routing), `tags="a,b"`.
- **Body**: a required `claim:` line + optional `detail:`/free prose. Keep `claim` to one sentence so it reads standalone in a ledger.
- **`scope`**: `durable` = an architecture/invariant fact worth project memory; `campaign` = working knowledge for an in-flight effort (`feature=`); `decision` = feeds a locked decision / ADR.

### What a finding is — the bar

A finding is a **reading companion for source code**: a HOW or WHY a fresh session reads *before* the code, to orient faster. It is **never a fact that competes with source** — the source is the only source of truth and is always more factual than any restatement, so a finding earns its place only by adding orientation the code itself doesn't immediately hand over.

Save a candidate only if it clears **all four**:

1. **Saves rediscovery** — without it, a fresh session would burn tokens reconstructing this by reading/tracing the code.
2. **Is HOW or WHY** — how an entity works (a non-obvious control flow / topology / mechanism) or why it is shaped this way (the rationale a diff/commit doesn't show) — not merely *that* it exists.
3. **Beats source on orientation, not on facts** — it adds the map, the relationship, the rationale, or the gotcha. A pure fact, a restated doc/CLAUDE.md line, or a transient/point-in-time number is **not** a finding.
4. **Anchors to code you'd open** — it names the symbol/file a future session will read; the index points there, the finding pre-loads the understanding, then they read the code.

**Kill test:** *if a fresh session read the cited source (plus CLAUDE.md), would this finding still have saved them real discovery effort?* If no, it is **not** a finding — drop it, even when it is perfectly true. Truth is necessary but not sufficient; **value (orientation that saves rediscovery) is the bar.** (Definition lives in the global template `~/.claude/skills/harvest-init/templates/knowledge-stamps.md`; this project adopts it.)

### How it's enforced — a value gate at every stage

The bar is not a one-time emit check; **every** stage re-applies it. A candidate that is *true but below the bar* is dropped, never stored "because it verified".

- **This rule** = best-effort inline habit (no hook can detect "you just understood something"; you *will* miss some — that's expected). Emit `⟦FINDING⟧` only for candidates that pass the kill test.
- **`/summarize-knowledge`** — invoked before `/clear`; sweep the session and write every *bar-passing* finding as stamps **directly into `.claude/harvest-findings/raw_conversation/<date>-<time>-<topic>.md`**, best-effort, **unverified** (`status=derived-unverified`). Don't verify here; do apply the bar (drop trivia / restated docs / transient mechanics).
- **`harvest-finding`** skill = the verify-and-store pass, which is **also a value pass**: read the stamps in `raw_conversation/` → extract → **verify** each `source:` against current code AND apply the kill test. A candidate that verifies TRUE but fails the bar is **dropped, not stored**. Survivors are saved as a timestamped JSON under `.claude/harvest-findings/raw_knowledge/<date>-<time>.json`.
- **`/process-finding`** = the categorize pass *after* harvest: read `raw_knowledge/*.json` → assign topic **labels** → maintain `.claude/harvest-findings/labels.json` + regenerate `.claude/harvest-findings/INDEX.md`. The index IS the "read this before the code" map, so every indexed entry must pass the kill test — flag any that don't for archival rather than labeling them.
- **`/mark-stale-finding`** = the maintenance **detector** (pure fast script, no model): reads the git diff and flips a finding to `status=stale` when a file it cites changed since its `verified_at_commit` baseline. Freshness is *orthogonal* to verdict — a `verdict=verified` finding can be `status=stale`. Runnable as a git hook.
- **`/maintain-finding`** = the maintenance **re-verifier** (model judgment): re-runs harvest-finding's verify+value contract over the `status=stale` subset, refreshing `verdict`/`verification` and re-stamping `verified_at_commit=HEAD` + `status=fresh` — and **archives a still-true finding that no longer earns its place** (redundant with docs / decayed to a trivial fact) as `verdict=below_bar`, distinct from `contradicted` (false).

Pipeline: **stamp (inline) → summarize-knowledge → harvest-finding → process-finding** (write side) ; **mark-stale-finding → maintain-finding** (maintenance loop) ; **consult INDEX.md** (read side, below).

### Recall — rebuild understanding from the index

When you need to rebuild knowledge about a topic (a subsystem, a past bug, *why* something is built a certain way), **consult the index first** before re-deriving it from scratch. On demand: **`/recall-finding <topic>`** — it matches the topic against the labels + finding text, pulls the verified findings (with sources + verdicts), and tells you plainly when the index has nothing. Manual fallback: `grep -i "<topic>" .claude/harvest-findings/INDEX.md` → label, then `grep -rl "<label>" .claude/harvest-findings/raw_knowledge/` → the findings. The pipeline above *writes* hard-won understanding; this is how you *read* it back.

### Folders (`.claude/harvest-findings/`)

Bootstrap a fresh clone/worktree/project with `/harvest-init` (creates the dirs + gitignore, confirms first, idempotent).

- `raw_conversation/` — the knowledge corpus: `<date>-<time>-<topic>.md` stamp files written directly by `/summarize-knowledge` (the parser greps the sentinels in any `.md`/`.txt`/`.json`). Gitignored.
- `raw_knowledge/` — verified-knowledge JSON output (each finding gains a `labels` array). Tracked.
- `labels.json` — canonical `{label: description}` vocabulary maintained by `/process-finding`. Tracked.
- `INDEX.md` — generated label catalog (the read-side map). Tracked.
- `archive/archived.json` — findings that re-verification found **false today** (no longer hold), moved out of `raw_knowledge/` by `/maintain-finding`. Preserved as history with `archived_reason` + `archived_at_commit`; not re-indexed for recall. Created on first archive. Tracked.
