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

### When to stamp (the bar)

Stamp only what is **non-obvious, derived from reading the source/running the app, and reusable**: an invariant, a topology/control-flow fact, a gotcha, a "why it's built this way", a confirmed/denied hypothesis. **Do not** stamp trivia, restated docs, transient task mechanics, or anything you didn't actually ground in code/behavior.

### How it's enforced

- **This rule** = best-effort inline habit (no hook can detect "you just understood something"; you *will* miss some — that's expected).
- **`/stamp`** — emit one stamp on demand the moment the user (or you) spots a keeper.
- **`/summarize-knowledge`** — invoked before `/clear`; sweep the whole session and dump every finding as stamps, best-effort, **unverified** (`status=derived-unverified`). Do **not** verify here — just try your best; a later pass checks.
- **`harvest-finding`** skill = the regular pass: read the stamps in `raw_conversation/` (written directly by `/summarize-knowledge` — no `/export` step; `collect.sh` can optionally pool in other transcript sources) → extract → **verify** each `source:` against current code → save the verified knowledge as a timestamped JSON under `.claude/harvest-findings/raw_knowledge/<date>-<time>.json`.
- **`/process-finding`** = the categorize pass *after* harvest: read `raw_knowledge/*.json` → assign each finding one or more topic **labels** (written back onto the finding) → maintain the canonical vocabulary `.claude/harvest-findings/labels.json` and regenerate `.claude/harvest-findings/INDEX.md` (label → description → finding ids). A later pass promotes findings into memory/`.scratch`/ADRs by label.

Pipeline: **stamp → summarize-knowledge → harvest-finding → process-finding** (write side) ; **consult INDEX.md** (read side, below).

### Recall — rebuild understanding from the index

When you need to rebuild knowledge about a topic (a subsystem, a past bug, *why* something is built a certain way), **consult the index first** before re-deriving it from scratch. On demand: **`/recall-finding <topic>`** — it matches the topic against the labels + finding text, pulls the verified findings (with sources + verdicts), and tells you plainly when the index has nothing. Manual fallback: `grep -i "<topic>" .claude/harvest-findings/INDEX.md` → label, then `grep -rl "<label>" .claude/harvest-findings/raw_knowledge/` → the findings. The pipeline above *writes* hard-won understanding; this is how you *read* it back.

### Folders (`.claude/harvest-findings/`)

Bootstrap a fresh clone/worktree/project with `/harvest-init` (creates the dirs + gitignore, confirms first, idempotent).

- `raw_conversation/` — the knowledge corpus: `<date>-<time>-<topic>.md` stamp files written directly by `/summarize-knowledge` (the parser greps the sentinels in any `.md`/`.txt`/`.json`; `collect.sh` can pool in other transcript sources). Gitignored.
- `raw_knowledge/` — verified-knowledge JSON output (each finding gains a `labels` array). Tracked.
- `labels.json` — canonical `{label: description}` vocabulary maintained by `/process-finding`. Tracked.
- `INDEX.md` — generated label catalog (the read-side map). Tracked.
