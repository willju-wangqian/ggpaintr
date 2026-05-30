## Knowledge Stamps — the contract

When you derive a **reusable, source-grounded understanding** while working this repo, emit an inline **stamp** so it can be harvested later (the user `/export`s the conversation, a script greps the stamps, a regular pass verifies + stores them). This is how hard-won findings survive past a single session instead of evaporating on `/clear`.

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
- **`harvest-finding`** skill = the regular pass: collect `/export`ed transcripts → extract stamps → **verify** each `source:` against current code → save the verified knowledge as a timestamped JSON under `.claude/harvest-findings/raw_knowledge/<date>-<time>.json` (a later pass promotes it into memory/`.scratch`/ADRs).

### Folders (`.claude/harvest-findings/`)

Bootstrap a fresh clone/worktree/project with `/harvest-init` (creates the dirs + gitignore, confirms first, idempotent).

- `exports/` — `/export` inbox: `/export .claude/harvest-findings/exports/<date>-<topic>.txt` (plain text; `.txt`/`.md` extension required or the parser skips it). Gitignored.
- `raw_conversation/` — pooled corpus for a harvest run (`collect.sh` dest). Gitignored.
- `raw_knowledge/` — verified-knowledge JSON output. Tracked.
