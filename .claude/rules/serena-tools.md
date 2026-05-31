---
name: serena-tools
description: Routing rule for Serena vs Read/Grep/Bash; Serena is the right hammer only for some nails
scope: project
severity: guidance
inject_into: [assessor, browser-tester, coder, compliance, design-facilitator, planner, reviewer, scanner, tester]
verify: Agent output shows Serena `get_symbols_overview` / `find_symbol` used when navigating named symbols in `R/paintr-*.R`, AND uses Read/Grep/Bash (not Serena) for `.Rmd` / `.md` / `.yml` / fixture `app.R` / test files / cross-file text searches / runtime probes. Serena edit tools (`replace_symbol_body`, `insert_after_symbol`, `rename_symbol`, etc.) are NOT used from sub-agents in nested worktrees.
---

## Serena for R Source — Use the Right Tool for the Target

Serena is a structural lens on R symbol trees. It pays off on large `R/paintr-*.R` files with many named top-level functions, and only there. It is not a general-purpose file reader, a runtime debugger, or a cross-file search engine — reaching for it on those targets burns tool calls without paying off.

### Routing table

| Target | Use | Why |
|---|---|---|
| `R/paintr-*.R`, navigating a *named* function in a big file | Serena `get_symbols_overview` → `find_symbol include_body=true` | One call to inventory; targeted body reads avoid paging 2k-line files |
| `R/paintr-*.R`, callers of a function before editing | Serena `find_referencing_symbols` | Symbol-aware; resists false hits from comments/strings |
| Cross-file string / pattern search across the repo | Bash `grep -rn` (or `Grep` tool) | Faster, returns line numbers, no MCP round-trip |
| `.Rmd`, `.md`, `.yml`, `.json`, `NAMESPACE`, `DESCRIPTION`, fixture `app.R`, `tests/testthat/test-*.R` | `Read` / Bash | No useful symbol tree — Serena is pure overhead and the project hooks may flag the misuse |
| Roxygen prose outside a function body | `Read` the file region | Not a symbol body |
| Runtime / reactive behavior ("why does X happen at flush-time?") | Bash `Rscript` probe scripts; inside shinytest2 children, instrument via `assignInNamespace()` written into the fixture `app.R` | Serena answers *structural* questions, not behavioral ones |

### Order matters for diagnosis

- **Behavioral question** ("why does this Shiny output go blank when the upload binds late?") — write a probe first; use Serena afterward to narrow which function the probe implicates.
- **Structural question** ("where is `runtime_consumer_entry` defined? who calls it?") — reverse: Serena first, probe only if the structure is not enough.

Probing before the first symbolic read wastes context; symbolic reading before the first probe wastes context for a behavioral question. The tool's strength is the inverse of the question type.

### Shinytest2 child-process instrumentation

Parent-process `trace()` / `debugonce()` are **invisible** to the shinytest2 child R process. To instrument code running inside the child, write the instrumentation into the fixture's `app.R` (loaded via `pkgload::load_all()`), using `assignInNamespace("fn_name", wrapped_fn, ns = "ggpaintr")`. The wrapped function can `message()` / `cat()` to surface state into the child's stdout, which `AppDriver` captures.

### Serena edit tools — sub-agent / worktree pitfall

`mcp__serena__replace_symbol_body`, `insert_after_symbol`, `insert_before_symbol`, `rename_symbol`, `safe_delete_symbol` — usable from the orchestrator session when the Serena project root is the worktree you intend to edit. **Do NOT use these from sub-agents spawned into nested worktrees**: Serena's project root is bound once and does not follow the sub-agent's working directory, so edits land in the wrong tree. Sub-agents must use `Edit` / `Write` with absolute paths under their assigned worktree. See project memory `feedback-serena-project-root-doesnt-follow-worktree`.

Symbolic *reads* (`get_symbols_overview`, `find_symbol`, `find_referencing_symbols`) are safe everywhere — only the edit tools have the leak.

### Practice reminders

- Do not re-read an entire file after a symbolic tool has given you what you need.
- Do not alternate Read + Grep on the same R file when one `get_symbols_overview` would have done it — the project hooks will flag this.
- If a Serena call returns what you wanted, commit to it; do not "double-check" with Read unless you have a concrete reason.
- The two `TimeoutError`s on parallel `find_symbol` calls (observed once during the super-1 diagnosis) are minor friction, not a reason to abandon symbolic reads; retry serially.
