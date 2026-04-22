---
name: serena-tools
description: Prefer Serena MCP symbolic tools when available for code exploration
scope: project
severity: guidance
inject_into: [assessor, browser-tester, coder, compliance, design-facilitator, planner, reviewer, scanner, tester]
verify: Agent output shows use of mcp__serena__* tools (get_symbols_overview, find_symbol, find_referencing_symbols) for R source exploration instead of Read/Grep on R/paintr-*.R files, except when reading .Rmd/.md/.yml or non-symbol-shaped patterns.
---

## Prefer Serena for R Source Exploration

When exploring the ggpaintr R source, prefer Serena MCP symbolic tools over flat file reads or substring searches. Symbolic tools cut tool-call count, stay on the symbol you care about, and preserve context budget for the actual task.

### Default to Serena

- `mcp__serena__get_symbols_overview` — one call to list every top-level function in a file. Use this as the first step when you open an unfamiliar `R/paintr-*.R` file, instead of reading the whole file.
- `mcp__serena__find_symbol` — fetch a function's body or signature by name. Use `name_path_pattern` (not `name_path`); `relative_path` must point at a file, not a directory.
- `mcp__serena__find_referencing_symbols` — find callers of a function before changing it.
- `mcp__serena__replace_symbol_body`, `mcp__serena__insert_after_symbol`, `mcp__serena__insert_before_symbol`, `mcp__serena__rename_symbol`, `mcp__serena__safe_delete_symbol` — edit code at symbol granularity when you already know the target.

### When Read / Grep are still correct

- `.Rmd`, `.md`, `.yml`, `.json`, `NAMESPACE`, `DESCRIPTION` — not R symbol trees, so Serena does not help. Use `Read`.
- Patterns that aren't symbol-shaped (string literals, multi-file text search, cross-language text) — use `Grep`.
- Roxygen comment text that lives outside a function body — `Read` the file region.

### Practice

- Do not re-read an entire file after a symbolic tool has given you what you need.
- Do not alternate Read + Grep calls on the same R file when one `get_symbols_overview` would have been enough — the project hooks will flag this.
- If a Serena call returns what you wanted, commit to it; do not "double-check" with Read unless you have a concrete reason.
