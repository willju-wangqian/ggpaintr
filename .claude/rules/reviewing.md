## ggpaintr Review Dimensions

Multi-pass focused review. Each pass targets one risk dimension.
Narrow prompts defeat sycophancy/attention bias — bounded verification > open-ended "review everything".

### Pass order (by cascade risk)

1. **Formula parsing & metaprogramming safety**
   - Injection via `expr` placeholder, malformed formulas, `rlang::parse_expr` edge cases, untrusted input reaching `eval()`
   - Files: `R/paintr-parse.R`, `R/paintr-runtime.R`
   - **Denylist is considered complete (~151 entries).** Do not suggest adding more entries unless you find a concrete, exploitable bypass that existing entries + the AST walker cannot catch. R's openness makes exhaustive enumeration impossible; the walker's recursive descent into all AST nodes (including lambda bodies, pairlists, string literals) is the primary safety mechanism, not the denylist alone. Focus reviews on walker logic, check ordering, and eval-path gaps instead.

2. **Shiny reactive correctness**
   - Reactive dependency leaks, missing `req()` guards, circular observers, stale `input$` after dynamic UI rebuild
   - Files: `R/paintr-runtime.R`, `R/paintr-app.R`, `R/paintr-ui.R`

3. **Placeholder registry contract**
   - Missing/mistyped hook names, `meta` field assumptions, registry merge order, custom placeholder breaking built-ins
   - Files: `R/paintr-placeholders.R`

4. **Copy/UI text correctness** (parallel with 5)
   - Merge precedence bugs, alias normalization misses (`colour`->`color`), `__unnamed__` positional args, stale defaults
   - Files: `R/paintr-copy.R`

5. **Data handling & upload** (parallel with 4)
   - Column name normalization edge cases, upload format validation, data type coercion
   - Files: `R/paintr-data.R`, `R/paintr-upload.R`

6. **API surface & package conventions** (final sweep)
   - Undocumented params, inconsistent return types, missing `@export`, NAMESPACE drift, `R CMD check` NOTEs
   - Files: all `R/paintr-*.R`, `NAMESPACE`

### How to use

- Run passes sequentially (1-3) then parallel (4+5) then final (6).
- Prompt each pass: "Review ONLY [dimension]. Ignore other concerns. Find issues or explicitly state none found with reasoning."
- Never trust a single-pass "all clear" — the first pass is biased toward approval.

### Deriving passes for new projects

1. List transformation boundaries (where data changes shape).
2. List extension points (where users inject behavior).
3. List trust boundaries (where untrusted input enters).
4. Cross-check against dependency graph — uncovered modules need their own pass.
5. Order by cascade risk — foundational layers first, cross-cutting last.
