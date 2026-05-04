---
status: proposed
created: 2026-05-02
size: small (combined)
depends-on: none
blocks: 03-docs-rewrite
---

# Concern 04 + 05 — Two small clarification fixes

Two unrelated low-blast-radius issues sharing one plan file because either
fits inside one fresh session.

---

## 04 — "Unsaved data inputs" wording

### Problem

The notice rendered when a user has typed into a data-pipeline placeholder but not yet clicked "Update data" reads **"Unsaved data inputs"**. The values aren't *unsaved* — they are typed into Shiny inputs and persist in the session — they are *unapplied*. The current wording suggests data loss is imminent.

### Status quo (verified)

- Defined in `R/paintr-runtime.R:613` (`ptr_stale_notice_ui`).
- Body: `"Click \"Update data\" to apply your changes for %s."` — already correct.
- Header: `tags$strong("Unsaved data inputs")` — the misleading bit.
- Comment above the function (`R/paintr-runtime.R:611-614`) accurately describes the situation: "user typed new data-pipeline values but hasn't clicked Update Data yet".

### Open questions

1. Best replacement wording — candidates: "Pending data changes" / "Unapplied data inputs" / "Data inputs not yet applied" / "Data changes pending — click Update data".
2. Does the surrounding `ui_text` slot machinery (`R/paintr-copy.R`) cover this string, or is it hard-coded? If hard-coded, should it move to `ui_text` so users can override it?
3. Are there other strings in the same area (`Update data` button label, the stale-notice background color choice, etc.) that need the same review while we're here?

### Out of scope

- Restyling the notice div.
- Changing the Update-data flow itself.

### Success criteria

- New wording is accurate and non-alarming.
- If the string was hard-coded and is now `ui_text`-overridable, the change is reflected in `test-copy-rules.R` per `.claude/rules/testing.md`.

---

## 05 — `formula_check` vs `expr_check`

### Problem

ggpaintr has two safety arguments and the access pattern is confusing:

- `formula_check` — parse-time, applied to the entire formula AST in `ptr_parse_formula()`.
- `expr_check` — runtime, applied to each `expr` placeholder's parsed value.

`expr_check` is exposed at the top level (`ptr_app()`, `ptr_app_bslib()`, `ptr_server_state()`). `formula_check` is **not** — to customize it, a user must call `ptr_parse_formula()` themselves, which means dropping below the `ptr_app()` convenience layer. The names also do not telegraph the parse-time-vs-runtime distinction.

### Status quo (verified)

- `formula_check` defined in `R/paintr-parse.R:33, 46`. Default `TRUE`. Accepts logical or list.
- `expr_check` exposed at: `R/paintr-app.R:285` (`ptr_server_state`), `R/paintr-app-bslib.R:21, 45, 101`. Default `TRUE`. Same shape.
- Both flow through to `validate_expr_safety()` (`R/paintr-utils.R:1046`).
- Resolution helper `resolve_expr_check()` lives in `R/paintr-utils.R:977` — handles `TRUE`/`FALSE`/`list(allow_list, deny_list)`.

### Open questions

1. Is the right fix **expose `formula_check` at the top level** (parallel to `expr_check`) so users get symmetric access, with no rename?
2. Or **rename for clarity** — e.g. `parse_safety` / `runtime_expr_safety`, or merge into a single `safety = list(parse = …, expr = …)` argument?
3. If renamed, do we keep aliases for backward compatibility through one minor version, or hard-break?
4. Should the safety story be documented as one concept ("two layers of validation") in a single doc topic instead of two separate `@param` entries?
5. Is there a case for a `safety_preset = "strict" / "permissive" / "off"` shorthand for the common cases?

### Out of scope

- Changes to the denylist contents (per memory: denylist is considered complete).
- Changes to the AST walker logic.
- New safety mechanisms.

### Success criteria

- A user reading `?ptr_app` can answer: "what does this validate, when, and how do I customize each?" without cross-referencing `?ptr_parse_formula`.
- Whatever name is chosen, it's accessible from the top-level entry points without dropping to `ptr_parse_formula()`.

---

## Recommended next-session approach

Either concern can run independently. Invoke `brainstorming` with the relevant section. 04 is essentially a one-line decision; 05 is a small API shape question — start there with question 1 (expose vs. rename).
