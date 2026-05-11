# Docs Rewrite Plan — README & Vignettes

**Owner:** willju · **Created:** 2026-04-19 · **Status:** planned, not started

This file is a self-contained brief. Each task section below (§A, §B, §C, §D)
can be picked up by a fresh Claude Code session with no prior context. Read
**§1 Shared Context** first, then execute exactly one task section per session.

Do **not** attempt multiple task sections in one session — the user will
invoke a fresh session for each rewrite.

---

## 1. Shared Context (read for every task)

### 1.1 Why this rewrite exists (user's complaint, verbatim)

> - The entire override system is complicated, but the motivations are not
>   explained well; examples are not covering all arguments, and potential
>   argument usage is not clear.
> - "Three levels of extensibility" is not right: first level is calling
>   `ptr_app` with a formula; second level is using `ptr_register_*` in a
>   shiny app; third level is using more low-level API for development.
>   There are no examples for third level.
> - Vignette should first explain the motivation — why we do this; next,
>   the technical details.
> - The technical details should come with examples, and potential argument
>   options should be clear.
> - After reading the vignette, the user who wants to customize the UI should
>   know exactly how he can customize the UI. The direction should be clear.
>   Same for customizing placeholders. Users who want to customize
>   placeholders should know exactly what is needed, what is optional, after
>   reading the vignette.

### 1.2 Package 30-second description

`ggpaintr` is an R package that turns a ggplot-like formula string into a
Shiny app. Placeholder tokens in the formula (`var`, `text`, `num`, `expr`,
`upload`) become Shiny input widgets automatically. Users can embed the
generated app into their own Shiny apps, customize labels/help text, and
register new placeholder types.

### 1.3 Correct 3-level extensibility model

Use this framing everywhere in the docs. **Do not** use the old "bind
helpers vs pure helpers" framing alone — it maps onto Level 2/3 but is not
the user-facing structure.

- **Level 1 — Turn-key app.** Call `ptr_app(formula)` or `ptr_app_bslib(formula)`.
  No Shiny code required. Good for quick demos and teaching.
- **Level 2 — Embed in your own Shiny app.** Use `ptr_register_*()` helpers
  (`ptr_register_draw`, `ptr_register_plot`, `ptr_register_code`,
  `ptr_register_error`) plus `ptr_input_ui` / `ptr_output_ui` to drop
  ggpaintr widgets into an existing Shiny UI/server. Customize `ui_text`
  and `placeholders` through public arguments.
- **Level 3 — Low-level API for developers.** Use `ptr_parse_formula`,
  `ptr_runtime_input_spec`, `ptr_exec`, `ptr_extract_plot`,
  `ptr_extract_code`, `ptr_extract_error`, `ptr_assemble_plot`,
  `ptr_gg_extra`, `ptr_setup_controls`, `ptr_server_state`,
  `ptr_build_ids` to build non-app workflows: embed a plot in a custom
  `renderPlot()`, generate code programmatically, write new placeholder
  types, or reuse parsing outside Shiny.

### 1.4 Exported API surface (27 functions, from `NAMESPACE`)

**Level 1 (apps):** `ptr_app`, `ptr_app_bslib`

**Level 2 (Shiny embedding):** `ptr_input_ui`, `ptr_output_ui`,
`ptr_register_draw`, `ptr_register_plot`, `ptr_register_code`,
`ptr_register_error`, `ptr_server`, `ptr_setup_controls`, `ptr_server_state`

**Level 3 (developer API):** `ptr_parse_formula`, `ptr_runtime_input_spec`,
`ptr_exec`, `ptr_extract_plot`, `ptr_extract_code`, `ptr_extract_error`,
`ptr_assemble_plot`, `ptr_gg_extra`, `ptr_build_ids`, `ptr_missing_expr`

**Customization (used at all levels):** `ptr_define_placeholder`,
`ptr_merge_placeholders`, `ptr_merge_ui_text`, `ptr_resolve_ui_text`,
`ptr_normalize_column_names`

> Before writing any example, open `R/` with serena
> (`mcp__serena__find_symbol`) and read the signature + roxygen of every
> function you mention. Do **not** guess argument names.

### 1.5 Override surfaces — fully enumerated

The docs must explicitly enumerate these. Current vignettes under-specify.

**(a) `ui_text` fields** (per-occurrence, from `R/paintr-copy.R:~150`):
- `label` — widget label
- `help` — help text below the widget
- `placeholder` — greyed-out hint inside a text/number input
- `empty_text` — fallback for unset values (used mainly by `var`)

Not every placeholder uses all four. Use `ptr_default_ui_text()` (internal,
accessible via `:::` if needed — prefer calling it through a loaded package)
to see which fields each placeholder actually reads.

**(b) `ui_text` merge precedence** (from
`vignettes/ggpaintr-placeholder-registry.Rmd:327-366`). Three specificity
layers, higher wins:
- `defaults` → keyword applies everywhere
- `params` → keyword applies when used as a specific named argument
- `layers` → keyword applies only inside one layer + argument combination

Plus `shell` (top-level app chrome text: title, draw button label, upload
widget labels) at the top level of the `ui_text` list, outside the
three-layer stack.

**(c) Placeholder registry hooks** (from `paintr-placeholders.R` +
`vignettes/ggpaintr-placeholder-registry.Rmd:182-326`):
- `build_ui(id, copy, meta, context)` — required. Returns the Shiny tag.
- `resolve_expr(value, meta, context)` — required. Returns the R expression
  injected into the generated ggplot call.
- `resolve_input(input, id, meta, context)` — optional. Maps `input$<id>` to
  the raw value.
- `bind_ui(input, output, metas, context)` — optional. Reactive wiring
  (e.g. update a `selectInput`'s choices when the data changes).
- `prepare_eval_env(input, metas, eval_env, context)` — optional. Stuff
  symbols into the eval environment (e.g. upload → data frame binding).

**(d) Placeholder metadata contract** (`meta`) — what the registry stores
per placeholder keyword: `copy_defaults` (list of `label`/`help`/etc.),
plus any custom fields a hook reads. Document required vs optional fields.

### 1.6 Files involved

**Do not touch:**
- `NEWS.md` — append only if you add a user-facing change; this rewrite is
  docs-only, so probably leave alone.
- `dev/developer-notes.md` — human-only.
- R source under `R/`.

**Edit targets (one per task):**
- `README.Rmd` (391 lines today) → regenerate `README.md` via
  `devtools::load_all("."); rmarkdown::render("README.Rmd", envir = globalenv())`
- `vignettes/ggpaintr-workflow.Rmd` (137 lines today)
- `vignettes/ggpaintr-placeholder-registry.Rmd` (424 lines today)
- `vignettes/ggpaintr-extensibility.Rmd` (258 lines today)

### 1.7 Verification steps (run at end of every task)

```r
# 1. Vignettes and examples must render/run
devtools::load_all(".")
devtools::build_vignettes()   # fails fast on broken chunks

# 2. README
rmarkdown::render("README.Rmd", envir = globalenv())

# 3. Full check (required before declaring done)
devtools::check(document = FALSE, manual = FALSE,
                args = c("--as-cran", "--no-manual"))
```

`devtools::check()` must pass with 0 errors, 0 warnings. NOTES about
vignette size are acceptable; new NOTES about undocumented args or broken
examples are not.

### 1.8 Style rules (shared across all tasks)

- Motivation before mechanics. Start every section by answering "why does
  this exist?" in 1–2 sentences.
- Every technical claim has a runnable example. Examples must be
  self-contained (load `penguins` from `palmerpenguins` or use `mtcars`).
- Enumerate every argument option as a bullet list or table. No
  "arguments include …" hand-waving.
- Prefer tables for override-field catalogs.
- Tidyverse style: snake_case, 2-space indent, `|>` over `%>%` in new code
  (match existing file style — older examples use `%>%`).
- No emoji unless already present.
- Length target: vignettes 400–900 lines; README 250–400 lines.

### 1.9 Things currently missing that must appear

- Level-3 examples. Today there are none. Concrete ones to write:
  1. Parse a formula once, call `ptr_exec()` headlessly to produce a plot
     without Shiny (for batch reports).
  2. Use `ptr_extract_plot()` inside a user-written `renderPlot()` to
     post-process the plot object before rendering.
  3. Use `ptr_gg_extra()` to add layers to the embedded plot from the host
     app.
  4. Write a custom placeholder end-to-end (date picker or slider) with
     all five hooks explained.
- A single table listing the four `ui_text` fields and which placeholders
  use each.
- A single table of the five placeholder hooks with signature, required
  vs optional, and one-line purpose.
- A worked example of `ui_text` merge precedence showing the same
  placeholder receiving different copy under `defaults` / `params` /
  `layers`.

---

## §A. README rewrite

**Fresh session prompt:** "Read `dev/tasks/docs-rewrite-plan.md` §1 and §A.
Rewrite `README.Rmd` following the spec. Re-knit to `README.md`. Do not
touch vignettes."

**File:** `README.Rmd` · **Target length:** 250–400 lines (down from 391,
but re-aim at content quality rather than a line target)

**Goals:**
1. Give a 30-second pitch: what ggpaintr is, what problem it solves.
2. Install + smallest working example in the first screen.
3. Show the 3-level extensibility model as the organizing principle.
4. Point to the three vignettes explicitly — one per level where it fits
   best.
5. Remove the current "Feature tour" mega-section (`README.Rmd:108-350`) —
   move Level-2/3 demos into vignettes, keep one small teaser per level
   in the README.

**Required sections (in order):**
1. Title + 2-line pitch + badge block.
2. Installation.
3. Quick start — smallest `ptr_app()` call that renders. One code block,
   one screenshot (keep existing if present).
4. What the formula syntax means — brief table of the 5 placeholder
   keywords (`var`, `text`, `num`, `expr`, `upload`) with one-line
   descriptions.
5. Three levels of use — one paragraph per level, each linking to the
   appropriate vignette:
   - L1 → (covered in this README + workflow vignette)
   - L2 → `vignette("ggpaintr-extensibility")`
   - L3 → `vignette("ggpaintr-extensibility")` (new Level-3 section)
6. Where to go next — vignette list with 1-line descriptions.
7. Stability guarantees (keep current `README.Rmd:351-385` content;
   prune).

**Remove:**
- "Supported public API" function list (`README.Rmd:86-107`) — this is
  what pkgdown reference is for.
- Most of "Feature tour" (`README.Rmd:108-350`) — move to vignettes.

**Keep:**
- Core concepts block.
- "Current behavior boundary" (move to bottom).

**Acceptance criteria:**
- Knitting `README.Rmd` produces `README.md` with no errors.
- `devtools::check()` clean.
- A new user can copy-paste one block from the README and see a Shiny
  app.
- The README links to all three vignettes.

---

## §B. Workflow vignette rewrite

**Fresh session prompt:** "Read `dev/tasks/docs-rewrite-plan.md` §1 and §B.
Rewrite `vignettes/ggpaintr-workflow.Rmd` following the spec. Do not
touch README or the other vignettes."

**File:** `vignettes/ggpaintr-workflow.Rmd` · **Target length:** 400–600 lines
(up from 137 — this is the foundational vignette).

**Scope:** Level 1 + "what happens under the hood" at a high level. This is
the first vignette a new user reads after the README.

**Required sections (in order):**

1. **Motivation.** Why take a formula string, parse it, and spin up a
   Shiny app? What pain does this remove vs. hand-writing a Shiny app?
   Mention teaching, exploration, prototyping.

2. **The formula syntax.** Full spec of the DSL:
   - How ggplot calls map to layers.
   - The 5 placeholder keywords — one subsection each with:
     - What it means.
     - What widget it becomes.
     - What value it produces at runtime (string? numeric? expr?).
     - One minimal example formula using it.

3. **Level 1: launch an app.** `ptr_app()` and `ptr_app_bslib()`. Show
   both. List every argument with defaults. Explain when to use which.

4. **Data sources.** Three paths:
   - Pass `data =` directly.
   - Use the `upload` placeholder.
   - Non-syntactic column names → `ptr_normalize_column_names()` (explain
     when normalization is automatic vs manual).

5. **What happens when you click "Update plot".** Walk through the
   runtime: `ptr_parse_formula()` → `ptr_runtime_input_spec()` →
   `ptr_exec()` at a conceptual level with a small diagram or bullet
   chain. Do **not** dive into hook internals — that belongs in the
   placeholder-registry vignette.

6. **Level 2 and 3 teasers.** Two short paragraphs each pointing to the
   other vignettes with the exact reader outcome promised:
   - L2: "After reading the extensibility vignette you will know how to
     embed ggpaintr controls in your own Shiny app and override any
     label or help text."
   - L3: "After reading the placeholder-registry vignette you will know
     how to add a new placeholder type end-to-end."

7. **Current behavior boundary.** Move from existing vignette.

**Acceptance criteria:**
- Every placeholder keyword has a worked example.
- Every argument of `ptr_app()` is listed with default and one-line
  meaning.
- A reader who finishes this vignette can launch an app from any
  formula against their own data.

---

## §C. Placeholder-registry vignette rewrite

**Fresh session prompt:** "Read `dev/tasks/docs-rewrite-plan.md` §1 and §C.
Rewrite `vignettes/ggpaintr-placeholder-registry.Rmd` following the spec.
Do not touch README or the other vignettes."

**File:** `vignettes/ggpaintr-placeholder-registry.Rmd` · **Target length:**
600–900 lines (up from 424).

**Scope:** The complete guide to adding or modifying placeholder types.
After reading this, a user who wants a custom placeholder must know
**exactly** what is needed and what is optional — that is the acceptance
bar the user stated.

**Required sections (in order):**

1. **Motivation.** Why a registry at all? Why not hard-code placeholder
   types? Cover: (a) letting users register new input widget types like a
   date picker or slider, (b) letting users replace the default widget
   for a built-in keyword, (c) letting packages extend ggpaintr without
   forking it.

2. **Built-in placeholders.** Table: keyword · widget · value type ·
   which hooks it defines. This sets the expectation for what a custom
   one looks like.

3. **The placeholder contract — required vs optional.**
   A **single table** that a reader can print and stick on the wall:

   | Hook | Signature | Required? | Purpose |
   |------|-----------|-----------|---------|
   | `build_ui` | `(id, copy, meta, context)` | **required** | Return a Shiny tag |
   | `resolve_expr` | `(value, meta, context)` | **required** | R expression injected into the ggplot call |
   | `resolve_input` | `(input, id, meta, context)` | optional | Map `input$<id>` → raw value |
   | `bind_ui` | `(input, output, metas, context)` | optional | Reactive wiring |
   | `prepare_eval_env` | `(input, metas, eval_env, context)` | optional | Seed the eval environment |

   Plus `meta` fields: required (`copy_defaults`?) vs extensible. Verify
   each row against current `paintr-placeholders.R` before publishing.

4. **A minimal custom placeholder.** End-to-end: date picker. Show the
   full `ptr_define_placeholder()` call, every hook, and a full Shiny app
   using it.

5. **A more complex example.** One that uses `bind_ui` (reactive updates)
   and `prepare_eval_env` (inject a value into the eval env). Slider with
   range dependent on data, or a column-subset chooser.

6. **Replacing a built-in placeholder.** Show overriding `var` to use
   `shinyWidgets::pickerInput` instead of `selectInput`.

7. **Plugging custom placeholders into every API.**
   - `ptr_app(formula, placeholders = ...)`.
   - `ptr_app_bslib(formula, placeholders = ...)`.
   - `ptr_parse_formula(formula, placeholders = ...)`.
   - Manual embedding via `ptr_input_ui` / `ptr_server`.

8. **Copy for custom placeholders.** Pointer to the copy-rules section
   of the extensibility vignette, with one small example showing
   `copy_defaults` inside a placeholder definition vs `ptr_merge_ui_text`
   overrides.

**Remove from existing vignette:**
- Any section that belongs in the workflow vignette (formula basics).
- The `ui_text` override section — **move it to the extensibility
  vignette** and leave a pointer here.

**Acceptance criteria:**
- A reader who wants to add a date-picker placeholder can copy the
  minimal example and adapt it without reading R source.
- Every hook is documented with signature, purpose, and required/optional
  marker.
- Every code example runs.

---

## §D. Extensibility vignette rewrite

**Fresh session prompt:** "Read `dev/tasks/docs-rewrite-plan.md` §1 and §D.
Rewrite `vignettes/ggpaintr-extensibility.Rmd` following the spec. Do not
touch README or the other vignettes."

**File:** `vignettes/ggpaintr-extensibility.Rmd` · **Target length:**
600–900 lines (up from 258).

**Scope:** Level 2 (embed in Shiny) and Level 3 (developer API). The user
complained this vignette's mental model is wrong and Level 3 has no
examples.

**Required sections (in order):**

1. **Motivation.** Why embed ggpaintr instead of using `ptr_app()`? Why
   expose a low-level API at all? Mention: existing Shiny apps,
   non-Shiny batch use, custom plot post-processing, programmatic code
   generation.

2. **Extensibility model.** One paragraph + a diagram (ASCII is fine)
   showing the three levels. Explicitly map each `ptr_*` export to a
   level — use the list from §1.4.

3. **Level 2: embed ggpaintr in your own Shiny app.**
   Sub-sections:
   - 3a. **Minimal embed.** UI with `ptr_input_ui` + `ptr_output_ui`;
     server with `ptr_register_draw`. Full runnable app.
   - 3b. **Splitting plot, code, and error panels.** `ptr_register_plot`,
     `ptr_register_code`, `ptr_register_error` — one runnable example
     using all three.
   - 3c. **Custom ids.** Full `ptr_build_ids()` signature, every field,
     when to override.
   - 3d. **Overriding UI copy.** The `ui_text` system end-to-end:
     - Table of the four fields (`label`, `help`, `placeholder`,
       `empty_text`) and which placeholder types use each (verify via
       `ptr_default_ui_text()` before writing).
     - Merge precedence: `shell` vs `defaults` vs `params` vs `layers`.
     - A worked example: one `date` placeholder receiving different
       copy under each of `defaults` / `params` / `layers`, with the
       final resolved copy shown via `ptr_resolve_ui_text()`.
     - How to validate a `ui_text` list before use
       (`ptr_validate_ui_text` if exported, else the resolve call).
   - 3e. **Placeholder overrides.** Pointer to the placeholder-registry
     vignette with a one-paragraph teaser.

4. **Level 3: the developer API.**
   Four recipes, each runnable:
   - 4a. **Headless plot from a formula.** `ptr_parse_formula()` →
     `ptr_runtime_input_spec()` → build an input list → `ptr_exec()` →
     `ptr_extract_plot()`. Output a png file, no Shiny. Useful for
     batch reports.
   - 4b. **Post-process the plot inside a custom `renderPlot()`.**
     Use `ptr_extract_plot()` to grab the `ggplot` object, add a
     layer, then render.
   - 4c. **Add layers from the host app with `ptr_gg_extra()`.**
     Full example: host app supplies extra `geom_smooth()` based on a
     checkbox, ggpaintr merges it into the embedded plot.
   - 4d. **Generate code programmatically.** `ptr_extract_code()` to
     get the generated ggplot call as a string, for copy-paste or for
     piping into a code-display widget outside ggpaintr.

5. **Reference card.** Small table: "I want to … → use …" mapping
   10–15 common tasks to API calls.

**Remove:**
- Old "Pure helpers vs bind helpers" framing as the primary
  organization (can appear as a footnote under Level 2 if helpful).
- "Roadmap" section (`ggpaintr-extensibility.Rmd:246`) unless kept
  brief at the bottom.

**Acceptance criteria:**
- Each of the four Level-3 recipes is fully runnable.
- The `ui_text` override table lists every field and every merge layer.
- A reader who wants to customize the UI labels for one specific
  argument in one specific layer knows exactly which list key to
  touch.
- A reader who wants to embed a ggpaintr plot inside their own
  `renderPlot()` has a copy-pasteable starting point.

---

## 2. Global acceptance checklist

Before declaring any task done, confirm:

- [ ] `devtools::build_vignettes()` succeeds.
- [ ] `devtools::check(document = FALSE, manual = FALSE, args = c("--as-cran", "--no-manual"))` → 0 errors, 0 warnings.
- [ ] `rmarkdown::render("README.Rmd")` succeeds and `README.md` is up to date (only required for §A).
- [ ] Every placeholder hook / override field / exported function
      mentioned is verified against current R source.
- [ ] Every code chunk is runnable as written; `eval = FALSE` is used
      only for app-launching chunks that block.
- [ ] Cross-references between vignettes use `vignette("name")` syntax,
      not raw URLs.
