# {package name TBD} — a placeholder-binding engine for R expressions

A host-agnostic library that turns a list of R expressions containing placeholder tokens into UI-driven, substituted, **evaluatable** expressions. It extracts ggpaintr's core idea (placeholder → widget → substitute → eval) and drops ggpaintr's weld to ggplot: the engine stops at producing an evaluatable expression and never owns eval-output, rendering, staging, or an app.

## Language

**Expression**:
A single R language object that may contain Placeholder tokens.
_Avoid_: formula, statement

**Expression list** (a.k.a. **Block**):
The ordered set of Expressions the engine binds at once — supplied as a `{ }` block or a list of expressions.
_Avoid_: workflow, pipeline, script

**Typed expression tree**:
An Expression parsed into its AST with every Placeholder hole identified and tagged by its kind, **and** the structural relationships between entities resolved — including, for each masked Placeholder, its governing Data mask. The "type" is the per-hole placeholder kind plus the resolved relationships — **not** a domain structure like ggpaintr's root/layer tree.
_Avoid_: formula tree, ggplot tree

**Data mask**:
The data context a masked Placeholder draws its choices from, found by ascending the Typed expression tree from the Placeholder to the **nearest enclosing call whose head's `formals()` contain a common-data name** (`data`, `.data`, …), then reading that call's data argument. Generalizes ggpaintr's source→consumer upstream resolution. The tree supplies *which* frame; the caller's env supplies the actual frame.
_Avoid_: data source, upstream, context

## Data-mask resolution (locked)

For each masked Placeholder `P`:
1. `tree_capture` **canonicalizes pipes** (`a |> f(...)` *and* `a %>% f(...)` → `f(a, ...)`) so data is always an argument, not a pipe sibling (ADR 0012 parallel; native `|>` self-rewrites, `%>%` does not).
2. **Ascend `P`'s entire call-ancestry** (not just the immediate parent). At each ancestor call ask: does its head's `formals()` contain a common-data name **AND does the call supply a value to it** (`match.call`)? Skip every call that fails this (no data formal — `factor`, `~`, `aes`, arithmetic — *or* a data formal left unsupplied). Stop at the first that qualifies.
3. The qualifying call's supplied data value is the **mask**.
4. **Eval that value in `env`** → `names()` → choices.
5. Reach the root with no qualifying ancestor / eval fails → fallback to explicit `ppVar(mpg, choice = …)`.

Two correctness bonuses fall out of the "supplied" check:
- `lm(factor(ppVar()))` → `factor` skipped; `lm` has a `data` formal but it's **unsupplied** → no qualifying ancestor → **fallback**.
- **Layer-data inheritance**: `ggplot(iris) + geom_point(aes(x = ppVar()))` → skip `aes`, skip `geom_point` (data formal unsupplied), stop at `ggplot` (data = `iris`) — mirroring ggplot's own runtime inheritance. With `geom_point(data = d2, …)` the ascent stops at `geom_point` (the layer override) — ggpaintr's source→consumer override.

**Scope boundary (refined — R1):** the engine evals **data masks and `choice=` expressions** (to build the UI); it never evals the **output expression** (the caller's separate `eval()`). ggpaintr already lives at this line.

**R3 — Placeholder args are engine metadata.** `ppVar`'s `default`/`choice=` are consumed during resolution/substitution and **stripped** from the output (`ppVar(mpg, choice=…)` → `mpg`), as ggpaintr's `ppVar(default)` is today.

**Placeholder**:
A `pp*` token inside an Expression marking a hole to be filled by a UI value. Each Placeholder has a kind (e.g. ppVar, ppText, ppNum, ppExpr, ppUpload) that selects its widget.
_Avoid_: input, variable

**Substitution**:
Replacing each Placeholder in a *copy* of the Typed expression tree with a value, yielding an evaluatable Expression. Original trees are never mutated.

**Outcome** (boundary term):
The value produced by `eval`-ing a substituted Expression. Eval is the **edge** of the engine's scope — the engine may offer it as a convenience but goes no further (no rendering, no staging, no app). Any downstream use of an Outcome is the caller's concern.

## Relationships

- An **Expression list** contains one or more **Expressions**.
- Each **Expression** parses into one **Typed expression tree**.
- A **Typed expression tree** contains zero or more **Placeholders**.
- Each **Placeholder** produces one UI widget; the widget value drives **Substitution** of a copy of the tree.
- **Substitution** yields an evaluatable **Expression**; `eval` of it is the **Outcome** — the engine's scope boundary.
- A masked **Placeholder** resolves its choices from its **Data mask**, found structurally in the **Typed expression tree** (not from any external context arg).

## Pipeline (redesigned — tidyverse style, every stage returns a typed tree)

Four artifacts flow through the pipe:

| # | artifact | what it is | values? |
|---|---|---|---|
| 1 | **expr list** | input `{ }` block / list of expressions | — |
| 2 | **pp_tree** (blueprint) | typed trees; placeholders identified + kind-tagged + Data masks resolved + choices computed | no |
| 3 | **valued pp_tree** | same tree, each Placeholder node carries a chosen value | yes |
| 4 | **expr** | values spliced in; runnable, no placeholders left | n/a |

Package: **exprForest**. Verbs use the **`tree_`** prefix, computational/rlang vocabulary. Classes: **`.expr_tree`** (one tree), **`.forest`** (a list of them).

```r
exprs
  |> tree_capture()      # 1→2  enexpr (native |> is a parse-time rewrite so {block} |> tree_capture() works)
                         #      + classify + resolve Data masks + compute choices + canonicalize pipes. Always returns a .forest.
  |> tree_prompt()       # 2→3  Shiny GADGET (runGadget/stopApp): attach user-picked values. No substitution.
  #  OR
  |> tree_bind(values)   # 2→3  STATIC: attach programmatic values (same output as tree_prompt).
  |> tree_inject()       # 3→4  inject values into holes → runnable expr (rlang !! semantics). (NOT substitute() — would mask base.)
  |> tree_eval()         # 4→outcome   thin wrapper over base::eval(env = parent.frame()); the CALLER's step.
```

### Full function set

| role | function | in → out |
|---|---|---|
| build | `tree_capture(exprs)` | expr list → `.forest` |
| attach · interactive | `tree_prompt(forest)` | `.forest` → valued `.forest` (opens gadget) |
| attach · static | `tree_bind(forest, values)` | `.forest` → valued `.forest` (pure) |
| attach · reactive (module) | `tree_ui(id)` / `tree_server(id, forest)` | embed in caller's app → reactive valued `.forest` |
| substitute | `tree_inject(forest)` | valued `.forest` → runnable expr(s) |
| eval | `tree_eval(expr, env = parent.frame())` | expr → outcome |

Accessors: `tree_nodes()` (placeholders: ids/kinds/defaults), `tree_masks()` (resolved frame per masked `ppVar`), `tree_choices()` (widget options), `tree_values()` (attached `id→value` set, for replay), `tree_walk(forest, f)` (low-level node visitor).

Renames from the first sketch: `build_tree`/`build_trees` → one **`tree_capture()`** (always a `.forest`, no plural). `interactive_substitute` → split into **`tree_prompt()`** (interactive attach) + **`tree_inject()`** (substitute). **`tree_bind()`** = static counterpart of `tree_prompt`. The interactive name (`tree_prompt`) self-signals the blocking UI side effect; the pure path (`tree_bind`) is clearly the no-UI one.

### Placeholder id scheme (locked)
**Structural path** — the placeholder's position in the tree. Unique, stable across builds as long as the expression is unchanged (so replay survives), zero user effort. `kind_default` is the widget *label*, never the key. Optional `ppVar(x, id = "…")` override for a human-legible value-set.

### What the user can grab at each stage
- after `tree_capture()` — the blueprint: `tree_nodes()`, `tree_masks()`, `tree_choices()` (inspect before any UI).
- after `tree_prompt()`/`tree_bind()` — `tree_values()` → named `id→value`, **extractable + replayable**.
- after `tree_inject()` — the runnable expr: `deparse()`/`dput()` = reproducible-code artifact.
- after `tree_eval()` — the Outcome (caller's).

### Replay property (the point of splitting attach from inject)
```r
f <- tree_capture({ ggplot(mtcars, aes(x = ppVar())) + geom_point() })
v <- f |> tree_prompt() |> tree_values()           # pick once, in the UI
f |> tree_bind(v) |> tree_inject() |> tree_eval()  # later: headless / batch / test
```

### Three attach modes, one inject
Values attach **interactively (`tree_prompt`, gadget)**, **reactively (`tree_ui`/`tree_server`, embedded in the caller's own app)**, or **statically (`tree_bind`)** — all produce a valued `.forest` consumed by one `tree_inject()`.

### Placeholder kind set & registry (Q7 — locked)
Placeholder kinds (`ppVar`, `ppText`, `ppNum`, `ppExpr`, `ppUpload`, …) are **extensible**, each defining a triple: **widget · choice-derivation · injection**. The registry is **process-global so custom kinds register once and are seen by every `{}`** — NOT forest-scoped (that would force re-injecting custom kinds into every `tree_capture()` call). Built-ins = the global default's contents; `tree_register_kind("ppDate", widget=, choices=, inject=)` mutates the global.

To avoid ggpaintr's untestable-global pain (registry env split across package/namespace under `load_all` → custom placeholders invisible; memories `probe-devtools-vs-pkgload-registry`, `test_file ≠ devtools::test`), the registry is a **first-class passable object**: `tree_capture(exprs, registry = tree_registry())` defaults to the global, but a fresh registry (or `withr::local_*`) can be passed for test isolation.

### Multi-step = composed pipelines, not engine machinery
`{ result1 <- … } |> tree_capture() |> tree_prompt() |> tree_inject() |> tree_eval(); foo(result1, …)` works because `tree_eval()` lands `result1` in the caller's frame. The engine stays one pipeline; "result feeds next expression" is ordinary sequential R in the caller's code.

## Out of scope (deliberate non-goals)

- Building a Shiny **app** — the engine provides Shiny *widgets / module pieces*; the caller owns the app.
- **Rendering / presenting** an Outcome (no `renderPlot`/`renderUI` ownership — the weld ggpaintr has and this engine rejects).
- **Staging** across interaction boundaries (`event_data`, async re-eval) — the caller arranges and evals substituted Expressions however they need.

## Example dialogue

> **Dev:** "If line 2 uses a variable assigned on line 1, does the engine wire that dependency?"
> **Designer:** "No — that's ordinary R. The engine only fills Placeholders and hands back the substituted Expressions; sequential eval in one env is the caller's job."

## Flagged ambiguities

- **RESOLVED — substitution ≠ evaluation.** `interactive_substitute`/`bind_server` hand back the *substituted Expression*; `eval` is always the caller's separate step. Engine never evals.
- **RESOLVED — "Typed expression tree"** = per-hole placeholder kind + resolved entity relationships (incl. Data mask), not ggpaintr's root/layer typing.
- **RESOLVED — placeholder choices** come from the Data mask resolved structurally in the tree, not an external context arg.
- **OPEN (Q5) — masking-convention knowledge.** How the resolver knows which arg of an arbitrary function is the data vs masked: structural heuristic only / per-function registry / heuristic + registry override — and whether that knowledge lives in the core or a pluggable resolution layer (ggpaintr's ggplot rules = one ruleset). Leaning: heuristic + override, in a pluggable layer outside the core.
