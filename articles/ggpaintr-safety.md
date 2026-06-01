# Safety

This vignette answers one question: *how do I think about sharing a
ggpaintr app with untrusted users?* For building apps, see
[`vignette("ggpaintr-tutorial")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-tutorial.md);
for driving ggpaintr from an LLM,
[`vignette("ggpaintr-llm")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-llm.md).

ggpaintr assumes *the formula author is the package user* — you — while
the *placeholder values*, including anything typed into an `ppExpr` box,
may come from anywhere. Three boundaries sit between those two trust
levels:

1.  **`expr_check`** — controls validation of user-typed `ppExpr` code.
2.  **The denylist + AST walker** — what that validation actually does.
3.  **The upload trust model** — what happens between “user picks a
    file” and a data frame reaching
    [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

The rest of this vignette takes each in turn. One scope note up front:
ggpaintr has not been hardened for adversarial public deployment, and
carries no tests for hosting, network, or process-isolation concerns.
Those belong to the host environment (shinyapps.io, Posit Connect roles,
container limits), not to ggpaintr’s surface.

## The `expr_check` flag

`ppExpr` is the only keyword that accepts arbitrary R code. The other
four — `ppVar`, `ppText`, `ppNum`, `ppUpload` — take atomic values or
files, and nothing runs what the user typed as R. `expr_check` decides
what happens to whatever lands in an `ppExpr` box before it reaches the
evaluator.

``` r

ptr_app(formula, expr_check = TRUE)               # default — full denylist + walker
ptr_app(formula, expr_check = FALSE)              # off — anything goes
ptr_app(formula, expr_check = list(               # custom
  allow_list = c("read.csv"),
  deny_list  = c("get")
))
```

`expr_check` is accepted on every public entry point that builds the AST
—
[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md),
[`ptr_ui()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui.md),
[`ptr_ui_controls()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_controls.md),
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)
(forwarded to
[`ptr_init_state()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_init_state.md)
via `...`), and
[`ptr_init_state()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_init_state.md)
— and defaults to `TRUE` everywhere.

### The three modes

| `expr_check =` | Mode | What gets checked |
|----|----|----|
| `TRUE` (or [`list()`](https://rdrr.io/r/base/list.html) with no lists) | denylist | The curated 151-entry denylist (below). Any call to, symbol naming, or string literal mentioning a denied name aborts. |
| `FALSE` | off | No validation. Whatever the user typed reaches the evaluator. |
| `list(deny_list = …)` | denylist | **Replaces** the curated 151 with *only* the names you list. On its own this *weakens* the default — there is no “add to the curated list” form. |
| `list(allow_list = …)` | allowlist | Only listed names may appear as call heads. The curated always-dangerous names (e.g. `system`) stay blocked as bare symbols or strings regardless. With both lists, the effective allow set is `allow_list` minus `deny_list`. |

The asymmetry is the thing to internalise: a bare `deny_list` *narrows*
protection (it discards the other 150 curated names), while `allow_list`
*keeps* the curated floor and only widens which call heads are
permitted. To permit one extra name without losing the curated
protection, use `allow_list`, not `deny_list`.

### When (never) to turn it off

`expr_check = FALSE` is for local prototyping with trusted input — you,
at the R prompt, on a private dataset. It is never right for any app
another person can reach; co-workers on a shared dev server count.

If a name you trust is blocked (e.g. `read.csv`, because the denylist is
conservative), the fix is `expr_check = list(allow_list = "read.csv")`,
which keeps the walker active for everything else — not
`expr_check = FALSE`.

## The denylist + AST walker

The denylist is *which* names the validator treats as dangerous. The
walker is *how* it hunts for them. Each does only half the job.

### The denylist (151 entries, treated as complete)

`unsafe_expr_denylist` (in `R/paintr-utils.R`) is the curated set of R
names categorically unsafe from untrusted input:

| Category | Representative entries |
|----|----|
| system escape | `system`, `system2`, `shell`, `shell.exec`, `pipe` |
| file I/O | `readLines`, `writeLines`, `readRDS`, `read.csv`, `download.file`, `unlink`, `file` |
| deserialization / workspace I/O | `serialize`, `unserialize`, `load`, `save` |
| meta-eval (denylist-bypass vectors) | `eval`, `parse`, `quote`, `bquote`, `do.call`, `match.fun`, `get`, `mget`, `str2lang`, `str2expression` |
| environment / global-state mutation | `<<-`, `->>`, `assign`, `attach`, `library`, `loadNamespace`, `options` |
| dangerous base / native code | `on.exit`, `q`, `.Internal`, `.Call`, `.External`, `dyn.load` |
| debugger / introspection | `debug`, `browser`, `parent.frame`, `environment`, `new.env` |
| info disclosure | `Sys.getenv`, `getwd`, `list.files`, `Sys.glob`, `R.home` |
| meta-dispatch / method injection | `exec`, `getExportedValue`, `delayedAssign`, `setClass`, `setMethod`, `unlockBinding` |
| delayed / deferred execution | `reg.finalizer`, `addTaskCallback`, `setHook`, `packageEvent` |

The list is **treated as complete** for the maintained branch. R is open
enough that no finite enumeration is exhaustive — `get("system")()`
would slip past any pure name-match if the walker did not also descend
into string literals, compound heads, and nested calls. New entries land
only when a concrete bypass is demonstrated against the *walker*, not
against the list alone.

There is no “add one name” shortcut. As the table notes,
`expr_check = list(deny_list = "your_name")` *replaces* the curated 151
with only `"your_name"`, silently dropping the other 150 protections. If
you think the curated list genuinely omits a dangerous name, treat it as
a walker/list gap and report it (see below) rather than narrowing your
own deployment to a one-entry denylist.

### The AST walker

`validate_expr_safety()` (also in `paintr-utils.R`) is the recursive
descent that turns the denylist from a list of names into a check with
teeth. Every `ppExpr` input passes through it before the formula is
evaluated. At each AST node:

- **Bare symbol** → checked by name (catches `system` typed alone).
- **String literal** (length 1 *or* longer) → every element checked
  (catches `do.call("system", …)` and `c("safe", "system")` alike).
- **Pairlist** (e.g. the formals of an inline `function(x)`) → recurse
  into every element.
- **Call** → recurse into the head *and* every argument.
  - **Compound head** (`(system)("ls")`, `base::eval(parsed)`) → recurse
    into the head sub-expression, then check its deparsed name.
  - **Placeholder annotation** (`ppNum(shared = "x")`) → skipped: that
    is formula DSL, validated by the parser at translation time, not
    user code.
- **Depth cap** → 100 nested levels; deeper aborts with “simplify it.” A
  guard against pathological inputs.

Importantly, in **allowlist** mode the floor still holds: a curated name
appearing as a bare symbol or string is blocked even if it is absent
from your `allow_list` — the allowlist governs which *call heads* are
permitted, not whether `system` may appear as data.

The walker is what makes the denylist usable. A name-only check would
miss every form that builds a function reference from a string —
`do.call("system", list("ls"))`, `eval(parse(text = "system('ls')"))`,
`getFromNamespace("system", "base")`. The walker recurses through all of
them, hitting denied names wherever they sit.

If you find a string that slips past both the denylist and the walker,
file a bug — the walker is the primary safety mechanism and where
bypasses get fixed first.

## The upload trust model

`ppUpload` is the second untrusted-input vector. Unlike `ppExpr`, an
upload does not execute user code — it parses user bytes as tabular
data.

### Accepted formats

| Extension | Reader | Suggested-dep package |
|----|----|----|
| `.csv` | [`utils::read.csv()`](https://rdrr.io/r/utils/read.table.html) (`UTF-8-BOM`) | base |
| `.tsv` | [`utils::read.delim()`](https://rdrr.io/r/utils/read.table.html) (`UTF-8-BOM`) | base |
| `.rds` | [`base::readRDS()`](https://rdrr.io/r/base/readRDS.html) | base |
| `.xlsx`, `.xls` | [`readxl::read_excel()`](https://readxl.tidyverse.org/reference/read_excel.html) | `readxl` |
| `.json` | `jsonlite::fromJSON(flatten = TRUE)` | `jsonlite` |

Any other extension is rejected — “Please upload a .csv, .tsv, .rds,
.xlsx, .xls, or .json file.” — with no fallback parser. A JSON upload
must be an array of records; nested fields are flattened, and any column
that remains a nested array or object is rejected.

`.rds` is the most permissive in what bytes it accepts — an `.rds` file
is an R serialization stream, opaque to ggpaintr until
[`readRDS()`](https://rdrr.io/r/base/readRDS.html) runs. It is supported
because users with data already in that format expect it; if you serve
untrusted users, pair an `expr_check` policy with a host-level upload
filter.

### Normalization is automatic

Every successful upload passes through
[`ptr_normalize_column_names()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_normalize_column_names.md)
(or `ptr_normalize_tabular_data()` for non-`data.frame` returns from
`readRDS`/`readxl`/`jsonlite`). Names arriving with spaces, reserved
words, or duplicates leave normalized, syntactic, and unique, so
downstream `ppVar` pickers always see a clean column vector. You never
call these yourself for uploads — only for in-session data frames you
reference by name
([`vignette("ggpaintr-tutorial")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-tutorial.md),
§1).

The file *stem* is sanitised too: a stem matching an R reserved word is
renamed (`if.csv` → dataset name `if_`), not rejected — the upload still
succeeds under the safe name, so it can’t shadow the `if` keyword if the
stem later reaches an `ppExpr` box.

### What is and isn’t validated

The pipeline validates *shape* (parseable as a recognised tabular
format) and *names* (syntactic, unique, non-reserved). It does **not**
validate:

- **Cell values.** A column may hold any content the parser produces. A
  bad value fails at render time and surfaces through
  `state$runtime()$error`; ggpaintr does not inspect cells.
- **Schema.** Nothing requires an upload to carry `Sepal.Length` just
  because the formula names it. A missing referenced column is a runtime
  error, not a parse-time rejection.
- **Size / rate limits.** Shiny’s `shiny.maxRequestSize` caps body size
  (host configuration). Per-user rate limits, file-count caps, and
  anti-abuse policy are the host’s concern.

If you need any of these, layer them above ggpaintr — a custom source
placeholder that wraps an upload and enforces a schema
([`vignette("ggpaintr-tutorial")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-tutorial.md),
§2.3) — or below it, at the deployment tier (Shiny / reverse-proxy
configuration).
