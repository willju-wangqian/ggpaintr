# ggpaintr: the high-level picture

*A narrative source document. Draw abstracts, README intros, talk openings, and proposal background from this.*

## 1. Origin

ggpaintr began from a small observation about what it actually feels like to build a Shiny application around a ggplot2 chart. The task is not intellectually hard, but it is tedious in a specific way: the author has to describe the same chart three times — once as UI widgets (`selectInput`, `numericInput`, `textInput`) in a separate `ui.R` file, once as server-side reactive plumbing (input binding, `reactive()`, `observeEvent()`, `req()` guards, and the output renderer), and once as the ggplot expression itself, whose value slots must be hand-wired back to the widgets above.

The three copies drift. Renaming a widget ripples across files. Swapping a geom or a column requires coordinated edits in places that do not look at each other. A copy-paste mistake produces an app where the UI says one thing and the plot draws another, and a one-line chart becomes an exercise in bookkeeping.

The original idea behind ggpaintr was to collapse these three copies into one. Let the author describe the chart once, in a ggplot-like formula string, with widget specifications embedded inline at the exact value slots where their outputs are used. A single call would compile that formula into a running Shiny app — widgets, plot, reactive wiring, and all. That hypothesis has held. What began as a thin formula-to-app translator has grown into a broader system, and this document captures what ggpaintr has become.

## 2. What ggpaintr does

Given a formula like:

```r
ptr_app("
ggplot(data = iris, aes(x = var, y = var)) +
  geom_point(aes(color = var), size = num) +
  labs(title = text) +
  facet_wrap(expr)
")
```

ggpaintr parses the formula, discovers the placeholder tokens (`var`, `text`, `num`, `expr`, `upload`), generates the corresponding Shiny widgets with labels and help copy inferred from slot context, wires up the reactive flow, renders the plot, and exposes a live code pane that mirrors the exact ggplot expression that produced what is on screen. No `ui.R`, no `server.R`, no `reactive()`, no input binding is written by hand.

The five built-in placeholders cover most real authoring needs: `var` is a column picker that returns a column symbol; `text` is a free-text input; `num` is a numeric input; `expr` is an arbitrary R expression guarded by a denylist-protected AST walker; `upload` accepts `.csv` or `.rds` data at runtime and normalizes column names automatically.

## 3. Mental model: a small compiler pipeline

ggpaintr is best understood as a small compiler pipeline, not as a single convenience function. The **source language** is the formula string — a constrained ggplot2-like DSL with placeholder keywords. The **intermediate representation** is the object returned by `ptr_parse_formula()`; every downstream subsystem consumes this same IR, including the UI builder, the reactive runtime, the code-pane generator, the `ui_text` copy merger, the placeholder hooks, and the headless executor. The **code-generation targets** are the three integration levels described in the next section.

Extension mechanisms hang off the IR. The placeholder registry (`ptr_define_placeholder()`, `ptr_merge_placeholders()`) lets authors add new widget types that traverse the same parse / UI / runtime / copy pipeline as the built-ins. The `ui_text` four-layer merge (shell, defaults, params, layers) localizes every label and help message with well-defined precedence.

This framing is why capabilities that look heterogeneous at the user level — inline widget declaration, embedding inside an existing app, headless render, custom placeholder packs — coexist in one package without fragmenting the codebase. They are not separate features bolted together; they are different consumers of the same IR.

## 4. Three levels of use

ggpaintr exposes the pipeline at three integration levels. Each level trades convenience for control; the right level for a user is the lowest one that covers their need.

**Level 1 — Turn-key.** `ptr_app()` and `ptr_app_bslib()` compile a formula into a running Shiny application in one call. For teaching, demos, quick exploration, and ggplot2-fluent authors who do not want to write Shiny code.

**Level 2 — Embed.** `ptr_input_ui()`, `ptr_output_ui()`, `ptr_server()`, the granular `ptr_register_*()` helpers, `ptr_build_ids()`, and the `ui_text` override system let ggpaintr's widgets and runtime live inside an existing Shiny application under that application's own element ids and copy. For developers who already have an app and want to reduce its per-chart boilerplate without importing a second framework.

**Level 3 — Parse / exec / extract.** `ptr_parse_formula()`, `ptr_runtime_input_spec()`, `ptr_exec()`, `ptr_extract_plot()`, `ptr_extract_code()`, `ptr_extract_error()`, `ptr_gg_extra()`, and `ptr_assemble_plot()` expose the compiler pipeline directly. For batch reports, headless rendering, programmatic code generation, user-owned `renderPlot()` blocks that post-process the ggplot object, and host applications that inject their own layers while keeping the code pane in sync.

## 5. Why it is better than the hand-written equivalent

ggpaintr's value over writing the same Shiny-ggplot app by hand rests on five load-bearing properties.

1. **Inline widget declaration.** The widget specification lives exactly where its value is used inside the ggplot expression. Changing an input no longer means coordinating edits across three files.
2. **Single source of truth.** The parsed IR drives the UI, the reactive wiring, the rendered plot, and the generated code pane simultaneously. The UI and the plot cannot drift out of sync because they are derived from the same object.
3. **Zero reactive plumbing.** The error-prone layer of Shiny development — `reactive()`, `observeEvent()`, `req()` guards, input binding, output rendering — is eliminated by default. The author never sees it unless they opt into Level 2.
4. **Composable with real Shiny.** Level 2 means ggpaintr is not a walled garden. Developers do not have to choose between "a ggpaintr app" and "a real Shiny app"; the widgets and runtime embed into an existing application under that application's own ids and copy.
5. **Extensible widget vocabulary.** `ptr_define_placeholder()` lets teams ship their own placeholder packs — date pickers, column-subset choosers, domain-specific inputs — that participate in the same formula DSL as the built-ins. The IR is an open extension point, not a closed feature set.

A secondary benefit, not load-bearing but worth noting: the live code pane turns the Shiny app into a teaching and scripting artifact. Users can copy the rendered ggplot expression into a script for reproducible, non-interactive use.

## 6. Audience

ggpaintr serves two audiences, both first-class. Shiny developers are the primary audience in ecosystem terms — they use Level 2 and Level 3 to reduce per-chart boilerplate in real applications, and `ptr_define_placeholder()` to standardize custom widget types across a team. For this audience, ggpaintr is a better way of building a Shiny app with ggplot2: not an alternative to Shiny, but a smaller authoring surface on top of it.

ggplot2-fluent users without Shiny expertise are the beneficiary audience. They use `ptr_app()` or `ptr_app_bslib()` to turn a chart formula into a shareable interactive artifact without writing any Shiny code. For this audience, ggpaintr is the shortest path from "I know ggplot2" to "I have a live app."

The package threads the needle by keeping the same formula DSL and IR across both audiences. Developers and non-developers write the same kind of formula; only the wrapper around it changes.

## 7. Positioning against neighbors

Several R packages occupy adjacent territory. ggpaintr does not compete with any of them, and the reasons are worth being explicit about.

**esquisse** is a GUI chart builder for end users who do not write code, whereas ggpaintr is a code-first DSL for developers. **plotly** and **ggplotly** add pan-zoom-hover interactivity to a static ggplot; ggpaintr adds parameter-driven interactivity, where the user picks columns, numbers, and expressions and the plot re-draws. **Golem** and **Rhino** are production Shiny application frameworks that organize Shiny code at the project level; they do not reduce the per-chart boilerplate that ggpaintr targets, and a ggpaintr-driven chart can live inside a Golem or Rhino project. **shinyuieditor** generates Shiny UI code that the user then edits by hand; ggpaintr does not generate editable scaffolding — the formula itself is the authoring surface. **flexdashboard** and **Quarto dashboards** provide layout shells and are orthogonal; ggpaintr components can live inside them.

Most of these neighbors serve either end users (chart builders, visual dashboards) or production-framework concerns (project layout, module conventions, CI). ggpaintr targets the per-chart authoring experience itself, for developers and for ggplot2-fluent users who are willing to write code.

## 8. Trajectory

ggpaintr's near-term growth is not about adding more core features. It is about compatibility with the tools people already use. The formula and the IR should be able to reach users wherever they are writing: in a running Shiny app today, and increasingly in Quarto documents, static HTML, and other publishing and runtime platforms.

The parse / exec / extract split at Level 3 makes this plausible — nothing about the IR is Shiny-specific. Concrete possibilities worth exploring, all future work and none in scope today, include a `shinylive` packaging helper that runs a `ptr_app()` in the browser without a server, first-class Quarto integration (chunk helpers, `server: shiny` ergonomics, or a WASM back-end), and non-Shiny back-ends that render the IR into an `htmlwidget`, an Observable reactive block, or a static interactive HTML page.

The authoring surface — a ggplot2-like formula string — is expected to remain stable. What grows is the set of places that formula can go.
