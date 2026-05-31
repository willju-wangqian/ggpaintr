# Curriculum index — ggpaintr-source-expert

Load only the files relevant to the current question. Each entry below is a one-line summary so you can decide.

- `00-module-map.md` — table of every `R/paintr-*.R` file to its role (parse, runtime, UI, registry, etc.). **Load on most questions.**
- `01-architecture.md` — the L2/L3 model, the parse → registry → server → runtime → UI pipeline in prose, reactive flow diagram.
- `02-key-paths.md` — annotated traces of the most important code paths: formula → tree, source placeholder → consumer placeholder → output bind, upload flow, custom-render L3 path.
- `03-design-decisions.md` — ADR digest: one paragraph per ADR with the decision, the why, and a link to the full doc. Use to answer "why is it like this?".
- `04-invariants.md` — load-bearing invariants the code depends on: denylist + AST walker safety model, registry env quirks, htmlDependency prefix, shared-coordinator partition rules, registry-init ordering.
- `05-extensibility.md` — how to extend ggpaintr: custom placeholders, L3 custom rendering off `state$runtime()`, bind helpers, custom UI hosting.
- `06-pitfalls.md` — durable lessons from prior debugging: shinytest2 quirks, registry env split under raw `test_file`, debounce + paired-observer race, etc. Promoted from auto-memory.

## Infrastructure files (not knowledge but referenced)

- `../symbol-lines.json` — auto-generated manifest of every top-level function definition with its current `start_line` / `end_line`. **Canonical source of truth for line numbers** — prefer over the inline citations in the knowledge files when exact current-state line accuracy matters. See SYSTEM.md "Symbol-line manifest" for protocol.
- `../build-symbol-manifest.R` — rebuild script for the manifest. The user runs it; do not invoke it yourself (scope guard: no editing curriculum infrastructure).

## Loading guide by question type

| Question shape | Load |
|---|---|
| "Where is X?" / "what file is X in?" | `00-module-map.md` |
| "How does the package work overall?" | `00-module-map.md` + `01-architecture.md` |
| "Trace what happens when ..." | `00-module-map.md` + `02-key-paths.md` |
| "Why is it designed this way?" | `03-design-decisions.md` (+ open the ADR it points to if needed) |
| "Can I rely on X?" / "what assumption does Y make?" | `04-invariants.md` |
| "How do I add my own ...?" | `05-extensibility.md` |
| "Why did this break / why does X happen at runtime?" | `06-pitfalls.md` + open source |
