# LLM materials audit — findings (2026-05-17)

Plan: `dev/plans/2026-05-17-llm-materials-audit.html` (#LLMA). Authority: `CONTEXT.md` (LOCKED 2026-05-16, ADR 0005) + the post-redesign `vignettes/ggpaintr-use-cases.Rmd` / `vignettes/ggpaintr-llm.Rmd` (reference only — not edited). Branch: `l2l3-followup-llm-audit`.

This pass was **verification-only on the corpus**: the audit found the Step-08 corpus already clean and CONTEXT.md-aligned, so no `inst/llm/` edit was needed. The two boundary-sitting defects below were put to the user (`AskUserQuestion`, 2026-05-17) and the user chose **flag-only, no edit** for both; a third residual issue inside `ggpaintr-llm.Rmd` is recorded here for the vignette owner. No `R/`, `README.Rmd`, `vignettes/`, `tests/`, `NAMESPACE`, or `man/` file was modified by this plan.

## Verified PASS

- **No removed-symbol / dead-id leak in the corpus.** `grep -rnE 'ptr_controls_ui|ptr_outputs_ui|ptr_ui_code_toggle|ptr_ui_plot_code|ptr_ui_outputs|ptr_shared_ui|level2_embed|level2_namespacing|server_ns_fn|ui_ns_fn|render_shared_section|code_toggle=|headless|testServer'` over `inst/llm/` returns only the **explicitly-negative "removed/superseded" framing** the success criteria allow (e.g. `overview.md:59`, `primer.md:67,81,88`, `level2_custom_ids.md:26`, `level3_custom_render.md:7,11`, `level3_layout.md:9` — each says the thing does *not* exist / is *not* a user tool). No live recommendation of any removed symbol.
- **Enumeration round-trip.** `ptr_llm_topics()` returns exactly the 12 files present (`identical(files, topics) == TRUE`); `invisible(lapply(ptr_llm_topics(), ptr_llm_topic))` runs with no error; `level2_embed` is not reachable; no present topic is unlisted. `ptr_llm_topics()` is `sort(sub(".md$","",list.files(dir, "\\.md$")))` — a directory scan, not a static registry, so it cannot drift by construction.
- **Primer topic list is current.** `primer.md:111-122` enumerates exactly the 12 shipped topics with accurate one-line descriptions; the `## Before writing R code` block points the model at `ggpaintr_docs(topic)` correctly.
- **Corpus states the CONTEXT.md model.** `ptr_<x>`=L2-self-contained / `ptr_ui_<x>`=L3-bare convention (`primer.md:19-24`, `overview.md:5`); the partition rule + single-vs-multi-instance hard split (`primer.md:49-63`, `formula_syntax.md:48-68`, `level2_module.md:30-40`, `level2_shared.md` whole); the combinator recipe; custom-render-is-L3 via the `moduleServer(id)` + `ptr_server()` pattern (`level3_custom_render.md`); no headless/`testServer`-as-feature framing anywhere.
- **Token-consistent with the vignettes (zero divergence).** The combinator recipe `ptr_ui_toggle_code(ptr_ui_inline_error(ptr_ui_plot(id), ptr_ui_error(id)), ptr_ui_code(id))` is byte-identical between `level3_layout.md:27` / `primer.md:82` and `ggpaintr-use-cases.Rmd:367`. The canonical custom-render scaffold (`plot1` id, `custom_plot`, `state$runtime()`, `req(isTRUE(res$ok), res$plot)`, `plotly::ggplotly`) matches `level3_custom_render.md:33-55` ⇔ `ggpaintr-use-cases.Rmd:458-491` verbatim. Shared-coordinator examples (`metric`/`sz`/`ax1`/`ax2` keys, `ptr_shared` → `ptr_shared_panel` + `ptr_shared_server`) are consistent across `level2_shared.md`, `level3_layout.md`, `level3_custom_render.md`, and the vignette.
- **`ptr_llm_*` roxygen is clean.** The roxygen for `ptr_llm_primer` / `ptr_llm_register` (`R/paintr-llm.R:1-16`, `:96-125`) names no removed symbol and no dead topic id — no roxygen edit was warranted, so `devtools::document()` was not run.

## Findings — FIXED 2026-05-17 (user approved the fix pass)

> Update 2026-05-17: the user directed all three findings be fixed. A and B fixed below; C auto-resolved by the A fix (no vignette edit needed). See the per-finding **RESOLUTION** lines.

### Finding A — `ptr_llm_register()` `type_enum` description string is stale (HIGH, LLM-facing)

`R/paintr-llm.R`, the `arguments = list(name = ellmer::type_enum(topic_names, paste0(...)))` description string, **lines 169-176**:

> `"... level1_ptr_options for session-wide defaults; level2_embed for embedding in the user's own Shiny app via module or split paths; level2_custom_ids and level2_namespacing for id and multi-instance concerns; level2_ui_text for copy overrides; level3_custom_render ... ; custom_placeholder for widget types beyond var/text/num/expr/upload."`

The enum **values** are dynamic and correct (`topic_names <- ptr_llm_topics()`, `R/paintr-llm.R:150`), so the model can only *select* a shipped topic. But the enum **description** — the natural-language guidance the model reads to *choose* which topic — still:

- names dead topic ids `level2_embed` and `level2_namespacing` (both removed in Step 08; `level2_embed` was the exact Step-10 discovery that motivated this plan);
- says "module or **split paths**" — the `ptr_controls_ui`/`ptr_outputs_ui` split is removed (CONTEXT.md §"L2", `dev/adr/0005`);
- **omits** the live topics `level2_module`, `level2_shared`, and `level3_layout` from its per-topic guidance, so the model gets no hint to request them.

Rationale / why it matters: this is precisely the "wider LLM surface was not verified end-to-end" failure the plan exists to catch — a model using the shipped `ptr_llm_register()` helper is told to ask for two non-existent topics and given no guidance toward three real ones. **Required change** (for a follow-up plan that owns `R/`): rewrite only the `type_enum` description string to enumerate the 12 live topics with accurate one-line guidance (mirror `primer.md:111-122`); no signature/logic/control-flow change.

**RESOLUTION (2026-05-17, FIXED):** the `type_enum` description string in `R/paintr-llm.R` (`ptr_llm_register()`, the `paste0(...)` second arg to `ellmer::type_enum()`) was rewritten to enumerate exactly the 12 live topics with one-line guidance mirroring `primer.md:111-122`. `level2_embed`/`level2_namespacing` and "split paths" are gone; `level2_module`/`level2_shared`/`level3_layout` are now described. Only the string literal changed — no signature, control-flow, or enum-values change (values remain `topic_names <- ptr_llm_topics()`).

### Finding B — `README.Rmd:60` names removed symbols (MEDIUM, propagates to README.md)

`README.Rmd:60` (the *use-cases* bullet under `## More topics`) describes L2 as `ptr_controls_ui` / `ptr_outputs_ui` / `ptr_server` and lists `ptr_ui_code_toggle()` among the L3 panes — all removed in the L2/L3 redesign (CONTEXT.md §"L2 public surface" / §"Combinator removed/superseded"). The leak is **already present in the generated `README.md:61-67`** and any future re-knit carries it forward.

Rationale / why it matters: README is first-contact discoverability; a reader (or an LLM scraping the README) is pointed at four functions that no longer exist. **Required change** (for whoever owns README / the use-cases-bullet copy): update line 60 to the current L2/L3 surface (`ptr_module_ui`/`ptr_module_server`, shared trio; `ptr_ui_*` bare pieces + combinators) per CONTEXT.md, then re-knit `README.md`.

**RESOLUTION (2026-05-17, FIXED):** `README.Rmd:60` rewritten to the current surface — L2 = `ptr_module_ui()`/`ptr_module_server()` + the shared-coordinator trio (`ptr_shared()`/`ptr_shared_panel()`/`ptr_shared_server()`); L3 = bare builders + the combinators (`ptr_ui_inline_error()`/`ptr_ui_toggle_code()`) + optional `ptr_ui_page()`. `ptr_controls_ui`/`ptr_outputs_ui`/`ptr_ui_code_toggle` removed; custom-render mention moved under L3 (per CONTEXT.md). `README.md` re-knit from the edited `README.Rmd`; `grep -nE 'ptr_controls_ui|ptr_outputs_ui|ptr_ui_code_toggle' README.md` now returns zero.

## Residual LLM-semantic issue in `ggpaintr-llm.Rmd` (flag for the vignette owner — do NOT edit)

`ggpaintr-llm.Rmd` has **no** removed-symbol / dead-id prose (Step 07 + the merged prose-sweep already corrected it; its topic table `:105-110` lists only live topics). One residual *semantic* gap remains, downstream of Finding A:

- **`ggpaintr-llm.Rmd:165-203` (§7 "Manual registration" + §"The pieces")** implicitly overstates the truthfulness of the bundled `ptr_llm_register()` helper. The section's hand-rolled example uses a *minimal* description (`:175-178`) and correctly explains that `type_enum(ptr_llm_topics(), …)` keeps the **enum values** current and that "the helper snapshots the list once at registration time" (`:195-203`). A reader reasonably concludes the only freshness concern is *re-registering after upgrade*. It never discloses that the **shipped helper's own built-in description string** additionally hard-codes per-topic guidance that can (and currently does — Finding A) name removed topics. Until Finding A is fixed, this section leaves the reader believing `ptr_llm_register()` is fully truthful when re-registered, which it is not. **Suggested change** (for the vignette owner, after Finding A): add one sentence in §7 noting that re-registration refreshes the enum *values* but the helper's guidance prose is fixed at the shipped version — or simply drop the caveat once Finding A makes the prose accurate.

**RESOLUTION (2026-05-17, AUTO-RESOLVED by Finding A — no vignette edit):** §7 (`ggpaintr-llm.Rmd:191-203`) contains no actively-false statement — only the omission this finding describes. With Finding A fixed, the shipped `ptr_llm_register()` description prose now enumerates the 12 live topics accurately, so a reader's conclusion that the helper is truthful when re-registered is now *correct*. Per this finding's own "or simply drop the caveat once Finding A makes the prose accurate", no vignette change is warranted (avoids unneeded churn; vignette stays reference-only for this plan).

## Net

Corpus + enumeration + roxygen: **PASS, no change required** (verified by the original audit pass).

**Fix pass 2026-05-17 (user-directed):** Finding A FIXED (`R/paintr-llm.R` `type_enum` description string → 12 live topics); Finding B FIXED (`README.Rmd:60` rewritten + `README.md` re-knit); Finding C AUTO-RESOLVED by A (no vignette edit — §7 had only an omission, now neutralised). Net surface change: one R string literal, one README.Rmd line + its generated `README.md`, this note. No signature/logic/test/NAMESPACE/man change.
