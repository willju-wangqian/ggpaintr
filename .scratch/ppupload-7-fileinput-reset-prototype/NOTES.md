# Prototype verdict — ppUpload #7 fileInput visual reset

**Question:** When a fileInput lives inside a `uiOutput` and the server re-renders that uiOutput with the **same** `inputId`, does the displayed filename pill actually **clear** in a real browser — while a **static** sibling `textInput` survives the re-render untouched? (chromote/CDP cannot prove this; handoff §6.)

**Design being validated (handoff approach a):**
- UI: `tagList(uiOutput("file_slot"), textInput("shortcut"))` — textInput is STATIC, never inside a renderUI.
- Server: `output$file_slot <- renderUI({ bump(); fileInput("upload", ...) })`.
- Trigger: RISING EDGE only (`shortcut` empty→nonempty bumps; clearing it does NOT → file-pick can't wipe the just-uploaded display).

## Run (real browser — mandatory, automation can't see the file pill)

```
R -e 'shiny::runApp(".scratch/ppupload-7-fileinput-reset-prototype", launch.browser=TRUE)'
```
Test files staged: `/tmp/mt.csv`, `/tmp/iris.csv`.

## The three things to eyeball (watch the file widget's filename pill)

1. **Re-render clears the pill.** Pick `/tmp/mt.csv` → widget shows "mt.csv". Type any char in *shortcut* → on the FIRST char the file widget's displayed filename should CLEAR. (Action log shows `RISING edge => bump` + `renderUI(file_slot) fired`.)
2. **Typing not disrupted.** Keep typing after the first char → cursor/focus stays in the textbox, no further re-renders (log shows no new bump). The textbox is static, so this should be clean.
3. **Asymmetric — file-pick does NOT wipe its own display.** Pick a file again → the textbox auto-clears (log: `file picked … updateTextInput`), and the log shows `NO bump (asymmetric)` — the file widget must KEEP showing the just-picked name.

## VERDICT (eyeballed in real browser 2026-05-29 — all YES)

- [x] (1) same-id uiOutput re-render clears the visible filename: **YES** — rising edge `'' -> 's'`/`'abc'` → `renderUI(file_slot) fired [bump=1/2]`, pill cleared.
- [x] (2) static textInput typing undisturbed: **YES** — only the first char bumps; textbox is static so typing continues cleanly.
- [x] (3) asymmetric trigger preserves just-uploaded display: **YES** — `file picked … updateTextInput` logged `NO bump (asymmetric)`; file display kept.

→ Approach (a) confirmed. Versioned-id fallback NOT needed. Port to ggpaintr (plan below / in chat).

**If all YES** → approach (a) is sound; port to `ptr_setup_source_uis` (emit `tagList(uiOutput(file slot), textInput(shortcut))` gated on `entry$shortcut`; `build_ui` returns only the source widget; rising-edge bump replaces the `ptr_reset_file_input` JS + text→file mutex half; keep the file→text `updateTextInput`). Then delete this prototype.

**If (1) is NO** → same-id re-render does not clear the pill in a real browser → fall back to versioned-id (handoff approach c): fresh fileInput id each clear. Heavier (threads a changing id through the resolver) but clears both visual + server value.
