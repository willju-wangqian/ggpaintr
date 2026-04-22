const pptxgen = require("/opt/homebrew/lib/node_modules/pptxgenjs");

const pres = new pptxgen();
pres.layout = "LAYOUT_16x9"; // 10 x 5.625
pres.author = "Will Ju";
pres.title = "ggpaintr Showcase Demo";

// Palette
const C = {
  ink:      "0F172A",
  body:     "1E293B",
  muted:    "64748B",
  line:     "E2E8F0",
  paper:    "FFFFFF",
  soft:     "F8FAFC",
  code:     "F1F5F9",
  accent:   "0D9488", // teal-600
  accentDk: "0F766E",
  hi:       "F59E0B",
};

const RMD = "file:///Users/willju/Research/ggpaintr/dev/demo/demo.Rmd";
const MONO = "Consolas";
const HEAD = "Georgia";
const BODY = "Calibri";

const W = 10, H = 5.625;

// ---------- helpers ----------
function footer(slide, partLabel) {
  slide.addShape(pres.shapes.LINE, {
    x: 0.5, y: H - 0.45, w: W - 1, h: 0,
    line: { color: C.line, width: 0.75 },
  });
  slide.addText(partLabel, {
    x: 0.5, y: H - 0.4, w: 6, h: 0.3,
    fontFace: BODY, fontSize: 9, color: C.muted, margin: 0,
  });
  slide.addText("ggpaintr showcase · Will Ju", {
    x: W - 4.5, y: H - 0.4, w: 4, h: 0.3,
    fontFace: BODY, fontSize: 9, color: C.muted, align: "right", margin: 0,
  });
}

function chunkRef(slide, label, line) {
  // right side card linking to the Rmd
  const x = W - 3.5, y = 1.0, w = 3.0, h = 1.3;
  slide.addShape(pres.shapes.RECTANGLE, {
    x, y, w, h,
    fill: { color: C.soft }, line: { color: C.line, width: 0.75 },
  });
  slide.addShape(pres.shapes.RECTANGLE, {
    x, y, w: 0.08, h,
    fill: { color: C.accent }, line: { type: "none" },
  });
  slide.addText("SOURCE CHUNK", {
    x: x + 0.2, y: y + 0.1, w: w - 0.3, h: 0.25,
    fontFace: BODY, fontSize: 9, bold: true, color: C.accent, charSpacing: 2, margin: 0,
  });
  slide.addText([
    { text: label, options: { fontFace: MONO, fontSize: 13, color: C.ink, bold: true } },
  ], {
    x: x + 0.2, y: y + 0.35, w: w - 0.3, h: 0.35, margin: 0,
  });
  slide.addText([
    { text: "Line ", options: { fontFace: BODY, fontSize: 10, color: C.muted } },
    { text: `${line}`, options: { fontFace: MONO, fontSize: 11, color: C.body, bold: true } },
    { text: "  ·  ", options: { fontFace: BODY, fontSize: 10, color: C.muted } },
    { text: "open demo.Rmd ›", options: { fontFace: BODY, fontSize: 10, color: C.accentDk, bold: true, hyperlink: { url: RMD } } },
  ], {
    x: x + 0.2, y: y + 0.75, w: w - 0.3, h: 0.35, margin: 0,
  });
}

function titleBar(slide, kicker, title) {
  slide.addText(kicker, {
    x: 0.5, y: 0.35, w: 9, h: 0.3,
    fontFace: BODY, fontSize: 11, bold: true, color: C.accent, charSpacing: 3, margin: 0,
  });
  slide.addText(title, {
    x: 0.5, y: 0.65, w: 9, h: 0.7,
    fontFace: HEAD, fontSize: 30, bold: true, color: C.ink, margin: 0,
  });
  slide.addShape(pres.shapes.LINE, {
    x: 0.5, y: 1.42, w: 0.6, h: 0,
    line: { color: C.accent, width: 2.5 },
  });
}

function bulletBlock(slide, items, opts={}) {
  const x = opts.x ?? 0.5;
  const y = opts.y ?? 1.7;
  const w = opts.w ?? 6;
  const h = opts.h ?? 3.3;
  const runs = [];
  items.forEach((it, i) => {
    const last = i === items.length - 1;
    if (typeof it === "string") {
      runs.push({ text: it, options: { bullet: true, fontFace: BODY, fontSize: 16, color: C.body, paraSpaceAfter: 8, ...(last ? {} : { breakLine: true }) } });
    } else {
      runs.push({ text: it.text, options: { bullet: true, fontFace: BODY, fontSize: 16, color: C.body, bold: it.bold, paraSpaceAfter: 8, ...(last ? {} : { breakLine: true }) } });
    }
  });
  slide.addText(runs, { x, y, w, h, margin: 0 });
}

function codeBlock(slide, code, opts={}) {
  const x = opts.x ?? 0.5;
  const y = opts.y ?? 3.3;
  const w = opts.w ?? 6;
  const h = opts.h ?? 1.65;
  slide.addShape(pres.shapes.RECTANGLE, {
    x, y, w, h,
    fill: { color: C.code }, line: { color: C.line, width: 0.5 },
  });
  slide.addText(code, {
    x: x + 0.15, y: y + 0.1, w: w - 0.3, h: h - 0.2,
    fontFace: MONO, fontSize: 11, color: C.body, margin: 0, valign: "top",
  });
}

// ---------- Slide 1: Title ----------
{
  const s = pres.addSlide();
  s.background = { color: C.ink };
  // accent bar
  s.addShape(pres.shapes.RECTANGLE, {
    x: 0, y: 0, w: 0.18, h: H,
    fill: { color: C.accent }, line: { type: "none" },
  });
  s.addText("R · ggplot2 · Shiny", {
    x: 0.8, y: 1.3, w: 8, h: 0.35,
    fontFace: BODY, fontSize: 13, bold: true, color: C.accent, charSpacing: 4, margin: 0,
  });
  s.addText("ggpaintr", {
    x: 0.8, y: 1.7, w: 8.5, h: 1.1,
    fontFace: HEAD, fontSize: 62, bold: true, color: C.paper, margin: 0,
  });
  s.addText("A formula string becomes a Shiny app.", {
    x: 0.8, y: 2.85, w: 8.5, h: 0.5,
    fontFace: HEAD, fontSize: 22, italic: true, color: "CBD5E1", margin: 0,
  });
  s.addShape(pres.shapes.LINE, {
    x: 0.8, y: 3.55, w: 1.2, h: 0, line: { color: C.accent, width: 2.5 },
  });
  s.addText([
    { text: "Will Ju", options: { fontFace: BODY, fontSize: 16, bold: true, color: C.paper, breakLine: true } },
    { text: "Showcase Demo", options: { fontFace: BODY, fontSize: 13, color: "94A3B8" } },
  ], { x: 0.8, y: 3.75, w: 8, h: 0.9, margin: 0 });
}

// ---------- Slide 2: About ----------
{
  const s = pres.addSlide();
  s.background = { color: C.paper };
  titleBar(s, "ABOUT THIS DEMO", "What is ggpaintr?");

  bulletBlock(s, [
    { text: "Turns a ggplot-like formula string into a live Shiny app.", bold: true },
    "Placeholder tokens inside the formula become widgets automatically.",
    "Five placeholders: var · text · num · expr · upload.",
    "Generated code pane mirrors user choices in real time.",
    "Run one example at a time — close the app before the next.",
  ], { x: 0.5, y: 1.7, w: 6.3, h: 3.3 });

  // placeholder chips on the right
  const chips = [
    { k: "var",    d: "column picker" },
    { k: "text",   d: "string input" },
    { k: "num",    d: "numeric input" },
    { k: "expr",   d: "parsed R expression" },
    { k: "upload", d: "file input" },
  ];
  const cx = 7.1, cy = 1.7, cw = 2.4, ch = 0.58;
  chips.forEach((c, i) => {
    const y = cy + i * (ch + 0.08);
    s.addShape(pres.shapes.RECTANGLE, {
      x: cx, y, w: cw, h: ch,
      fill: { color: C.soft }, line: { color: C.line, width: 0.5 },
    });
    s.addShape(pres.shapes.RECTANGLE, {
      x: cx, y, w: 0.08, h: ch,
      fill: { color: C.accent }, line: { type: "none" },
    });
    s.addText([
      { text: c.k, options: { fontFace: MONO, fontSize: 14, bold: true, color: C.ink, breakLine: true } },
      { text: c.d, options: { fontFace: BODY, fontSize: 10, color: C.muted } },
    ], { x: cx + 0.18, y: y + 0.05, w: cw - 0.25, h: ch - 0.08, margin: 0 });
  });

  footer(s, "About · overview of placeholders");
}

// ---------- Slide 3: Part 0 cover ----------
{
  const s = pres.addSlide();
  s.background = { color: C.ink };
  s.addShape(pres.shapes.RECTANGLE, { x: 0, y: 0, w: 0.18, h: H, fill: { color: C.accent }, line: { type: "none" } });
  s.addText("PART 0", {
    x: 0.8, y: 1.8, w: 8, h: 0.4,
    fontFace: BODY, fontSize: 14, bold: true, color: C.accent, charSpacing: 5, margin: 0,
  });
  s.addText("Motivation: building Shiny + ggplot by hand", {
    x: 0.8, y: 2.2, w: 8.5, h: 1.2,
    fontFace: HEAD, fontSize: 38, bold: true, color: C.paper, margin: 0,
  });
  s.addText("How far can three selectInputs take us before the wiring gets painful?", {
    x: 0.8, y: 3.5, w: 8.5, h: 0.7,
    fontFace: HEAD, fontSize: 18, italic: true, color: "94A3B8", margin: 0,
  });
}

// ---------- Slide 4: simple-iris-1 ----------
{
  const s = pres.addSlide();
  s.background = { color: C.paper };
  titleBar(s, "PART 0 · V1", "A simple Shiny app for iris");
  bulletBlock(s, [
    "Three selectInputs bound to x, y, color — all from names(iris).",
    "Plot re-renders on every change; no button.",
    "ggplot uses .data[[input$x]] to read columns by string.",
    "Works, but every new control means new UI + new server wiring.",
  ], { x: 0.5, y: 1.7, w: 6, h: 3.3 });
  chunkRef(s, "simple-iris-1", 43);
  footer(s, "Part 0 · manual Shiny — version 1");
}

// ---------- Slide 5: simple-iris-2 ----------
{
  const s = pres.addSlide();
  s.background = { color: C.paper };
  titleBar(s, "PART 0 · V2", "Add a draw button and a plot title");
  bulletBlock(s, [
    "textInput for the title, actionButton to gate rendering.",
    "eventReactive(input$draw) bundles state; ignoreNULL = FALSE for first paint.",
    "labs(title = p$title) threaded through the reactive payload.",
    "Each new widget now touches: UI, reactive list, and renderPlot body.",
  ]);
  chunkRef(s, "simple-iris-2", 76);
  footer(s, "Part 0 · manual Shiny — version 2");
}

// ---------- Slide 6: simple-iris-3 ----------
{
  const s = pres.addSlide();
  s.background = { color: C.paper };
  titleBar(s, "PART 0 · V3", "Add facet_wrap and geom_smooth");
  bulletBlock(s, [
    "Two more selectInputs: facet column and smoothing method.",
    "Empty-string sentinels with nzchar() gate optional layers.",
    "renderPlot body now branches on inputs — conditional layer logic.",
    "Scaling pain: error handling, code visibility, file splits still ahead.",
  ]);
  chunkRef(s, "simple-iris-3", 121);
  footer(s, "Part 0 · manual Shiny — version 3");
}

// ---------- Slide 7: Pain points ----------
{
  const s = pres.addSlide();
  s.background = { color: C.paper };
  titleBar(s, "PART 0 · TAKEAWAY", "Where hand-rolled Shiny starts to hurt");

  const pains = [
    { t: "More controls",     d: "Each widget edits UI + reactive + render — three places." },
    { t: "Error messages",    d: "Bad inputs crash the app or render silently empty." },
    { t: "Code visibility",   d: "Users see the plot but not the code that made it." },
    { t: "File organisation", d: "ui.R / server.R split is only a matter of time." },
  ];
  const x0 = 0.5, y0 = 1.7, cw = 4.45, ch = 1.4, gap = 0.15;
  pains.forEach((p, i) => {
    const col = i % 2, row = Math.floor(i / 2);
    const x = x0 + col * (cw + gap);
    const y = y0 + row * (ch + gap);
    s.addShape(pres.shapes.RECTANGLE, { x, y, w: cw, h: ch, fill: { color: C.soft }, line: { color: C.line, width: 0.5 } });
    s.addShape(pres.shapes.RECTANGLE, { x, y, w: 0.08, h: ch, fill: { color: C.hi }, line: { type: "none" } });
    s.addText(p.t, { x: x + 0.25, y: y + 0.15, w: cw - 0.35, h: 0.4, fontFace: HEAD, fontSize: 18, bold: true, color: C.ink, margin: 0 });
    s.addText(p.d, { x: x + 0.25, y: y + 0.55, w: cw - 0.35, h: 0.75, fontFace: BODY, fontSize: 13, color: C.body, margin: 0, valign: "top" });
  });
  footer(s, "Part 0 · why we want a higher-level abstraction");
}

// ---------- Slide 8: simple-iris-ggpaintr ----------
{
  const s = pres.addSlide();
  s.background = { color: C.paper };
  titleBar(s, "PART 0 · ENTER ggpaintr", "Three versions collapse into one formula");
  bulletBlock(s, [
    { text: "Same capabilities — x / y / color / smooth / facet / title.", bold: true },
    "Placeholders var, text, expr mark inputs; widgets are generated.",
    "ptr_app() ships the draw button, error pane, and code pane for free.",
    "No ui.R, no server.R, no eventReactive plumbing.",
  ], { x: 0.5, y: 1.7, w: 6, h: 2.2 });
  codeBlock(s,
`ptr_app("
 ggplot(data = iris, aes(x = var, y = var, color = var)) +
   geom_point() +
   geom_smooth(method = text) +
   facet_wrap(expr) +
   labs(title = text)
")`,
    { x: 0.5, y: 3.85, w: 6, h: 1.25 }
  );
  chunkRef(s, "simple-iris-ggpaintr", 183);
  footer(s, "Part 0 · the ggpaintr equivalent");
}

// ---------- Slide 9: simple-iris-ggpaintr-2 ----------
{
  const s = pres.addSlide();
  s.background = { color: C.paper };
  titleBar(s, "PART 0 · REDESIGN", "Swap dataset and geoms — formula is the only edit");
  bulletBlock(s, [
    "Replace data = iris with data = penguins (palmerpenguins).",
    "Stack geom_point + geom_boxplot + geom_smooth in one formula.",
    "Add fill = var — a new widget appears automatically.",
    "Same ptr_app() call, same widgets-from-placeholders contract.",
  ], { x: 0.5, y: 1.7, w: 6, h: 2.2 });
  codeBlock(s,
`ptr_app("
 ggplot(data = penguins, aes(x = var, y = var, color = var, fill = var)) +
   geom_point() + geom_boxplot() +
   geom_smooth(method = text) +
   facet_wrap(expr) + labs(title = text)
")`,
    { x: 0.5, y: 3.85, w: 6, h: 1.25 }
  );
  chunkRef(s, "simple-iris-ggpaintr-2", 195);
  footer(s, "Part 0 · design changes are formula edits");
}

// ---------- Slide 10: Part 1 cover ----------
{
  const s = pres.addSlide();
  s.background = { color: C.ink };
  s.addShape(pres.shapes.RECTANGLE, { x: 0, y: 0, w: 0.18, h: H, fill: { color: C.accent }, line: { type: "none" } });
  s.addText("PART 1", { x: 0.8, y: 1.8, w: 8, h: 0.4, fontFace: BODY, fontSize: 14, bold: true, color: C.accent, charSpacing: 5, margin: 0 });
  s.addText("Core features", { x: 0.8, y: 2.2, w: 8.5, h: 1.2, fontFace: HEAD, fontSize: 40, bold: true, color: C.paper, margin: 0 });
  s.addText("Three examples, escalating in ambition, all one formula.", { x: 0.8, y: 3.5, w: 8.5, h: 0.7, fontFace: HEAD, fontSize: 18, italic: true, color: "94A3B8", margin: 0 });
}

// ---------- Slide 11: 1.1 basic ----------
{
  const s = pres.addSlide();
  s.background = { color: C.paper };
  titleBar(s, "EXAMPLE 1.1", "The simplest possible app");
  bulletBlock(s, [
    "One formula: two var pickers, one text box.",
    "Everything else is free — the geom has no placeholders.",
    "Click Update plot; code pane mirrors selections.",
    "Smallest useful thing ggpaintr can do.",
  ], { x: 0.5, y: 1.7, w: 6, h: 2.2 });
  codeBlock(s,
`ptr_app("
ggplot(data = iris, aes(x = var, y = var)) +
  geom_point() +
  labs(title = text)
")`,
    { x: 0.5, y: 3.85, w: 6, h: 1.25 }
  );
  chunkRef(s, "basic", 216);
  footer(s, "Part 1 · example 1.1");
}

// ---------- Slide 12: 1.2 all-placeholders ----------
{
  const s = pres.addSlide();
  s.background = { color: C.paper };
  titleBar(s, "EXAMPLE 1.2", "All five placeholders in one app");
  bulletBlock(s, [
    { text: "upload replaces the dataset with a user-uploaded CSV.", bold: false },
    "size / alpha / legend.position driven by num and text.",
    "expr fields take raw R (se = TRUE, facet_wrap(~ group)).",
    "Each layer is individually toggleable — vanishes from plot and code.",
  ], { x: 0.5, y: 1.7, w: 6, h: 2.2 });
  codeBlock(s,
`ptr_app("
ggplot(data = upload, aes(x = var, y = var)) +
  geom_point(aes(color = var), size = num, alpha = num) +
  geom_smooth(method = text, se = expr) +
  labs(title = text, x = text, y = text) +
  facet_wrap(expr) + theme(legend.position = text)
")`,
    { x: 0.5, y: 3.85, w: 6, h: 1.4 }
  );
  chunkRef(s, "all-placeholders", 233);
  footer(s, "Part 1 · example 1.2");
}

// ---------- Slide 13: 1.3 expr-compat ----------
{
  const s = pres.addSlide();
  s.background = { color: C.paper };
  titleBar(s, "EXAMPLE 1.3", "expr: the compatibility escape hatch");
  bulletBlock(s, [
    { text: "expr is parsed as an R expression at draw time.", bold: true },
    "Scales, coords, stats, annotations, vector args — any of them.",
    "No dedicated placeholder needed for obscure ggplot features.",
    "Trailing expr = theme_minimal(...) splices whole components in.",
  ], { x: 0.5, y: 1.7, w: 6, h: 2.2 });
  codeBlock(s,
`scale_color_manual(values = expr) +
coord_cartesian(xlim = expr, ylim = expr) +
labs(title = text) +
expr   # e.g. theme_minimal(base_size = 14)`,
    { x: 0.5, y: 3.85, w: 6, h: 1.25 }
  );
  chunkRef(s, "expr-compat", 262);
  footer(s, "Part 1 · example 1.3");
}

// ---------- Slide 14: Part 2 cover ----------
{
  const s = pres.addSlide();
  s.background = { color: C.ink };
  s.addShape(pres.shapes.RECTANGLE, { x: 0, y: 0, w: 0.18, h: H, fill: { color: C.accent }, line: { type: "none" } });
  s.addText("PART 2", { x: 0.8, y: 1.8, w: 8, h: 0.4, fontFace: BODY, fontSize: 14, bold: true, color: C.accent, charSpacing: 5, margin: 0 });
  s.addText("Three levels of extensibility", { x: 0.8, y: 2.2, w: 8.5, h: 1.2, fontFace: HEAD, fontSize: 38, bold: true, color: C.paper, margin: 0 });
  s.addText("How much of the shell do you want to own?", { x: 0.8, y: 3.5, w: 8.5, h: 0.7, fontFace: HEAD, fontSize: 18, italic: true, color: "94A3B8", margin: 0 });

  const tiers = [
    { n: "L1", t: "Use the wrapper",   d: "ptr_app() + ui_text" },
    { n: "L2", t: "Embed in your UI",  d: "ptr_server_state + register_*" },
    { n: "L3", t: "Own the render",    d: "ptr_extract_plot + ptr_gg_extra" },
  ];
  const x0 = 0.8, y0 = 4.3, cw = 2.9, ch = 0.9, gap = 0.15;
  tiers.forEach((t, i) => {
    const x = x0 + i * (cw + gap);
    s.addShape(pres.shapes.RECTANGLE, { x, y: y0, w: cw, h: ch, fill: { color: "1E293B" }, line: { color: C.accent, width: 0.75 } });
    s.addText(t.n, { x: x + 0.15, y: y0 + 0.08, w: 0.6, h: 0.3, fontFace: BODY, fontSize: 11, bold: true, color: C.accent, charSpacing: 2, margin: 0 });
    s.addText(t.t, { x: x + 0.15, y: y0 + 0.32, w: cw - 0.25, h: 0.3, fontFace: HEAD, fontSize: 15, bold: true, color: C.paper, margin: 0 });
    s.addText(t.d, { x: x + 0.15, y: y0 + 0.58, w: cw - 0.25, h: 0.3, fontFace: MONO, fontSize: 10, color: "CBD5E1", margin: 0 });
  });
}

// ---------- Slide 15: L1 ----------
{
  const s = pres.addSlide();
  s.background = { color: C.paper };
  titleBar(s, "LEVEL 1", "One-liner wrapper with UI text overrides");
  bulletBlock(s, [
    { text: "ptr_app() / ptr_app_bslib() — the shipped wrappers.", bold: true },
    "Pass ui_text to retitle shell, buttons, per-param and per-layer labels.",
    "No Shiny code written — just a nested list of copy overrides.",
    "Good fit when you want a polished single-purpose app.",
  ], { x: 0.5, y: 1.7, w: 6, h: 2.2 });
  codeBlock(s,
`ptr_app_bslib(formula, ui_text = list(
  shell  = list(title = list(label = "Iris Explorer")),
  params = list(x = list(var = list(label = "X-axis column"))),
  layers = list(geom_point = list(num = list(size = list(
    label = "Point size", help = "Typical: 1 – 5"))))
))`,
    { x: 0.5, y: 3.85, w: 6, h: 1.35 }
  );
  chunkRef(s, "extensibility-level-1", 296);
  footer(s, "Part 2 · level 1");
}

// ---------- Slide 16: L2 ----------
{
  const s = pres.addSlide();
  s.background = { color: C.paper };
  titleBar(s, "LEVEL 2", "Embed into your own Shiny layout");
  bulletBlock(s, [
    { text: "ptr_build_ids() gives you control over every Shiny id.", bold: true },
    "Drop ptr_input_ui() / ptr_output_ui() wherever in your layout.",
    "ptr_register_{draw,plot,error,code}() wire the runtime in.",
    "Your own reactives can read ptr_state$runtime() — side panels, status.",
  ], { x: 0.5, y: 1.7, w: 6, h: 2.2 });
  codeBlock(s,
`ptr_state <- ptr_server_state(formula, ids = custom_ids)
ptr_setup_controls(input, output, ptr_state)
ptr_register_draw (input,         ptr_state)
ptr_register_plot (output,        ptr_state)
ptr_register_error(output,        ptr_state)
ptr_register_code (output,        ptr_state)`,
    { x: 0.5, y: 3.85, w: 6, h: 1.35 }
  );
  chunkRef(s, "extensibility-level-2", 330);
  footer(s, "Part 2 · level 2");
}

// ---------- Slide 17: L3 ----------
{
  const s = pres.addSlide();
  s.background = { color: C.paper };
  titleBar(s, "LEVEL 3", "Own the render path");
  bulletBlock(s, [
    { text: "Swap in your own renderPlot() (or renderGirafe / renderPlotly).", bold: true },
    "ptr_extract_plot(runtime) hands you the built ggplot object.",
    "Append layers freely — theme_minimal, scale_color_brewer, etc.",
    "Wrap additions in ptr_gg_extra() so they also appear in the code pane.",
  ], { x: 0.5, y: 1.7, w: 6, h: 2.2 });
  codeBlock(s,
`output$outputPlot <- renderPlot({
  plot_obj <- ptr_extract_plot(ptr_state$runtime())
  plot_obj + ptr_gg_extra(ptr_state,
    theme_minimal(base_size = 16),
    scale_color_brewer(palette = "Dark2"))
})`,
    { x: 0.5, y: 3.85, w: 6, h: 1.35 }
  );
  chunkRef(s, "extensibility-level-3", 387);
  footer(s, "Part 2 · level 3");
}

// ---------- Slide 18: ggiraph ----------
{
  const s = pres.addSlide();
  s.background = { color: C.paper };
  titleBar(s, "EMBEDDING · ggiraph", "Interactive SVG via renderGirafe");
  bulletBlock(s, [
    "Swap plotOutput for ggiraph::girafeOutput in the UI.",
    "Use renderGirafe and call girafe(code = print(plot_obj), ...).",
    "Add opts_hover / opts_selection for hover and selection behaviour.",
    "Works with geom_point_interactive — tooltip, data_id as aes mappings.",
  ], { x: 0.5, y: 1.7, w: 6, h: 3.2 });
  chunkRef(s, "unnamed (ggiraph)", 452);
  footer(s, "Part 2 · ggiraph embedding");
}

// ---------- Slide 19: plotly ----------
{
  const s = pres.addSlide();
  s.background = { color: C.paper };
  titleBar(s, "EMBEDDING · plotly", "Zero-effort interactivity via ggplotly");
  bulletBlock(s, [
    "plotlyOutput + renderPlotly on the Shiny side.",
    "ggplotly(plot_obj) converts any ggpaintr plot to a plotly widget.",
    "No changes to the formula — interactivity comes from the render path.",
    "Trade-off: less control than ggiraph, but one line of conversion.",
  ], { x: 0.5, y: 1.7, w: 6, h: 3.2 });
  chunkRef(s, "unnamed (plotly)", 507);
  footer(s, "Part 2 · plotly embedding");
}

// ---------- Slide 20: Part 3 cover ----------
{
  const s = pres.addSlide();
  s.background = { color: C.ink };
  s.addShape(pres.shapes.RECTANGLE, { x: 0, y: 0, w: 0.18, h: H, fill: { color: C.accent }, line: { type: "none" } });
  s.addText("PART 3", { x: 0.8, y: 1.8, w: 8, h: 0.4, fontFace: BODY, fontSize: 14, bold: true, color: C.accent, charSpacing: 5, margin: 0 });
  s.addText("Custom placeholders", { x: 0.8, y: 2.2, w: 8.5, h: 1.2, fontFace: HEAD, fontSize: 40, bold: true, color: C.paper, margin: 0 });
  s.addText("Teach ggpaintr new keywords with build_ui + resolve_expr.", { x: 0.8, y: 3.5, w: 8.5, h: 0.7, fontFace: HEAD, fontSize: 18, italic: true, color: "94A3B8", margin: 0 });
}

// ---------- Slide 21: 3.1 color ----------
{
  const s = pres.addSlide();
  s.background = { color: C.paper };
  titleBar(s, "EXAMPLE 3.1", "A color placeholder with colourpicker");
  bulletBlock(s, [
    { text: "ptr_define_placeholder(keyword = \"color\", ...).", bold: true },
    "build_ui: colourpicker::colourInput — one widget per occurrence.",
    "resolve_expr: returns the chosen hex as a quoted string literal.",
    "copy_defaults templates the label; {param} interpolates fill / color.",
  ], { x: 0.5, y: 1.7, w: 6, h: 2.2 });
  codeBlock(s,
`color_placeholder <- ptr_define_placeholder(
  keyword      = "color",
  build_ui     = function(id, copy, meta, context) { ... },
  resolve_expr = function(value, meta, context)  { ... },
  copy_defaults = list(label = "Pick {param} colour"))`,
    { x: 0.5, y: 3.85, w: 6, h: 1.35 }
  );
  chunkRef(s, "color-placeholder", 578);
  footer(s, "Part 3 · example 3.1");
}

// ---------- Slide 22: 3.2 date ----------
{
  const s = pres.addSlide();
  s.background = { color: C.paper };
  titleBar(s, "EXAMPLE 3.2", "A date placeholder with param-aware copy");
  bulletBlock(s, [
    { text: "shiny::dateInput as the widget, as.Date(\"...\") in generated code.", bold: true },
    "ptr_merge_ui_text layers: defaults → params → layers (most specific wins).",
    "Same mechanism that drives built-in placeholders — nothing special.",
    "Useful anywhere xintercept, vlines, or date filters are natural.",
  ], { x: 0.5, y: 1.7, w: 6, h: 2.2 });
  codeBlock(s,
`date_ui_text <- ptr_merge_ui_text(list(
  defaults = list(date = list(label = "Pick any date")),
  params   = list(xintercept = list(date = list(
              label = "Reference date"))),
  layers   = list(geom_vline = list(date = list(xintercept =
              list(help = "Cut-off for the vertical guide."))))),
  placeholders = date_placeholders)`,
    { x: 0.5, y: 3.85, w: 6, h: 1.4 }
  );
  chunkRef(s, "date-placeholder", 631);
  footer(s, "Part 3 · example 3.2");
}

// ---------- Slide 23: Wrap-up ----------
{
  const s = pres.addSlide();
  s.background = { color: C.ink };
  s.addShape(pres.shapes.RECTANGLE, { x: 0, y: 0, w: 0.18, h: H, fill: { color: C.accent }, line: { type: "none" } });
  s.addText("WRAP-UP", { x: 0.8, y: 0.7, w: 8, h: 0.4, fontFace: BODY, fontSize: 13, bold: true, color: C.accent, charSpacing: 5, margin: 0 });
  s.addText("The through-line", { x: 0.8, y: 1.05, w: 8.5, h: 0.8, fontFace: HEAD, fontSize: 34, bold: true, color: C.paper, margin: 0 });

  const points = [
    { h: "Formula first",   d: "One string is the whole UI contract." },
    { h: "Escape hatches",  d: "expr + custom placeholders cover anything ggplot." },
    { h: "Three tiers",     d: "Wrapper → embed → full render-path control." },
    { h: "Round-trips",     d: "Plot and code pane stay in sync automatically." },
  ];
  const x0 = 0.8, y0 = 2.3, cw = 4.3, ch = 1.15, gap = 0.2;
  points.forEach((p, i) => {
    const col = i % 2, row = Math.floor(i / 2);
    const x = x0 + col * (cw + gap);
    const y = y0 + row * (ch + gap);
    s.addShape(pres.shapes.RECTANGLE, { x, y, w: cw, h: ch, fill: { color: "1E293B" }, line: { color: C.accent, width: 0.5 } });
    s.addShape(pres.shapes.RECTANGLE, { x, y, w: 0.08, h: ch, fill: { color: C.accent }, line: { type: "none" } });
    s.addText(p.h, { x: x + 0.25, y: y + 0.15, w: cw - 0.35, h: 0.4, fontFace: HEAD, fontSize: 17, bold: true, color: C.paper, margin: 0 });
    s.addText(p.d, { x: x + 0.25, y: y + 0.55, w: cw - 0.35, h: 0.55, fontFace: BODY, fontSize: 13, color: "CBD5E1", margin: 0, valign: "top" });
  });

  s.addText("Questions?", {
    x: 0.8, y: H - 0.9, w: 8.5, h: 0.5,
    fontFace: HEAD, fontSize: 22, italic: true, color: C.accent, margin: 0,
  });
}

pres.writeFile({ fileName: "/Users/willju/Research/ggpaintr/dev/demo/demo_slides.pptx" })
  .then(f => console.log("wrote:", f));
