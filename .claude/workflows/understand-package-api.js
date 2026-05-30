export const meta = {
  name: 'understand-package-api',
  description: 'Build verified public-API + extensibility-level usage knowledge for an R/Shiny package: discover the API surface, read source, build & browser-run example apps, verify each finding, write it into the harvest pipeline',
  whenToUse: 'When you want a verified, source-grounded knowledge base of a package\'s public API and how to USE it (not bug mechanics) — emitted as harvest findings under .claude/harvest-findings/ ready for /process-finding.',
  phases: [
    { title: 'Discover', detail: 'resolve repo/head/ts + build API-surface clusters from the export manifest + source, gap-aware vs existing INDEX.md' },
    { title: 'API Sweep', detail: 'one reader agent per cluster, each independently verified against source' },
    { title: 'Build & Run', detail: 'per extensibility level: build a fresh example app + boot fixtures via AppDriver (capped concurrency)' },
    { title: 'Synthesize', detail: 'dedup vs existing INDEX.md, assign scope' },
    { title: 'Scribe', detail: 'write raw_conversation stamps + raw_knowledge JSON (harvest schema)' },
  ],
}

// ----------------------------------------------------------------------------
// CONFIG (every field optional — args overrides a sensible default). Defaults
// reproduce the ggpaintr run that authored this workflow. Parameterize by:
//   repo, head, ts            — usually let Discover resolve these (pass to pin)
//   sourceGlob                — what source files the readers sweep
//   exportManifest            — file listing the public surface (R: NAMESPACE)
//   fixturesDir               — app-dir fixtures the browser phase boots
//   levels                    — [] to skip the browser phase entirely
//   topic                     — slug used in the output filenames
// ----------------------------------------------------------------------------
const A = args || {}
const CFG = {
  repo: A.repo || null,
  head: A.head || null,
  ts: A.ts || null,
  sourceGlob: A.sourceGlob || 'R/paintr-*.R',
  exportManifest: A.exportManifest || 'NAMESPACE',
  fixturesDir: A.fixturesDir || 'tests/testthat/fixtures/vignette-apps',
  topic: A.topic || 'public-api-understanding',
  nClusters: A.nClusters || 13,
  levels: A.levels || DEFAULT_LEVELS(),
}
const AGENT = 'general-purpose'

function DEFAULT_LEVELS() {
  return [
    { key: 'browser-l1', level: 'L1', fixtures: 'app-basic, app-grid-shared-added', tests: 'test-j11-app-basic-journey.R, test-e2e-vignette-examples-shinytest2.R, test-app-grid-layout.R',
      task: `LEVEL L1 (turn-key entry point). (a) Boot an existing simple fixture via AppDriver; show the Data subtab, set a column picker, click Draw, ASSERT the plot output and generated-code output are non-empty. (b) BUILD A FRESH minimal example app-dir that calls the L1 turn-key entry with a one-line formula over a supplied data frame; boot, exercise, assert it draws. Report whether one line yields a working app, what the default UI contains, and any usage gotcha.` },
    { key: 'browser-l2', level: 'L2', fixtures: 'module-app, l2-shared', tests: 'test-module-server-shared.R, test-module-ui.R',
      task: `LEVEL L2 (custom assembly inside a user-owned shinyApp). (a) Boot an existing L2 fixture; exercise + assert it draws. (b) BUILD A FRESH example where the USER writes their own ui + server function and drops the package's ui()+server() in (read the fixture app.R + tests to learn the exact calling convention, the id arg, whether server is called bare or under moduleServer). Boot, exercise, assert. Report the L2 wiring contract and how L2 differs from L1.` },
    { key: 'browser-l3', level: 'L3', fixtures: 'l3-pieces, l3-plotly, l3-gg-extra', tests: 'test-ui-pieces.R',
      task: `LEVEL L3 (fully custom render from UI-only pieces + extract accessors). (a) Boot an existing L3 fixture; exercise + assert hand-assembled pieces render and a custom plot draws. (b) BUILD A FRESH example that hand-assembles a layout from the UI pieces, calls the server, and uses the extract-plot accessor to render in a custom output. Read the L3 fixtures' app.R + tests for the extraction contract. Boot, exercise, assert. Report the L3 custom-render contract.` },
    { key: 'browser-shared', level: 'shared', fixtures: 'single-instance-shared, grid-shared-partition, app-grid-shared-added', tests: 'test-shared-server.R, test-shared-partition.R',
      task: `SHARED coordinator (a control shared across multiple formulas/panels). (a) Boot an existing shared fixture; change the SHARED control once and ASSERT it affects the plot(s); assert the shared panel renders. (b) BUILD A FRESH example with a control declared shared across two formulas/panels; boot, change the shared control, assert both respond. Read the shared fixtures' app.R + tests for the shared declaration syntax + selectors. Report the USER-level contract for sharing a control and when to use it.` },
  ]
}

// ---- schemas ----------------------------------------------------------------
const PLAN_SCHEMA = {
  type: 'object', additionalProperties: false,
  properties: {
    repo: { type: 'string' }, head: { type: 'string' }, ts: { type: 'string' },
    gap: { type: 'string' },
    clusters: { type: 'array', items: {
      type: 'object', additionalProperties: false,
      properties: { key: { type: 'string' }, title: { type: 'string' }, body: { type: 'string' } },
      required: ['key', 'title', 'body'],
    } },
  },
  required: ['repo', 'head', 'ts', 'gap', 'clusters'],
}

const FINDINGS_SCHEMA = {
  type: 'object', additionalProperties: false,
  properties: { findings: { type: 'array', items: {
    type: 'object', additionalProperties: false,
    properties: {
      id: { type: 'string' }, claim: { type: 'string' }, detail: { type: 'string' },
      scope: { type: 'string', enum: ['durable', 'campaign', 'decision'] },
      source: { type: 'string' }, tags: { type: 'string' }, runtime_grounded: { type: 'boolean' },
    },
    required: ['id', 'claim', 'detail', 'scope', 'source', 'runtime_grounded'],
  } } },
  required: ['findings'],
}

const VERIFIED_SCHEMA = {
  type: 'object', additionalProperties: false,
  properties: { findings: { type: 'array', items: {
    type: 'object', additionalProperties: false,
    properties: {
      id: { type: 'string' }, claim: { type: 'string' }, detail: { type: 'string' },
      scope: { type: 'string', enum: ['durable', 'campaign', 'decision'] },
      source: { type: 'string' }, tags: { type: 'string' }, runtime_grounded: { type: 'boolean' },
      verdict: { type: 'string', enum: ['verified', 'contradicted', 'unconfirmable'] },
      verification: { type: 'string' }, checked_refs: { type: 'array', items: { type: 'string' } },
    },
    required: ['id', 'claim', 'detail', 'scope', 'source', 'runtime_grounded', 'verdict', 'verification', 'checked_refs'],
  } } },
  required: ['findings'],
}

const STAMP_BAR = `
THE BAR (stamp only keepers): non-obvious, derived from reading the ACTUAL source, reusable. An invariant, a
control-flow/topology fact, a "how to use it correctly" gotcha, a "why it's built this way", a confirmed/denied
hypothesis. Do NOT stamp trivia, restated docs, or transient mechanics. Each finding: a ONE-sentence standalone
'claim', an optional multi-line 'detail', a 'scope' (durable=architecture/invariant worth project memory |
campaign=in-flight working knowledge | decision=feeds an ADR), and a precise 'source' string of EXACT file:line refs
you actually read. Use stable kebab-case ids. Aim for 3-6 high-quality findings per cluster (quality over count).`

// ---- Phase: Discover --------------------------------------------------------
phase('Discover')
const plan = await agent(
  `You are the discovery planner for a package-understanding knowledge harvest. Work in the current repo.

1. Resolve and RETURN: repo (absolute root via \`pwd\` / git rev-parse --show-toplevel), head (\`git rev-parse HEAD\`),
   ts (\`date +%Y-%m-%d-%H%M\`).
2. Read the export manifest (${CFG.exportManifest}) to list the PUBLIC API surface, and skim the source files matching
   ${CFG.sourceGlob} (use get_symbols_overview / grep for an inventory — do NOT read them whole).
3. Read the existing knowledge index .claude/harvest-findings/INDEX.md (if present) to see what is ALREADY covered.
   Write a 'gap' string: what the existing index over-covers vs the USAGE/PUBLIC-API/COMPOSITION story that is thin or
   absent. Readers must target the gap, not re-derive what is indexed.
4. Partition the public surface into about ${CFG.nClusters} reader CLUSTERS. Each cluster = {key (kebab), title, body}
   where 'body' is a focused instruction naming the exact functions + source files to read and what to characterize
   (what each does, how a user calls it, args/defaults, how it composes, which extensibility level it belongs to).
   Cover the WHOLE exported surface across the clusters; group by cohesive subsystem (entry points, the token/keyword
   surface, the custom-extension registry, the L2/L3 server+ui split, custom-render accessors, the shared coordinator,
   config/copy, safety, the transform pipeline, introspection, any assist/LLM surface, etc. — adapt to THIS package).

Return repo/head/ts/gap/clusters via the StructuredOutput tool.`,
  { schema: PLAN_SCHEMA, agentType: AGENT, label: 'discover', phase: 'Discover' },
)

const REPO = CFG.repo || plan.repo
const HEAD = CFG.head || plan.head
const TS = CFG.ts || plan.ts
const GAP = plan.gap
const CLUSTERS = plan.clusters
log(`discover: repo=${REPO} head=${HEAD.slice(0, 7)} ts=${TS} | ${CLUSTERS.length} clusters | levels=${CFG.levels.length}`)

const GAP_BLOCK = `
PROJECT GAP (target this; do not re-derive what the index already covers):
${GAP}`

const BOOT_PATTERN = `
HOW TO BOOT AN APP IN A REAL BROWSER (the ONLY faithful way — reading source is NOT enough for runtime claims):
1. The app must be an app DIRECTORY whose app.R FIRST LINE is:
     pkgload::load_all(Sys.getenv("GGP_PKG"), quiet=TRUE, helpers=FALSE, attach_testthat=FALSE)
   so the child R process loads the DEV source, not a stale system install.
2. Set env GGP_PKG to the repo root: ${REPO}
3. Boot: app <- shinytest2::AppDriver$new(app_dir, name="probe", load_timeout=60000, timeout=30000); app$wait_for_idle(15000)
4. RULES (each cost a debugging cycle once):
   - NEVER call app$get_values() (500s on custom-renderer apps). Assert via app$get_html("#id") / app$get_value(output="id").
   - The app re-renders only on the Update/Draw click. Use app$set_inputs(id=value, wait_=FALSE) for EVERY placeholder
     set, THEN click the draw/update button.
   - source/consumer var pickers live in a renderUI under a layer's Data subtab and are NOT bound until shown: set the
     source/consumer input, set <layer>_subtab="Controls", wait_for_idle(), THEN set the var pickers.
   - Wrap AppDriver$new() in suppressWarnings().
5. DISCOVER the real input ids / draw-button id / assertion selectors by READING the matching test file + the fixture
   app.R BEFORE writing your probe. Do not guess ids. Run with: Rscript probe.R. If chromote/Chrome is missing the run
   SKIPS — report that honestly rather than claiming a pass.`

function readerPrompt(c) {
  return `You are a source-truth reader for an R package (repo ${REPO}, HEAD ${HEAD}).
${GAP_BLOCK}

YOUR CLUSTER: ${c.title}
${c.body}

Read the ACTUAL source with Read/Grep. Ground every claim in lines you opened. You MAY run a quick non-interactive
Rscript probe, but you do NOT need a browser (the browser phase covers runtime). Set runtime_grounded=false on all
your findings.
${STAMP_BAR}
Return your findings via the StructuredOutput tool.`
}

function verifyPrompt(title, findingsJson) {
  return `You are an INDEPENDENT verifier for package knowledge findings (repo ${REPO}, HEAD ${HEAD}). You did NOT write
these. For EACH finding (${title}), open the cited source refs (Read/Grep at the exact file:lines) and adversarially
check the claim against current code:
 - verdict="verified" if the source supports the claim today;
 - verdict="contradicted" if the source disproves it (then CORRECT the claim/detail to what the source actually says,
   and set verdict back to "verified" if your corrected claim is now source-true; keep "contradicted" only if the
   finding's substance is wrong, not merely a stale line number);
 - verdict="unconfirmable" if the refs don't exist or don't bear on the claim.
Refine vague claims to be precise and standalone. Populate checked_refs with the refs you actually opened. Keep
id/scope/tags/runtime_grounded. Do not pass a claim you could not ground.

FINDINGS TO VERIFY (JSON):
${findingsJson}
Return the verified findings via the StructuredOutput tool.`
}

function browserPrompt(b) {
  return `You are a runtime-truth builder for an R/Shiny package (repo ${REPO}, HEAD ${HEAD}). The rule is absolute: a
boot/runtime/reactive claim is UNVERIFIED until you RUN the app in a real browser. You will RUN existing example apps
AND BUILD a fresh example app with the public API, proving each works (or honestly reporting where it breaks).
${GAP_BLOCK}
${BOOT_PATTERN}

YOUR TASK:
${b.task}

Representative existing fixtures live under ${REPO}/${CFG.fixturesDir} (candidates: ${b.fixtures}). Matching tests to
read for input ids / draw-button / selectors: ${b.tests}.

Work under a scratch dir you create (e.g. /tmp/pkg-${b.key}); never edit anything under ${REPO}/R or the repo fixtures.
Capture the actual Rscript stdout proving the boot + draw + assertions. Emit findings about how apps at this level
boot, compose, and behave at RUNTIME, and whether your fresh example worked end-to-end. Set runtime_grounded=true on
findings you actually observed in a browser run; false on purely structural ones. Cite both the app behavior AND the
code refs that explain it in 'source'.
${STAMP_BAR}
Return your findings via the StructuredOutput tool.`
}

// ---- Phase: API Sweep (reader -> independent verify, pipelined) -------------
phase('API Sweep')
const api = await pipeline(
  CLUSTERS,
  (c) => agent(readerPrompt(c), { label: `read:${c.key}`, phase: 'API Sweep', schema: FINDINGS_SCHEMA, agentType: AGENT })
    .then((r) => ({ key: c.key, cluster: c, findings: (r && r.findings) || [] })),
  (prev) => prev.findings.length === 0
    ? prev
    : agent(verifyPrompt(prev.cluster.title, JSON.stringify(prev.findings)), { label: `verify:${prev.key}`, phase: 'API Sweep', schema: VERIFIED_SCHEMA, agentType: AGENT })
        .then((v) => ({ key: prev.key, findings: (v && v.findings) || [] })),
)

// ---- Phase: Build & Run (capped at <=4 concurrent browser boots) -----------
let browser = []
if (CFG.levels.length > 0) {
  phase('Build & Run')
  browser = await parallel(
    CFG.levels.map((b) => () =>
      agent(browserPrompt(b), { label: `run:${b.key}`, phase: 'Build & Run', schema: FINDINGS_SCHEMA, agentType: AGENT })
        .then((r) => {
          const f = (r && r.findings) || []
          if (f.length === 0) return { key: b.key, findings: [] }
          return agent(verifyPrompt(`browser ${b.level}`, JSON.stringify(f)), { label: `verify:${b.key}`, phase: 'Build & Run', schema: VERIFIED_SCHEMA, agentType: AGENT })
            .then((v) => ({ key: b.key, findings: (v && v.findings) || [] }))
        })),
  )
}

const allFindings = [...api, ...browser].filter(Boolean).flatMap((r) => r.findings)
log(`collected ${allFindings.length} verified findings (${CLUSTERS.length} reader + ${CFG.levels.length} browser clusters)`)

// ---- Phase: Synthesize (dedup vs existing index) ---------------------------
phase('Synthesize')
const synth = await agent(
  `You are the synthesis editor for a package knowledge harvest (repo ${REPO}, HEAD ${HEAD}). Below are ${allFindings.length}
verified findings from independent reader + browser agents. Read the EXISTING index at
${REPO}/.claude/harvest-findings/INDEX.md and the finding ids it lists. Produce the FINAL set:
 - DROP findings that merely restate a fact already in the index (keep new usage/API/composition facts even if they
   touch an indexed subsystem from a usage angle).
 - MERGE near-duplicates across clusters into one crisp finding (keep the better refs, union checked_refs).
 - Ensure every id is unique kebab-case; fix collisions.
 - Keep verdict/verification/checked_refs/runtime_grounded as given (do NOT re-verify). Drop any finding still
   verdict="unconfirmable" with empty checked_refs.
 - Assign scope sensibly (durable for API contracts/invariants; campaign only for genuinely in-flight specifics).
Aim for a clean, non-redundant set — do not pad, do not over-prune.

FINDINGS (JSON):
${JSON.stringify(allFindings)}
Return the final findings via the StructuredOutput tool.`,
  { label: 'synthesize', phase: 'Synthesize', schema: VERIFIED_SCHEMA, agentType: AGENT },
)
const finalFindings = (synth && synth.findings) || allFindings

// ---- Phase: Scribe ---------------------------------------------------------
phase('Scribe')
const rawConvPath = `.claude/harvest-findings/raw_conversation/${TS}-${CFG.topic}.md`
const rawKnowPath = `.claude/harvest-findings/raw_knowledge/${TS}.json`
const vCount = finalFindings.filter((f) => f.verdict === 'verified').length
const cCount = finalFindings.filter((f) => f.verdict === 'contradicted').length
const uCount = finalFindings.filter((f) => f.verdict === 'unconfirmable').length

const scribe = await agent(
  `You are the scribe. Write TWO files into the repo (root ${REPO}) from the FINAL findings JSON below. Use absolute
paths under ${REPO}. Touch nothing else.

FILE 1 — the unverified stamp corpus, at ${REPO}/${rawConvPath}:
A markdown file: a short header line, then for EACH finding one stamp block EXACTLY in this grammar (sentinels are
U+27E6 and U+27E7, open/close tags each on their own line):

⟦FINDING id=<id> status=derived-unverified scope=<scope> source="<source>"⟧
claim: <claim>
detail: <detail>
⟦/FINDING⟧

(Include tags="..." on the open tag only when the finding has a non-empty tags field.)

FILE 2 — the verified harvest JSON, at ${REPO}/${rawKnowPath}. Valid JSON with this exact top-level shape:
{
  "harvested_at": "${TS}",
  "corpus": "${rawConvPath} (workflow: understand-package-api; ${finalFindings.length} findings)",
  "counts": { "verified": ${vCount}, "contradicted": ${cCount}, "unconfirmable": ${uCount} },
  "findings": [ ... ]
}
Each "findings" element MUST have exactly these keys: id, claim, detail, scope, feature (""), tags (string, "" if none),
source, verdict, verification, checked_refs (array), origin_transcript ("${rawConvPath}"), origin_status
("derived-unverified"), labels ([] — leave EMPTY; /process-finding fills it), status ("fresh"), verified_at_commit
("${HEAD}"). Map each input finding to one output element, preserving id/claim/detail/scope/tags/source/verdict/
verification/checked_refs verbatim.

After writing, VALIDATE FILE 2 parses (Rscript -e 'invisible(jsonlite::fromJSON("${REPO}/${rawKnowPath}"))' OR
python3 -m json.tool "${REPO}/${rawKnowPath}" > /dev/null; confirm exit 0) and confirm the stamp file's ⟦FINDING count
equals ${finalFindings.length}. Fix and re-validate if either fails. Report: the two paths, the JSON-validation result,
and the stamp count.

FINAL FINDINGS (JSON):
${JSON.stringify(finalFindings)}`,
  { label: 'scribe', phase: 'Scribe', agentType: AGENT },
)

return {
  repo: REPO, head: HEAD, ts: TS,
  counts: { total: finalFindings.length, verified: vCount, contradicted: cCount, unconfirmable: uCount },
  rawConvPath, rawKnowPath, scribeReport: scribe,
  nextStep: 'Run /process-finding to assign labels + regenerate INDEX.md. Then review the contradicted/unconfirmable findings by hand.',
}
