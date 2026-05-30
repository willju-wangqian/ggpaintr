# ppUpload #7 follow-up — diagnosis probes (relocated from /tmp, 2026-05-29)

Throwaway AppDriver probes that runtime-verified the host-vs-single source/consumer asymmetry for the plan `dev/plans/0025-item7-followup-host-shared-consumer-clear/plan.html`. They are the **prototype for the regression test** (`tests/testthat/test-shared-host-source-reset.R`).

Each `app.R` boots dev source via `pkgload::load_all(GGP_PKG)`, defines an env dataset `typed_ds`, and instruments `consumer_upstream_source_state` / `consumer_clear_for_new_source` via `assignInNamespace` to `message()` the identity snapshot + clear decision into child stderr (captured by `app$get_logs()`).

Run (browser ON via shinytest2 setting NOT_CRAN; ~60s each):

```
Rscript .scratch/ppupload7-probes/shared/run-probe.R    # host coordinator path — stale column RIDES (the bug)
Rscript .scratch/ppupload7-probes/single/run-probe.R    # single-instance reference — picker clears to NULL
```

Key result (differential): identity = `sid#datapath#boundName`. Single-instance's `boundName` changes across the upload's two reactive beats so the trailing render re-clears (sticks); the host path runs at `state=NULL` so `boundName=""` always → identity stabilises → the trailing render reverts the clear. `single/discover.R` just dumps input ids (the consumer picker is suspended until its source is provided + the Controls subtab opens).

**Proxy trap baked into the test:** the consumer assertion MUST use a column present in BOTH datasets (`body_mass_g`) — a unique column is auto-dropped by `selectInput` and yields a false green.
