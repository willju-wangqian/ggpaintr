# MRE — ADR 0025 §3b consumer-seeding bug (first upload with no boot data)

**Status:** ~~reproduced~~ **FIXED** (2026-05-29) on `ppUpload-bug`. `mre.R` now reports "no bug -- picker came up unselected"; it is kept as a manual regression demonstrator. The fix: `consumer_clear_for_new_source()` (R/paintr-server.R) clears the picker on a first render whose data is an actual upload (`uploaded` datapath present), gated on `is.null(seed)` so a boot-time `spec=` seed still wins. Originally reproduced @ `4a469f1` + the F3/F2 fixes in the working tree.

**This is NOT the F3 pollution bug.** It is a *separate* consumer-seeding defect in the same area as the boot-oracle / super-3 seeding campaign. It was surfaced (not introduced) by making `adr15-consumer-binding` a clean `ppUpload()` upload test — the old test masked it by typing a shortcut afterward (a rename → ADR-0025 contract-(ii) clear).

## The contract (ADR 0025 §3b, locked 2026-05-28)

> A consumer placeholder seeds its formula-side default **only at boot, and only against the data present at boot**. Once the app is running, any new data source … clears the consumer picker. The picker comes up **populated … but with no selection**.

So: `ppUpload()` has **no data at boot** → the default must **not** seed. The first upload happens while the app is running → the picker must come up **populated but UNSELECTED**.

## What actually happens

After the first upload, the x picker (`ppVar(mpg)`) comes up with **`mpg` already selected** — the formula default was carried onto brand-new uploaded data.

## Reproduce it

Headless (one command, from the repo root):

```
Rscript .scratch/ppupload-3b-seeding-mre/mre.R
```

Expected output ends with:

```
x-picker selection after upload: "mpg"
VERDICT : BUG REPRODUCED -- the formula default 'mpg' was auto-selected.
```

Interactive (eyeball it in a browser):

```r
shiny::runApp(".scratch/ppupload-3b-seeding-mre")
# 1. Note: no plot/columns yet (no data at boot).
# 2. Upload .scratch/ppupload-3b-seeding-mre/data.csv to the file input.
# 3. Look at the layer's x-aes column picker: it is pre-selected to "mpg".
#    Per ADR 0025 §3b it should be populated (mpg/wt/hp offered) but BLANK.
```

`data.csv` columns: `mpg, wt, hp`.

## Contrast (why this is the *no-boot-data* path specifically)

If the formula instead boots **with** data — e.g. `ppUpload(mtcars)` (env-shortcut seeds `mtcars` at boot) — then seeding the default at boot is *correct* (author-known data, §3b allows it), and a subsequent upload correctly clears it. The bug is specific to **no data at boot, then a first upload**.

## Files

- `app.R` — the minimal app (`geom_point(data = ppUpload(), aes(x = ppVar(mpg), y = ppVar(wt)))`).
- `data.csv` — upload payload (mpg, wt, hp).
- `mre.R` — headless shinytest2 reproduction with a printed verdict.
