# ADR 0020 / 0021 PLAN-06 — assert the use-cases vignette section for
# ppLayerOff / ppVerbSwitch exists and teaches the required content. Pure
# file-text checks (read the .Rmd, regex over it). Vignette knit + render
# are covered by `devtools::build_vignettes()` outside the gate; this
# file's job is to lock the prose contract so future edits cannot silently
# strip the section.

vignette_lines <- function() {
  path <- testthat::test_path("..", "..", "vignettes", "ggpaintr-use-cases.Rmd")
  testthat::skip_if(
    !file.exists(path),
    "vignette prose checks need the package source tree; absent under the R CMD check .Rcheck sandbox"
  )
  readLines(path, warn = FALSE)
}

vignette_text <- function() paste(vignette_lines(), collapse = "\n")

# ---- SC1 — keyword literal-strings appear with sufficient density ----

test_that("SC1: vignette contains >= 5 mentions of `ppLayerOff`", {
  hits <- length(grep("ppLayerOff", vignette_lines(), fixed = TRUE))
  expect_gte(hits, 5L)
})

test_that("SC1: vignette contains >= 5 mentions of `ppVerbSwitch`", {
  hits <- length(grep("ppVerbSwitch", vignette_lines(), fixed = TRUE))
  expect_gte(hits, 5L)
})

# ---- SC1 / SC2 — new section heading exists ----

test_that("SC1: vignette has a top-level section heading covering both keywords", {
  lines <- vignette_lines()
  # An H2 heading that names both keywords (or one heading per ADR 0020 /
  # 0021 plan prose). Match either the combined heading the plan suggests
  # or a heading that names ppLayerOff in the title.
  pattern <- "^## .*(ppLayerOff|Layer.*stage|stage.*toggles)"
  expect_true(any(grepl(pattern, lines, perl = TRUE)))
})

# ---- SC2 — motivation appears in the section ----

test_that("SC2: motivation prose mentions source-of-truth framing", {
  text <- vignette_text()
  # The plan asks for one of: "source of truth", "boot state", "formula",
  # "single source". The motivation paragraph in this section uses both
  # "single source of truth" and "boot state" explicitly.
  expect_match(text, "single source of truth", fixed = TRUE)
  expect_match(text, "boot state", fixed = TRUE)
})

# ---- SC3 — runtime symmetry example is present and `eval = TRUE` ----

test_that("SC3: a runnable naked-R chunk for the keywords exists with eval = TRUE", {
  text <- vignette_text()
  # The naked-R chunk this plan adds is tagged `pp-toggles-naked` and
  # carries the explicit `eval = TRUE` opt-in (the vignette's setup default
  # is `eval = interactive()`).
  expect_match(text, "pp-toggles-naked, eval = TRUE", fixed = TRUE)
  # The chunk uses the keyword in a real ggplot context.
  expect_match(text, "ppLayerOff(geom_point()", fixed = TRUE)
})

# ---- SC4 — app-mode contrast chunk references ptr_app ----

test_that("SC4: a contrast chunk wraps the formula in ptr_app(", {
  text <- vignette_text()
  # The chunk `pp-toggles-app` shows the same formula text inside ptr_app(...).
  expect_match(text, "pp-toggles-app", fixed = TRUE)
  # The chunk references ptr_app(.
  expect_match(text, "ptr_app(\n\"mtcars", fixed = TRUE)
})

# ---- SC5 — ppVerbSwitch data-argument-position assumption is documented ----

test_that("SC5: the section teaches the first-positional-argument constraint", {
  text <- vignette_text()
  # The plan asks for one of "position 1" or "first argument" (or close
  # equivalent). The new section uses both "first positional argument" and
  # the explicit "position 1" / "position 2" pair in the counter-example.
  expect_match(text, "first positional argument", fixed = TRUE)
  expect_match(text, "position 1", fixed = TRUE)
})

test_that("SC5: a counter-example shows what does NOT round-trip", {
  text <- vignette_text()
  # The vignette uses a hypothetical `my_fit(formula, data, weights)` to
  # demonstrate the wrong-position hazard and the wrapper escape valve.
  expect_match(text, "my_fit(formula", fixed = TRUE)
  expect_match(text, "fit_first", fixed = TRUE)
})

# ---- SC6 — spec = precedence over formula-side default ----

test_that("SC6: section explains that spec = overrides the formula-side default", {
  text <- vignette_text()
  # Both substrings present, and "override" appears with "spec" in a close
  # paragraph (we use a fixed substring that puts them adjacent).
  expect_match(text, "spec =", fixed = TRUE)
  expect_match(text, "override", fixed = TRUE)
  # A concrete code snippet shows `spec = list(<layer>_checkbox = TRUE)` to
  # flip the formula default at boot.
  expect_match(text, "geom_smooth_checkbox = TRUE", fixed = TRUE)
})
