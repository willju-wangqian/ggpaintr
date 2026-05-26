# Boot scaffolding (NOT vignette code). Fixture for Cut 1.5 — when a new
# file is uploaded into a `ppUpload(default)` widget, the companion
# "Optional dataset name" textInput must auto-update to the uploaded
# file's basename. Pre-fix, the companion stayed pinned to its boot seed
# (`node$default %||% ""`), so generated code referenced the wrong name
# (e.g. "mtcars") while the in-memory frame was actually whatever the
# user uploaded (e.g. penguins), producing dishonest generated code that
# silently bound the uploaded frame under the stale name.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)

ptr_app(
  ppUpload(mtcars) |>
    ggplot(aes(x = ppVar(mpg))) +
    geom_point()
)
