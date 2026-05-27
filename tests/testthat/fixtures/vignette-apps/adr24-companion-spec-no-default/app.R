# Boot scaffolding: see .claude/rules/testing.md "Browser e2e (shinytest2)".
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
# EXCEPT: ADR 0024 — spec= seeds a companion with NO `default=` on the
# placeholder. End-to-end chain: apply_spec_at_boot writes the companion
# id into state$spec_seed AND queues updateTextInput on onFlushed (post
# FINDING #8 fix) → companion input becomes "mtcars" → resolve observer
# fires → try_bind_source_default_resolved binds mtcars from envir (post
# ADR 0024 lift — pre-ADR-0024 the null-default guard rejected this).
ptr_app(
  "ppUpload() |> ggplot(aes(x = ppVar('col'))) + geom_bar()",
  spec = list(ggplot_0_ppUpload_NA_shortcut = "mtcars")
)
