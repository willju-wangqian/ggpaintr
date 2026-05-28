# J12 journey scaffolding (NOT a vignette chunk): smallest fixture for the
# non-default ptr_spec emission contract. Absorbs the testServer-level
# assertion previously pinned at test-ptr-spec-emission.R:160 (deleted as
# part of the J12 browser-faithfulness merge — see
# dev/audit/audit-test-fidelity-v8-j12-browser-faithfulness-2026-05-27-
# 2337.html).
#
# Two `ppVar` consumers at aes positions x and y; the journey sets both,
# clicks Update Plot, toggles ptr_code_mode to "spec", and asserts the
# `ptr_spec <- list(...)` block contains the bare ids with the picked column
# names. The pre-fix bug would either emit the formula text alongside (ADR
# 0022 audience-split regression) or omit the spec entries entirely.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)

ptr_app("ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point()")
