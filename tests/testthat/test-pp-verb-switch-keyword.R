# ADR 0021 / PLAN-02 — ppVerbSwitch is registered as a built-in
# keyword and as a structural-keyword registry entry.
#
# SC-7: `ptr_builtin_keywords()` includes "ppVerbSwitch" so
# `ptr_clear_placeholder()` refuses to drop it.
# SC-8: After a fresh `devtools::load_all()` the structural-keyword
# registry knows about "ppVerbSwitch" (registered in
# `ptr_register_builtins()`).

test_that("SC-7: ppVerbSwitch is listed in ptr_builtin_keywords()", {
  expect_true("ppVerbSwitch" %in% ptr_builtin_keywords())
})

test_that("SC-8: ppVerbSwitch is in ptr_registry_keywords() after load_all", {
  expect_true("ppVerbSwitch" %in% ptr_registry_keywords())
})
