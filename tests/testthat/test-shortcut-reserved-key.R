# PLAN-01 / ADR 0025 §1 worked example #4 — `shared = "shortcut"` is
# reserved because the suffix `"_shortcut"` names the source's env-shortcut
# sibling input. Translation must abort with a message naming the reserved
# key.

test_that("shared key 'shortcut' is rejected at translate time", {
  expect_error(
    ptr_translate(
      "ppUpload(shared = 'shortcut') |> ggplot(aes(x = ppVar('mpg'))) + geom_bar()"
    ),
    regexp = "Shared key 'shortcut' is reserved",
    class = "rlang_error"
  )
})

test_that("non-source placeholders binding shared = 'shortcut' also abort", {
  expect_error(
    ptr_translate(
      "ggplot(mtcars, aes(x = ppVar(mpg, shared = 'shortcut'))) + geom_point()"
    ),
    regexp = "Shared key 'shortcut' is reserved",
    class = "rlang_error"
  )
})

test_that("other shared keys still translate without error", {
  expect_silent(
    ptr_translate(
      "ggplot(mtcars, aes(x = ppVar(mpg, shared = 'ds'))) + geom_point()"
    )
  )
})
