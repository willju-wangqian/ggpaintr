# Behavior: user-supplied CSS hook (`css =`) on the public UI entry points,
# plus the regression guard that the split module form ships ggpaintr.css.

render_html <- function(tag) {
  paste(as.character(tag), collapse = "\n")
}

test_that("ptr_user_css_assets(NULL) returns NULL", {
  expect_null(ptr_user_css_assets(NULL))
})

test_that("a valid .css file becomes a linked stylesheet served as a resource", {
  dir <- withr::local_tempdir()
  f <- file.path(dir, "overrides.css")
  writeLines(".ptr-app { background: hotpink; }", f)

  tags <- ptr_user_css_assets(f)
  expect_s3_class(tags, "shiny.tag.list")
  html <- render_html(tags)
  expect_match(html, "<link[^>]+rel=\"stylesheet\"")
  expect_match(html, "/overrides\\.css\"")

  # the file's directory was registered as a static resource path.
  # `mustWork = FALSE` keeps normalizePath quiet about unrelated, already
  # deleted temp dirs that other test files left in the process-global
  # Shiny resource registry; `dir` itself still resolves on both sides.
  expect_true(normalizePath(dir, mustWork = FALSE) %in%
                vapply(shiny::resourcePaths(),
                       normalizePath, character(1), mustWork = FALSE))
})

test_that("multiple files in one directory share one resource prefix", {
  dir <- withr::local_tempdir()
  a <- file.path(dir, "a.css"); writeLines(".a{}", a)
  b <- file.path(dir, "b.css"); writeLines(".b{}", b)

  html <- render_html(ptr_user_css_assets(c(a, b)))
  hrefs <- regmatches(html, gregexpr("href=\"[^\"]+\"", html))[[1]]
  hrefs <- sub("href=\"", "", sub("\"$", "", hrefs))
  prefixes <- unique(dirname(hrefs))
  expect_length(prefixes, 1L)
  # vector order is preserved
  expect_match(hrefs[[1]], "a\\.css$")
  expect_match(hrefs[[2]], "b\\.css$")
})

test_that("files in different directories get distinct resource prefixes", {
  d1 <- withr::local_tempdir(); a <- file.path(d1, "x.css"); writeLines(".x{}", a)
  d2 <- withr::local_tempdir(); b <- file.path(d2, "y.css"); writeLines(".y{}", b)

  html <- render_html(ptr_user_css_assets(c(a, b)))
  hrefs <- regmatches(html, gregexpr("href=\"[^\"]+\"", html))[[1]]
  prefixes <- unique(dirname(sub("href=\"", "", sub("\"$", "", hrefs))))
  expect_length(prefixes, 2L)
})

test_that("missing file path is an error", {
  expect_error(ptr_user_css_assets(file.path(tempdir(), "nope.css")),
               "not found")
})

test_that("non-.css extension is an error", {
  dir <- withr::local_tempdir()
  f <- file.path(dir, "styles.txt"); writeLines("body{}", f)
  expect_error(ptr_user_css_assets(f), "\\.css")
})

test_that("ptr_app() links the bundled stylesheet before the user stylesheet", {
  dir <- withr::local_tempdir()
  f <- file.path(dir, "mine.css"); writeLines(".ptr-app{}", f)

  parts <- ptr_app_components(
    "ggplot(mtcars) + geom_point(aes(x = ppVar, y = ppVar))",
    css = f
  )
  # ggpaintr.css ships as an htmlDependency now; render deps + body so the
  # head injection is visible alongside the body-level user <link>.
  html <- render_with_deps(parts$ui)
  pos_bundled <- regexpr('/ggpaintr\\.css"', html)
  pos_user <- regexpr("/mine\\.css", html)
  expect_gt(pos_bundled, 0L)
  expect_gt(pos_user, 0L)
  expect_lt(pos_bundled, pos_user)
})

test_that("module UI ships ggpaintr.css (regression)", {
  # Post-redesign the L2 split is gone; the self-contained ptr_ui()
  # is what carries the bundled stylesheet (bare L3 pieces are assetless
  # by design — orthogonality contract, covered in test-rewrite-app).
  ui <- ptr_ui("ggplot(mtcars) + geom_point(aes(x = var))", "p")
  expect_match(render_with_deps(ui), '/ggpaintr\\.css"')
})
