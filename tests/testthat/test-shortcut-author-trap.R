# PLAN-01 / ADR 0025 §1 worked example #6 — when a custom source registers
# with `shortcut = TRUE` but the author's `build_ui` forgets to render a
# `textInput(node$shortcut_id, ...)`, translation and boot must succeed
# silently (no auto-injected widget, no warning). The framework stamps
# `node$shortcut_id` but the rendered tag list is whatever the author
# returned.

test_that("shortcut=TRUE without textInput is a silent author trap", {
  ptr_registry_clear()
  ptr_register_builtins()
  withr::defer({ ptr_registry_clear(); ptr_register_builtins() })

  ptr_define_placeholder_source(
    keyword = "ppNoTextInputSource",
    # Author opts into the shortcut surface but renders only a selectInput.
    build_ui = function(node, label = NULL, ...) {
      shiny::selectInput(node$id, label = label %||% "Dataset",
                         choices = c("mtcars", "iris"))
    },
    resolve_data = function(value, node, ...) {
      if (is.null(value) || !nzchar(value)) return(NULL)
      get(value, envir = asNamespace("datasets"))
    },
    shortcut = TRUE
  )

  formula <- "ppNoTextInputSource() |> ggplot(aes(x = ppVar('mpg'))) + geom_point()"

  expect_no_error(tree <- ptr_translate(formula))
  srcs <- find_nodes(tree, is_ptr_ph_data_source)
  expect_length(srcs, 1L)
  node <- srcs[[1L]]
  expect_equal(node$shortcut_id, paste0(node$id, "_shortcut"))

  # Boot server-side without error.
  expect_no_error({
    shiny::testServer(function(input, output, session) {
      ptr_server_internal(input, output, session, formula,
                          envir = globalenv())
    }, { session$flushReact() })
  })

  # The author's build_ui rendered only a selectInput; no input bound to
  # node$shortcut_id can appear in the rendered tag list.
  rendered <- ptr_registry_lookup("ppNoTextInputSource")$build_ui(
    list(id = node$id, shortcut_id = node$shortcut_id, keyword = node$keyword),
    label = "Pick"
  )
  html <- as.character(rendered)
  expect_false(any(grepl(paste0("id=\"", node$shortcut_id, "\""),
                         html, fixed = TRUE)))
})
