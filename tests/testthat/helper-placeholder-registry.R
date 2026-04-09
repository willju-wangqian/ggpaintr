test_sales_data <- data.frame(
  date_col = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")),
  value = c(10, 12, 15)
)

test_date_formula <- paste(
  "ggplot(data = test_sales_data, aes(x = date_col, y = value)) +",
  "geom_line() +",
  "geom_vline(xintercept = date)"
)

test_sales_env <- function() {
  list2env(
    list(test_sales_data = test_sales_data),
    parent = asNamespace("ggpaintr")
  )
}

make_test_date_placeholder <- function() {
  ptr_define_placeholder(
    keyword = "date",
    build_ui = function(id, copy, meta, context) {
      ptr_attach_help(
        shiny::dateInput(id, copy$label),
        copy$help
      )
    },
    resolve_expr = function(value, meta, context) {
      if (is.null(value) || identical(value, "")) {
        return(ptr_missing_expr())
      }

      if (inherits(value, "Date")) {
        value <- as.character(value)
      }

      rlang::expr(as.Date(!!value))
    },
    copy_defaults = list(label = "Choose a date for {param}")
  )
}

named_date_build_ui <- function(id, copy, meta, context) {
  ptr_attach_help(
    shiny::dateInput(id, copy$label),
    copy$help
  )
}

named_date_resolve_expr <- function(value, meta, context) {
  if (is.null(value) || identical(value, "")) {
    return(ptr_missing_expr())
  }

  if (inherits(value, "Date")) {
    value <- as.character(value)
  }

  rlang::expr(as.Date(!!value))
}

make_non_inline_date_placeholder <- function() {
  ptr_define_placeholder(
    keyword = "date",
    build_ui = named_date_build_ui,
    resolve_expr = named_date_resolve_expr,
    copy_defaults = list(label = "Choose a date for {param}")
  )
}
