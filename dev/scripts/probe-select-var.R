suppressMessages(devtools::load_all("/Users/willju/Research/ggpaintr"))

obj <- ptr_parse_formula(
  "mtcars |> select(var) |> ggplot(aes(x = var)) + geom_histogram()"
)

cat("=== layers ===\n")
print(names(obj$expr_list))

cat("\n=== ggplot layer expr ===\n")
print(obj$expr_list[["ggplot"]])

cat("\n=== data_pipeline_info ===\n")
str(obj$data_pipeline_info)

cat("\n=== placeholder_map[ggplot] ===\n")
str(obj$placeholder_map[["ggplot"]])

cat("\n=== runtime input spec ===\n")
print(ptr_runtime_input_spec(obj))
