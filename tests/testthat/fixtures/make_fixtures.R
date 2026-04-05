simple_numeric <- data.frame(
  x = c(1, 2, 3, 4),
  y = c(2, 4, 6, 8),
  group = c("a", "a", "b", "b")
)

non_tabular <- list(alpha = 1, beta = 2)

saveRDS(simple_numeric, "simple_numeric.rds")
saveRDS(non_tabular, "non_tabular.rds")
