messy <- data.frame(
  check.names = FALSE,
  "first column" = 1:3,
  "if"           = 4:6
)
clean <- ptr_normalize_column_names(messy)
ptr_app("ggplot(clean, aes(x = var, y = var)) + geom_point()", envir = environment())
