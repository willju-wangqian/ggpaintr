# Headless batch-report example -- INTERNAL / UNSTABLE API.
#
# `ggpaintr:::ptr_run_formula()` runs the formula -> plot pipeline without a
# Shiny session, so a parameter grid can be knit into N plots in an .Rmd (or a
# plain script) with no running server. This file is a manual smoke check, not
# an automated test; `ptr_run_formula()` is unexported and may change without
# notice -- do not rely on it from package-external code.

library(ggpaintr)

fml <- "ggplot(data = mtcars, aes(x = var, y = var)) +
  geom_point(aes(color = factor(cyl)), size = num)"

# Each row is a snapshot keyed by RAW input id (the `input_id` column of
# ggpaintr:::ptr_runtime_input_spec(ggpaintr:::ptr_translate(fml))).
grid <- list(
  list(ggplot_1_1_var_NA = "wt",   ggplot_1_2_var_NA = "mpg",  geom_point_2_num_NA = 3),
  list(ggplot_1_1_var_NA = "hp",   ggplot_1_2_var_NA = "mpg",  geom_point_2_num_NA = 2),
  list(ggplot_1_1_var_NA = "disp", ggplot_1_2_var_NA = "qsec", geom_point_2_num_NA = 4)
)

plots <- lapply(grid, function(row) {
  res <- ggpaintr:::ptr_run_formula(fml, inputs = row, envir = environment())
  stopifnot(isTRUE(res$ok))
  message("code: ", gsub("\n", " ", res$code_text))
  res$plot
})

# In an .Rmd you'd just `print(plots[[i]])` in a chunk per row; here, render
# them to a temp PDF as a smoke check.
out <- tempfile(fileext = ".pdf")
grDevices::pdf(out)
for (p in plots) print(p)
grDevices::dev.off()
message("wrote ", length(plots), " plots to ", out)
