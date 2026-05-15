# cu2: css= argument; requires my-theme.css alongside this file
css_path <- file.path(dirname(sys.frame(1)$ofile %||% "."), "my-theme.css")
if (!file.exists(css_path)) css_path <- "dev/audit/runs/my-theme.css"
ptr_app(
  "ggplot(mtcars, aes(var, var)) + geom_point()",
  css = css_path
)
