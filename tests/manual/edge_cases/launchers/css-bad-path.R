# CSS edge case — missing path → ptr_app aborts at construction.
# Test: run this launcher; expect a non-zero exit code and an error message
# that names the missing file. The app is intentionally never reached.
source("tests/manual/edge_cases/launchers/_setup.R")
ptr_app("ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()",
        css = file.path(CSS_DIR, "this-file-does-not-exist.css"))
