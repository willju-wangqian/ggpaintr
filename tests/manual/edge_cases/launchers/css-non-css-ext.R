# CSS edge case — file exists but extension isn't .css → ptr_app aborts.
# Test: expect non-zero exit and an error that names the offending path
# and explains the extension constraint.
source("tests/manual/edge_cases/launchers/_setup.R")
ptr_app("ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()",
        css = file.path(CSS_DIR, "not-a-stylesheet.notcss"))
