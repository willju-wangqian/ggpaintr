# Upload edge cases — single launcher, multiple sub-cases driven by the test JS.
# Shell formula uses `data = upload` and two `var` aes placeholders so we can
# verify that after upload the column picker is populated with the fixture's
# columns ("a", "b", "c").
#
# Sub-cases (selected by which fixture the test JS POSTs):
#   simple_numeric.csv   — baseline; var pickers populate, plot renders
#   simple_numeric.rds   — same outcome; rds reader path
#   simple_numeric.tsv   — same outcome; tsv reader path
#   simple_numeric.xlsx  — same outcome; xlsx reader path
#   simple_numeric.json  — same outcome; json reader path
#   bad_extension.txt    — upload rejected with inline error, plot stays empty
#   blank-name           — upload csv with `_name` field empty; generated code
#                          uses dataset name derived from filename ("simple_numeric")
#   custom-name          — upload csv with `_name` field set to "my_data";
#                          generated code uses "my_data".
source("tests/manual/edge_cases/launchers/_setup.R")
shiny::runApp(
  ptr_app("ggplot(data = upload, aes(x = var, y = var)) + geom_point()"),
  port = PORT, host = "127.0.0.1", launch.browser = FALSE
)
