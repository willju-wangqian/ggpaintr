# Doc-only reference (ADR 0013 D3). Not parsed by tests.
#
# Plain-ggplot2 expressions that the App-3 super-app's pre-quoted formulas
# resolve to under "all defaults" -- i.e. every placeholder collapsed to its
# default. Kept here so a human reader can diff the placeholder formula
# against its all-defaults equivalent without running the app.

library(ggplot2)

# formula_a default-equivalent (ppVar(mpg)/ppVar(wt)/ppVar(cyl) -> mpg/wt/cyl;
# ppNum(2)/ppNum(0.7) -> 2/0.7; ppRange(c(10, 40)) -> c(10, 40); ppText("...")
# -> "Cell A: ggplot").
ggplot(mtcars, aes(x = mpg, y = wt, color = cyl)) +
  geom_point(size = 2, alpha = 0.7) +
  scale_x_continuous(limits = c(10, 40)) +
  labs(title = "Cell A: ggplot")

# formula_b default-equivalent.
ggplot(mtcars, aes(x = hp, y = qsec, color = cyl)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", linewidth = 1) +
  labs(title = "Cell B: plotly")
