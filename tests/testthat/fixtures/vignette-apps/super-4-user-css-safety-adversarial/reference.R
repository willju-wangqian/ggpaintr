# super-4 reference: all-defaults equivalent of the paste0/sprintf-assembled
# formula in app.R, with the string-builder pieces resolved at authoring time.
# Doc-only — not executed by any test; kept for diff against the rendered
# final-mode code text (the test asserts propagation, not equality with this).
#
# Resolution map:
#   y_arg          -> "ppVar(wt)"        -> y = wt        (final mode)
#   title_segment  -> sprintf("Speed vs %s", "Weight")  -> "Speed vs Weight"
#   ppColor()      -> "#3366FF"          (build_ui fallback when no formula
#                                         default and the widget is untouched)

ggplot2::ggplot(
  head(mtcars),
  ggplot2::aes(x = mpg, y = wt, color = cyl)
) +
  ggplot2::geom_point(size = 2, alpha = 0.7) +
  ggplot2::geom_smooth(method = "lm", color = "#3366FF",
                        linewidth = 1, span = 0.75) +
  ggplot2::labs(title = "Speed vs Weight", subtitle = nrow(mtcars))
