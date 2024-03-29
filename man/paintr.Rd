% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/paintr.R
\name{paintr}
\alias{paintr}
\title{Construct a \code{paintr_obj} based on an expression in \code{ggplot2} alike layout}
\usage{
paintr(
  id,
  data,
  expr,
  extra_ui = NULL,
  extra_ui_args = NULL,
  data_path = "data"
)
}
\arguments{
\item{id}{An ID string that corresponds with the ID used for all component of this \code{paintr_obj}}

\item{data}{the dataset for plotting}

\item{expr}{a \code{ggplot2} alike expression which is referred to as "paintr expression" in \code{ggpaintr}}

\item{extra_ui}{a named list: of extra functions that generate paintr_ui. The
names of this list should be key words of a paintr expression
For example \code{extra_ui = list(param1 = my_ui_func1, param2 = my_ui_func2)}}

\item{extra_ui_args}{a named list: of lists of arguments. The names of this
list should be key words of a paintr expression. And the element of a key word
should be a list of arguments that goes into the ui function of this key word
For example \code{list(param1 = list(my_ui_func1_arg1, my_ui_func1_arg2), param2 = list(my_ui_func2_arg1, my_ui_func2_arg2))}}

\item{data_path}{string. path to the dataset; used for code of obtaining the data}
}
\value{
a \code{paintr_obj} that contains all pieces of the paintr expression and
and their corresponding ui elements used to build a shiny app. Additionally, it includes:
\itemize{
\item \code{id}
\item \code{data}
\item \code{data_path}
}
}
\description{
Construct a \code{paintr_obj} based on an expression in \code{ggplot2} alike layout
}
\details{
the \code{expr} should have a \code{ggplot2} alike layout and be wrapped by \code{rlang::expr}.
And the functionality of \code{paintr} is based on this \code{expr} that follows certain
rules. In general, a ggpaintr expression should be:

\if{html}{\out{<div class="sourceCode r">}}\preformatted{rlang::expr(
  geom_<chart>(aes(<mapping_1>, <mapping_2>), <geom_args_1>, <geom_args2>) +
    <plot_settings_1> +
    <plot_settings_2> +
    <plot_settings_3>
)
}\if{html}{\out{</div>}}

And note that:
\itemize{
\item \verb{<mapping_*>} is a \code{mapping} keyword that represents a aesthetic mapping in \code{ggplot2}, like \code{x}, \code{y}, or \code{color}
\item \verb{<geom_args_*>} is a \code{geom_args} keyword that represents an argument passed into \verb{geom_<chart>}, like \code{position}
\item \verb{<plot_settings_*>} is a \code{plot_settings} keyword that represents a \code{ggplot2} function like \code{coord_flip}, which can modify the plot
\item \verb{geom_<chart>} specifies the geom function used for the plot. \code{'geom_'}is used to identify \verb{geom_<chart>}
\item \code{aes} is used to distinguish \code{mapping} keywords and \code{geom_args} keywords in \verb{geom_<chart>}
}
}
\note{
\code{extraFunc} and \code{extraFuncArgs} allow users to override the ui functions provided by \code{ggpaintr} package
}
\examples{
paintr("boxplot_id", mtcars, geom_boxplot(aes(x, y)))

# alternatively, one can define the expression first
library(rlang)
my_expr <- rlang::expr(geom_boxplot(aes(x, y)))
paintr("boxplot_id", mtcars, !!my_expr)
}
