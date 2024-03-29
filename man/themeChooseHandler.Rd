% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/paintr_server_handler.R
\name{themeChooseHandler}
\alias{themeChooseHandler}
\title{Module server that provides several \code{ggplot2} themes}
\usage{
themeChooseHandler(id, module_id)
}
\arguments{
\item{id}{An ID string that corresponds with the ID used to call the module's UI function.}

\item{module_id}{the id of the ui elements (not including its prefix created by
the name space)}
}
\value{
an implemented \code{ggplot2} theme and its code or \code{NULL}
}
\description{
Module server that provides several \code{ggplot2} themes
}
\note{
currently the following themes of \code{ggplot2} are implemented:
\itemize{
\item \code{theme_gray()}
\item \code{theme_classic()}
\item \code{theme_bw()}
\item \code{theme_minimal()}
}
}
