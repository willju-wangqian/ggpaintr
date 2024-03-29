% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/paintr_control.R
\name{getControlList}
\alias{getControlList}
\title{Get all the implemented keys and their corresponding ui functions or handler functions}
\usage{
getControlList(scope = "mapping", type = "ui", show_all = FALSE)
}
\arguments{
\item{scope}{optional. \code{scope} can be one of \code{mapping}, \code{geom_args}, or \code{plot_settings}}

\item{type}{optional. By default it's \code{ui}. Can be \code{ui} or \code{handler}}

\item{show_all}{optional. bool. \code{show_all = TRUE} prints all keys}
}
\value{
a named list where names are ggpaintr keys and list elements are the corresponding
ui functions or handler functions
}
\description{
Get all the implemented keys and their corresponding ui functions or handler functions
}
