% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.R
\name{append_list_name}
\alias{append_list_name}
\title{Append a named list}
\usage{
append_list_name(x, name, value)
}
\arguments{
\item{x}{a list}

\item{name}{new name}

\item{value}{value of the new name}
}
\value{
a list
}
\description{
Append a named list
}
\examples{
x <- list(a = 1, b = 2)
append_list_name(x, "c", 3)
}
