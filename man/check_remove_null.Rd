% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.R
\name{check_remove_null}
\alias{check_remove_null}
\title{check if a list has \code{NULL}; if so, remove it (them)}
\usage{
check_remove_null(x)
}
\arguments{
\item{x}{list}
}
\value{
\code{NULL} or a list
}
\description{
check if a list has \code{NULL}; if so, remove it (them)
}
\examples{
x <- list(a = 1, b = NULL)
check_remove_null(x)
}
