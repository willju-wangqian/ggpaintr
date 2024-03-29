% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.R
\name{check_char_set_names}
\alias{check_char_set_names}
\title{Check if a character vector has name}
\usage{
check_char_set_names(x)
}
\arguments{
\item{x}{a character vector}
}
\value{
a character vector with names
}
\description{
If this character vector does not have names, then its elements will be assigned to \code{names(x)}
}
\examples{
check_char_set_names(c("a", "b"))
}
