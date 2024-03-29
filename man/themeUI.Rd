% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/paintr_ui.R
\name{themeUI}
\alias{themeUI}
\title{ui component for \code{theme()}}
\usage{
themeUI(id, theme_selected = TRUE)
}
\arguments{
\item{id}{An ID string that corresponds with the ID used to call the module's UI function.}

\item{theme_selected}{vector of selected parameters of \code{theme()}}
}
\value{
a list with two elements
\itemize{
\item ui: the ui element of this piece
\item id: the id of this ui element
}
}
\description{
ui component for \code{theme()}
}
