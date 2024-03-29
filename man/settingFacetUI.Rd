% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/paintr_ui.R
\name{settingFacetUI}
\alias{settingFacetUI}
\title{ui component for selecting variables for \code{facet_grid()}}
\usage{
settingFacetUI(id, data_vars, ui_id = "miscFacet")
}
\arguments{
\item{id}{An ID string that corresponds with the ID used to call the module's UI function.}

\item{data_vars}{variable names of the dataset}

\item{ui_id}{id of the ui element}
}
\value{
a list with two elements
\itemize{
\item ui: the ui element of this piece
\item id: the id of this ui element
}
}
\description{
ui component for selecting variables for \code{facet_grid()}
}
