% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/paintr_server_handler.R
\name{scaleColorHandler_inner}
\alias{scaleColorHandler_inner}
\title{The inner module server that sets colors through \code{scale_fill_gradient}, \code{scale_fill_manual},
\code{scale_color_gradient}, \code{scale_color_manual}}
\usage{
scaleColorHandler_inner(id, selected_colors, color_fill)
}
\arguments{
\item{id}{An ID string that corresponds with the ID used to call the module's UI function.}

\item{selected_colors}{the selected colors from input}

\item{color_fill}{string. either \code{color} or \code{fill}. Specifies whether it's \code{scale_color}
or \code{scale_fill}}
}
\value{
the return of \verb{scale_<fill/color>_<gradient/manual>} and its code or \code{NULL}
}
\description{
The inner module server that sets colors through \code{scale_fill_gradient}, \code{scale_fill_manual},
\code{scale_color_gradient}, \code{scale_color_manual}
}
