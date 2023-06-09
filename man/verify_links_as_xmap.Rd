% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/verify_links_as_xmap.R
\name{verify_links_as_xmap}
\alias{verify_links_as_xmap}
\title{Check if candidate links meet crossmap properties}
\usage{
verify_links_as_xmap(df, from, to, weights, tol = .Machine$double.eps^0.5)
}
\arguments{
\item{df}{data.frame like object containing candidate links}

\item{from, to}{Columns in \code{x} specifying the source and target nodes}

\item{weights}{Column in \code{x} specifying the weight applied to data passed along the directed link between source and target node}

\item{tol}{numeric \eqn{\ge 0}. Ignore differences smaller than \code{tol}.
Passed through to the \code{tolerance} arg of \code{base::all.equal()}.}
}
\description{
Check if candidate links meet crossmap properties
}
\examples{
# For a well formed crossmap:
links <- data.frame(
  a = "AUS",
  b = c("VIC", "NSW", "WA", "OTHER"),
  w = c(0.1, 0.15, 0.25, 0.5)
)
verify_links_as_xmap(links, from = a, to = b, weights = w)
}
