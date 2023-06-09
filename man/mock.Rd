% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{mock}
\alias{mock}
\title{Mock input objects for the \code{xmap} package}
\format{
\subsection{\code{mock}}{

A list with:
\describe{
\item{named_ctr_iso3c}{named vector. Names are ISO-3 country codes, values are ISO English country names. Retrieved from \code{countrycode} package:
\url{https://github.com/vincentarelbundock/countrycode}}
\item{df_anzsco21}{4-column tibble. Contains major and submajor occupation codes and descriptions for ANZSCO21. Retrieved from \code{strayr} package:
\url{https://github.com/runapp-aus/strayr}}
\item{df_mixed}{3-column data.frame. }
}
}
}
\usage{
mock
}
\description{
A collection of mock inputs for experimenting with functions
in the \code{xmap} package.
\code{named_} objects are either named vectors or nested lists.
\code{df_} objects may contain source-target \emph{pairs} (no weights),
or weighted source-target \emph{links}.
}
\keyword{datasets}
