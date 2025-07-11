% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conv.R
\name{to_eplus}
\alias{to_eplus}
\title{Convert a DeST model to EnergyPlus model}
\usage{
to_eplus(dest, ver = "latest", copy = TRUE, verbose = FALSE)
}
\arguments{
\item{dest}{A [string or DBIConnection] path to a DeST model file or a
DBIConnection object.}

\item{ver}{[string] A character string specifying the EnergyPlus version.
It can be \code{"latest"}, which is the default, to indicate using the
latest EnergyPlus version supported by the
\{\href{https://cran.r-project.org/package=eplusr}{eplusr}\} package.}

\item{copy}{[logical] Whether to copy the input DeST database to a
temporary SQLite database. Note that if \code{FALSE}, the input database
will be modified during the conversion. Default is \code{TRUE}.}

\item{verbose}{[logical] Whether to show verbose messages. Default is
\code{FALSE}.}
}
\value{
[eplusr::Idf] The converted EnergyPlus model.
}
\description{
Convert a DeST model to EnergyPlus model
}
