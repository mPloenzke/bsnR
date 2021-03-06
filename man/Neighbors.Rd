% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Neighbors.R
\name{Neighbors}
\alias{Neighbors}
\title{Neighbors object.}
\usage{
Neighbors(dat, ID, keyVars)
}
\arguments{
\item{dat}{Input data matrix used in the BSN algorithm.}

\item{ID}{Character or numeric value. Identifing column in dat.
This is the ID variable for which \code{Neighbors} will reference.}

\item{keyVars}{Character or numeric vector. Which variables to
include when generating sort keys?}
}
\value{
An object of class \code{\link{Neighbors}}.
}
\description{
Stores the input data and parameters for calling \code{blockedSN}.
}
