% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scoreFamilies.R
\name{scoreFamilies}
\alias{scoreFamilies}
\alias{scoreFamilies.default}
\alias{scoreFamilies.Blocks}
\alias{scoreFamilies.Scores}
\title{Calculate match score generic.}
\usage{
scoreFamilies(object, familyDat, selfID, motherID, fatherID)

\method{scoreFamilies}{default}(object, familyDat, selfID, motherID,
  fatherID)

\method{scoreFamilies}{Blocks}(object, familyDat, selfID, motherID,
  fatherID)

\method{scoreFamilies}{Scores}(object, familyDat, selfID, motherID,
  fatherID)
}
\arguments{
\item{object}{object containing pre-computed Neighbors from the BSN algorithm.}

\item{familyDat}{data matrix containing family data so score.}

\item{selfID}{character referencing the column in familyDat which points to a self's ID.}

\item{motherID}{character referencing the column in familyDat which points to a self's mother's ID}

\item{fatherID}{character referencing the column in familyDat which points to a self's father's ID}
}
\value{
An object of class \code{\link{Scores}} containing the scored neighbor pairs.
}
\description{
\code{scoreFamilies} returns a \code{\link{Scores}} object with the \code{Neighbors}
slot populated with a match score for each potential duplicate found from the BSN algorithm.
A match score is calculated by averaging the number of string/binary variable matches
consistent between the two records plus a similarity measure for the continuous variables.
}
