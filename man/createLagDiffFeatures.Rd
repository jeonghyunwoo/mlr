% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createLagDiffFeatures.R
\name{createLagDiffFeatures}
\alias{createLagDiffFeatures}
\title{Generate lags and differences for feature variables}
\usage{
createLagDiffFeatures(obj, lag = 0L, difference = 0L, difference.lag = 1L,
  cols = NULL, target = character(0L), seasonal.lag = 0L,
  seasonal.difference = 0L, seasonal.difference.lag = 1L, frequency = 1L,
  na.pad = TRUE, return.nonlag = TRUE)
}
\arguments{
\item{obj}{[\code{data.frame} | \code{\link{Task}}]\cr
Input data.}

\item{lag}{[\code{integer}]\cr
An integer vector of lag lengths.}

\item{difference}{[\code{integer}]\cr
An integer of the order of differencing}

\item{difference.lag}{[\code{integer}]\cr
An integer denoting the period to difference over}

\item{cols}{[\code{character}]\cr
A character vector of columns to create dummy features for.
 Default is to use all columns.}

\item{target}{[\code{character(1)} | \code{character(2)} | \code{character(n.classes)}]\cr
Name(s) of the target variable(s).
Only used when \code{obj} is a data.frame, otherwise ignored.
If survival analysis is applicable, these are the names of the survival time and event columns,
so it has length 2.
For multilabel classification these are the names of logical columns that indicate whether
a class label is present and the number of target variables corresponds to the number of
classes.}

\item{seasonal.lag}{[\code{integer}]\cr
An integer vector of seasonal lag lengths, made as \code{seasonal.lag * frequency}}

\item{seasonal.difference}{[\code{integer}]\cr
An integer of the seasonal order of difference, made as \code{seasonal.difference * frequency}}

\item{seasonal.difference.lag}{[\code{integer}]\cr
An integer denoting the period to seasonaly difference over}

\item{frequency}{[\code{integer}]\cr
An integer representing the periodicity in the time series. If frequency is declared in the task,
the task frequency will be used.}

\item{na.pad}{[\code{logical}]\cr
A logical to denote whether the data should be padded to the original size with NAs}

\item{return.nonlag}{[\code{logical}]\cr
A logical to denote whether the original unlagged features should be returned}
}
\description{
Replace all variables with their generated lagged and differenced variables.
Uses the \code{xts} framework for developing lags and is only available for \code{TimeTasks}.
}
\seealso{
Other eda_and_preprocess: \code{\link{capLargeValues}},
  \code{\link{createDummyFeatures}},
  \code{\link{dropFeatures}},
  \code{\link{mergeSmallFactorLevels}},
  \code{\link{normalizeFeatures}},
  \code{\link{removeConstantFeatures}},
  \code{\link{summarizeColumns}}
}

