% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/narx.R
\name{build_narx}
\alias{build_narx}
\title{Build a nonlinear auto-regressive time-series forecasting with exogenous input}
\usage{
build_narx(learner_func, p, P = 0, d = 0, D = 0, freq = 1,
  lambda = NULL, ...)
}
\arguments{
\item{learner_func}{Non-linear learning function}

\item{p}{Auto-regressive order}

\item{P}{Seasonal auto-regressive order}

\item{d}{Model differencing order}

\item{D}{Seasonal differencing order}

\item{freq}{Time-series periodicity}

\item{lambda}{Box-Cox transform's lambda}

\item{...}{Extra parameters passed to \code{learner_func}}
}
\value{
A NARX model object
}
\description{
Build a nonlinear auto-regressive time-series forecasting with exogenous input
}
\seealso{
\code{\link{narx}}
}

