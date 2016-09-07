# Originally from R package Caret <http://cran.r-project.org/web/packages/caret/>
# https://github.com/topepo/caret/blob/master/pkg/caret/R/createTimeSlices.R
# License: GPL (>= 2)
# Originally from Tony Cooper <tonyc@iconz.co.nz> on 1/9/13
# Packaged by Max Kuhn <Max.Kuhn@pfizer.com> into caret
# Refactoring and Roxygen Documentation by Farzad Noorian <farzad.noorian@gmail.com>
###########################################################################################

#' Create time-slices for time-series cross-validation
#'
#' @param y                   The input time-series
#' @param initial_window initial number of consecutive values in each training set sample
#' @param horizon       number of consecutive values in test set sample
#' @param fixed_window   To use a fixed window of size \code{initial_window} for training every stride. If FALSE, we use the maximum possible length for the training set.
#' @param skip          How many slices are to be skipped and not used for cross-validation
#'
#' @return Time-slices, for use with \code{\link{ts_crossval}}
#'
#' @details It is ensured that \code{initial_windowlength + horizonlength <= length(y)}.
#'
#' @references See \url{https://github.com/topepo/caret/blob/master/pkg/caret/R/createTimeSlices.R}
#'
#' @export
create_cv_timeslices <- function(y, initial_window, horizon = 1, fixed_window = TRUE, skip = 0) {

  skip = floor(skip)
  initial_window = floor(initial_window)
  horizon = floor(horizon)
  stopifnot(skip >= 0)

  stops <- (seq(along = y))[initial_window:(length(y) - horizon)]

  if (fixed_window) {
    starts <- stops - initial_window + 1
  } else {
    starts <- rep(1, length(stops)) # all start at 1
  }

  train <- mapply(seq, starts, stops, SIMPLIFY = FALSE)
  test <- mapply(seq, stops+1, stops+horizon, SIMPLIFY = FALSE)
  names(train) <- paste("Training", gsub(" ", "0", format(seq(along = train))), sep = "")
  names(test) <- paste("Testing", gsub(" ", "0", format(seq(along = test))), sep = "")

  thin <- function(x, skip = 2) {
    n <- length(x)
    x[seq(1, n, by = skip)]
  }

  if(skip > 0) {
    train <- thin(train, skip = skip+1)
    test <- thin(test, skip = skip+1)
  }
  out <- list(train = train, test = test)

  out
}


## This will reproduce the 4 examples above
#createTimeSlices(1:9, 5, 1, fixed_window = FALSE)
#createTimeSlices(1:9, 5, 1, fixed_window = TRUE)
#createTimeSlices(1:9, 5, 3, fixed_window = TRUE)
#createTimeSlices(1:9, 5, 3, fixed_window = FALSE)
