# Copyright (c) 2016 Farzad Noorian and Richard I. A. Davis.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
################################################################################

# Implementation of Lag Windows. Usefull for Time-series forecasting.

#' Collects and embeds lagged windows of data.
#'
#' @param x            data input (can handle multi-column data, such as data.frames)
#' @param lag_indices  index of lags to embed. Can be empty
#' @param pstr         name of datacolumn (only used if x is a single column or vector)
#' @param no_peeking   if TRUE, lags <= 0 (from now and future) are removed
#' @param ...          Not used
#'
#' @return             a matrix of lagged windows
#'
#' @export
lagmatrix <- function(x, lag_indices, pstr=deparse(substitute(x)), no_peeking=FALSE, ...)
  UseMethod("lagmatrix")

#' @method lagmatrix vector
#' @export
lagmatrix.vector <- function(x, lag_indices, pstr=deparse(substitute(x)), no_peeking=FALSE, ...) {

  # check no peeking
  if (no_peeking) {
    lag_indices = lag_indices[lag_indices > 0]
  }

  # return empty if no indices given
  if (length(lag_indices) == 0) {
    return (NULL)
  }

  # extract information from time-seres
  x = as.numeric(x)
  len_x = length(x)

  # check if lags fall out of the number of samples
  if (len_x <= max(lag_indices)) {
      stop("Lag window: Not enough data for the given Lag.")
  }

  # put each lag into a matrix column
  embeded.ts = matrix(NA, nrow = len_x, ncol = length(lag_indices))
  for (i in seq_along(lag_indices)) {
    L = lag_indices[i]
    if (L >= 0) { # lags
      embeded.ts[(1+L):len_x, i] = x[1:(len_x - L)]
    } else if (L < 0) { # leads
      L = abs(L)
      embeded.ts[1:(len_x - L), i] = x[(1+L):len_x]
    }
  }

  # give each column a name
  colnames(embeded.ts) = ifelse(lag_indices < 0,
                                paste0(pstr, ".lf", -lag_indices), # the LEAD FORWARD!
                                paste0(pstr, ".l", lag_indices)) # the lag

  return (embeded.ts)
}


#' @method lagmatrix matrix
#' @export
lagmatrix.matrix <- function(x, lag_indices, pstr=deparse(substitute(x)), no_peeking=FALSE, ...) {

  if (length(lag_indices) == 0) { # return empty if no indices given
    return (NULL)
  }

  if (ncol(x) == 1) { # for one column objects
    return (lagmatrix(x[,1], lag_indices, pstr, no_peeking))
  } else { # for multi column objects
    # call lag.window each separately
    nc = ncol(x)
    hw = lapply(1:nc, function(i) lagmatrix(x[,i], lag_indices, pstr=paste0(colnames(x)[i]), no_peeking))

    # combine them
    hw.combined = do.call(cbind, hw)
    colnames(hw.combined) = as.vector(sapply(hw, colnames))

    return(hw.combined)
  }
}

#' @method lagmatrix numeric
#' @export
lagmatrix.numeric <- lagmatrix.vector

#' @method lagmatrix data.frame
#' @export
lagmatrix.data.frame <- lagmatrix.matrix

#' @importFrom stats ts start end frequency deltat
#' @method lagmatrix ts
#' @export
lagmatrix.ts <- function(x, lag_indices, pstr=deparse(substitute(x)), no_peeking=FALSE, ...) {

  # return empty if no indices given
  if (length(lag_indices) == 0) {
    return (NULL)
  }

  w = lagmatrix(coredata(x), lag_indices, pstr, no_peeking)

  # convert back to ts
  w = ts(w, start=start(x), end=end(x), frequency=frequency(x), deltat = deltat(x))

  return(w)
}

#' @importFrom xts xts
#' @importFrom zoo index
#' @method lagmatrix xts
#' @export
lagmatrix.xts <- function(x, lag_indices, pstr=deparse(substitute(x)), no_peeking=FALSE, ...) {

  w = lagmatrix(coredata(x), lag_indices, pstr, no_peeking)

  # convert back to xts
  w = xts(w, index(x))

  return(w)
}

# LaggedWindows <- function(x, p = 0, P = 0, freq = 1, shift = 0, pstr=deparse(substitute(x))) {
#   # collects and embeds p lengthed windows of data as a vector
#   # INPUTS:
#   #    x: data input (can handle multi-column data, such as data.frames)
#   #    p: window length
#   #    P: seasonal window length
#   #    shift: if the time-series is to be shifted
#   #    freq: seasonal frequency
#   #    pstr: name of datacolumn
#   # OUTPUT:
#   #    a matrix of lagged windowsl_a_mat
#
#   n <- length(x)
#
#   # the normal window order
#   ind.lag <- 0:p # it starts from 0!
#
#   # seasonal window order
#   if (P * freq > 0) {
#     ind.lag = unique(c(ind.lag, 1:P * freq ))
#   }
#
#   #  the lags
#   ind.lag = ind.lag + shift
#
#   return (lagmatrix.multi.column.ts(x, ind.lag, pstr))
# }

#' Collects and embeds lagged windows of data as a vector.
#'
#' Order \code{p} determines size of the recent lags,
#' and \code{P} and \code{freq} can be used to create seasonal lags.
#'
#' @param x           data input (can handle multi-column data, such as data.frames)
#' @param p           window length
#' @param P           seasonal window length
#' @param freq        seasonal period
#' @param shift       if the time-series is to be shifted
#' @param no_peeking  if TRUE, lags <= 0 (from now and future) are removed
#' @param pstr        name of datacolumn
#'
#' @return  a matrix of lagged windows
#'
#' @seealso \code{\link{centered_lagged_windows}}, \code{\link{seasonal_lag_windows}}
#'
#' @export
lag_windows <- function(x, p = 0, P = 0, freq = 1, shift = 0, no_peeking = TRUE, pstr=deparse(substitute(x))) {

  n <- length(x)

  # the normal window order
  ind.lag <- 0:p # it starts from 0!

  # seasonal window order
  if (P * freq > 0) {
    ind.lag = unique(c(ind.lag, 1:P * freq ))
  }

  #  the lags
  ind.lag = ind.lag + shift

  # check peeking
  if (no_peeking) {
    ind.lag = ind.lag[ind.lag > 0]
  }

  return (lagmatrix(x, ind.lag, pstr))
}
#' Create Centers the lag winow around Lag 0, with width p on each side.
#'
#' @details Use this function for creating features from time-series that are available in future.
#'
#' @param x data input (can handle multi-column data, such as data.frames)
#' @param p  window length around lag 0.
#' @param pstr        name of data column
#'
#' @return  A time-series object of lagged and leaded x. If \code{p == 0}, \code{x} itself is returned. If \code{p < 0}, \code{NULL} is returned.
#'
#' @seealso \code{\link{lag_windows}}, \code{\link{seasonal_lag_windows}}
#'
#' @export
centered_lagged_windows <- function(x, p = 1, pstr=deparse(substitute(x))) {

  if (p < 0) {
    return (NULL)
  }

  if (p == 0) {
    colnames(x) = paste0(pstr, ".l0")
    return(x)
  }

  return (lag_windows(x, 2*p+1, shift = -p, pstr=pstr, no_peeking = FALSE))
}

#' Creates a window around each seasonal lag, with \code{width+shift}/\code{width-shift} on each side.
#'
#' @param x           data input (can handle multi-column data, such as data.frames)
#' @param P           seasonal window length
#' @param freq        seasonal frequency
#' @param width       width of the left or right window
#' @param shift       shifts each window towards the end of time-series
#' @param no_peeking  if TRUE, lags <= 0 (from now and future) are removed
#' @param pstr        name of data column
#'
#' @return  A time-series object of lagged and leaded x
#'
#' @seealso \code{\link{lag_windows}}, \code{\link{centered_lagged_windows}}
#'
#' @export
seasonal_lag_windows <- function(x, P = 1, freq = frequency(x), width = 0, shift = 0, no_peeking = TRUE, pstr=deparse(substitute(x))) {

  # Sanity checks
  if (width < 0 ||
      P*freq <= 0) {
    return (NULL)
  }

  # get the lags
  ind.lag = unique(1:P * freq)

  # add the window width
  left_windw = shift - width
  right_windw = shift + width

  shifted.lags = NULL
  for (i in left_windw:right_windw) {
    shifted.lags = unique(c(shifted.lags, i+ind.lag))
  }

  # check peeking
  if (no_peeking) {
    shifted.lags = shifted.lags[shifted.lags > 0]
  }

  # sort lags and extract data
  shifted.lags = sort(shifted.lags)

  return (lagmatrix(x, shifted.lags, pstr))
}
