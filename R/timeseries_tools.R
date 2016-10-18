# Tools for continueing time-series based on its time-stamps

#' A robust nrow. Returns length if nrow not supported
.nsamples <- function(x) {
  h = nrow(x)
  if (is.null(h)) {
    h = length(x)
  }
  return(h)
}

#' Subset a time-series using the given index
#'
#' @param x      Time-series
#' @param ind    Indices to select
#'
#' @return x[ind] (also works for TS)
#'
#' @importFrom stats start end frequency

.ts.subset <- function(x, ind) {
  if (is.xts(x)) {
    x_sub = x[ind]
  } else if (is.ts(x)) {
    x_sub = window(x, start=head(ind, 1), end=tail(ind, 1))
  } else {
    stop("Time-series not supported")
  }

  return(x_sub)
}


#' Continue the given time-stamps for the next \code{h} steps
#'
#' @param timestamps The timestamps to continue
#' @param h          Horizon len to forecast the new time-series
#'
#' @return A vector of \code{h} time-stamps
#'
#' @export

continue_timestamps <- function(timestamps, h = 1) {

  N = length(timestamps)
  if (N < 2) {
    stop("Need at least 2 timestamps")
  }

  # TODO: detect and handle series without weekends

  # for time-based time-stamps
  last_timestamp = timestamps[N]
  dt = last_timestamp - timestamps[N-1]

  if ("timeDate" %in% class(timestamps)) {
    dt = as.numeric(dt, units="secs")
  }

  return(seq(last_timestamp + dt, by = dt, length.out = h))
}

#' Continue the given time-stamps for the next \code{h} steps using a reference timestamp
#' @param timestamps        The timestamps to continue
#' @param h                 Horizon len to forecast the new time-series
#' @param reference.stamps  Reference time-stamps. If given, the first of these time-stamps which are
#'                          larger than the last of "timestamps" are chosen and used
#'
#' @return A vector of \code{h} time-stamps
#'
#' @importFrom utils tail
#' @export

continue_timestamps_using_reference <- function(timestamps, h = 1, reference.stamps = NULL) {

  if (is.null(reference.stamps)) {
    return (continue_timestamps(timestamps, h))
  }

  last_ts = tail(timestamps, 1)

  return (head(reference.stamps[reference.stamps > last_ts], h))
}

#' Create a bew time-series with the new value that continues the input time-series
#' @param timeseries     the timeseries to continue its timestamps
#' @param newdata     new values to append to the timeseries
#'
#' @return Time-series with the new data
#'
#' @importFrom xts xts
#' @importFrom zoo index
#' @importFrom stats ts start end deltat
#' @export
ts_continue <- function(timeseries, newdata) {

  # get number of items to add
  h = .nsamples(newdata)

  ind = index(timeseries)
  new_timestamps = continue_timestamps(ind, h)

  # for the XTS
  if ("xts" %in% class(timeseries)) {

    # calculate the new time stamp if not given
    new.ts = xts(newdata, new_timestamps)

  } else if ("ts" %in% class(timeseries)) {

    new.ts  = ts(data = newdata,
               start=new_timestamps[1],
               end=tail(new_timestamps, 1),
               deltat = new_timestamps[2] - new_timestamps[1])
  } else if (is.array(timeseries)) {
    new.ts = as.array(newdata)
  } else if (is.vector(timeseries)) {
    new.ts = as.vector(newdata)
  } else {
    stop(paste("Time-series not supported for class", class(timeseries)))
  }

  return(new.ts)
}

#' Appends new values to a time-series.
#'
#' Appends new values to a time-series. Timestamps of the new data can be given.
#' If not not, they are automatically deduced by from the given series data.
#'
#' @param timeseries     the timeseries to continue its timestamps
#' @param newdata     new values to append to the timeseries
#' @param new_timestamps  new timestamp of the data to use
#'
#' @return Time-series (\code{ts} or \code{xts}) with the new data appended to it.
#'
#' @importFrom xts xts
#' @importFrom zoo index
#' @export
ts_append <- function(timeseries, newdata, new_timestamps = NULL) {

  # get number of items to add
  h = .nsamples(newdata)

  # for the XTS
  if ("xts" %in% class(timeseries)) {

    # calculate the new time stamp if not given
    if (is.null(new_timestamps) || is.na(new_timestamps)) {
        new_timestamps = continue_timestamps(index(timeseries), h)
    }

    newdata = xts(newdata, new_timestamps)
  }

  return (c(timeseries , newdata))
}


#' Change values in a time-series
#' @param x  Time-series to clone stamps from
#' @param newdata The new values
#'
#' @importFrom stats start end frequency
#' @importFrom xts xts
#' @importFrom zoo index
#' @export
clone_timestamps <- function(x, newdata) {

  index_x = index(x)

  if (.nsamples(newdata) != length(index_x)) {
    stop("Length of time-series and new-values are not equal")
  }

  # for the XTS
  if ("xts" %in% class(x)) {
    return (xts(newdata, index_x))
  }  else if ("ts" %in% class(x)) {
    return (ts(data = newdata,
               start=start(x),
               end=end(x),
               frequency = frequency(x)))
  } else {
    stop("Time-series not supported")
  }
}
