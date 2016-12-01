# Aggregation Utilities for time series

#' Aggregate data to the given time granularity
#'
#' @param   data         Input \code{\link{xts}} object.
#' @param   on           Determines the new unit of the data frequency. Can be "minutes", "hours", or "days".
#' @param   k            The new value of frequency (only for hours and minutes).
#' @param   FUN          Function to aggregate the data with. Either a closure (function), or a string.
#'                       Strings currently supported are "sum", "prod", "min", "max", last", and "first".
#'                       By default \code{sum} is used.
#' @param   na_handling  What to do with NA's. "auto" fills 0 if FUN == sum, na.locf's last value else.
#'                       NULL does nothin to NAs. If a function is given, it is applied directly.
#' @param   align_data   Align data to the next truncated time-stamp
#' @param   start_date   First Date in the regular series. Calculated from the given series if NULL. Its type must match the type of \code{data}'s index.
#' @param   end_date     Last Date in the regular series. Calculated from the given series if NULL. Its type must match the type of \code{data}'s index.
#' @param   ...          Arguements passed to \code{FUN}.
#'
#' @return  Aggregated Time-series
#'
#' @importFrom stats      start end
#' @importFrom lubridate  minutes hours days
#' @importFrom xts        align.time endpoints period.apply
#' @importFrom zoo        na.locf
#'
#' @export
aggregate_xts <- function(data, on, k = 1, FUN = "sum", na_handling = "auto", align_data = TRUE, start_date = NULL, end_date = NULL, ...) {

  # match partial arguement
  on = match.arg(on, c("mins", "minutes", "hours", "days"))
  if (on == "minutes") {
    on = "mins"
  }

  # check if any data is given for Aggregation
  if (length(data) == 0) {
    stop("No data given to Aggregation Function")
  }

  ##
  # create an empty timeseries, covering the whole time

  # get start and stop date if not given
  if (is.null(start_date)) start_date = start(data)
  if (is.null(end_date))   end_date = end(data)

  if (!( ("timeDate" %in% class(start_date)) || ("Date" %in% class(start_date)) || ("POSIXt" %in% class(start_date)))) {
    start_date = as.POSIXct(start_date)
  }

  if (!( ("timeDate" %in% class(end_date)) || ("Date" %in% class(end_date)) || ("POSIXt" %in% class(end_date)))) {
    end_date = as.POSIXct(end_date)
  }

  if (any(class(start_date) != class(index(data)))) {
    stop("Start date must have the same type as the data")
  }
  if (any(class(end_date) != class(index(data)))) {
    stop("End date must have the same type as the data")
  }

  # shift the end day a bit when aligning
  if (align_data) {
    if (on == "mins") {
      end_date = end_date - lubridate::minutes(k)
    } else if (on == "hours") {
      end_date = end_date - lubridate::hours(k)
    }
  }

  # get the new time index
  new_index = seq(start_date, end_date,  by=paste(k, on))

  if (start_date < end_date) {
    # create an empty timeseries, covering the whole time
    # then merge with client data, to create a "regular" time series
    #e <- xts(NULL, new_index), frequency = x.freq)
    e <- xts(NULL, new_index)
    data <- merge(e, data, fill=NA)
  }

  # NA handling
  if (!is.null(na_handling)) {
    if (na_handling == "auto") {
      if (deparse(FUN)[1] == deparse(sum) || (is.character(FUN) && FUN == "sum")) {
        # for sum, use 0
        data[is.na(data)] <- 0
      } else {
        # for others, fill with previous value
        data <- na.locf(data)
      }
    } else  {
      data <- na_handling(data)
    }
  }

  ## do the actual sum
  ep <- endpoints(data, on = on, k = k)

  if (!is.character(FUN)) {
    data.sum <- period.apply(data, ep, FUN, ...)        #TODO: period apply is slow
  } else {
    data.sum <- period.apply.string(data, ep, FUN)
  }

  # align time series to the minute
  #TODO: this if also seems to be slow
  if (align_data) {
    if (on == "mins") {
      aligned.data = align.time(data.sum, k * 60)
    } else if (on == "hours") {
      aligned.data = align.time(data.sum, k * 3600)
    }  else if (on == "days") {
      align.by.day = function(dt) {
        next.days = as.Date(index(dt) + days(1))
        return (xts(dt, order.by=next.days))
      }
      aligned.data = align.by.day(data.sum)
    }
  } else {
    aligned.data = data.sum
  }

  # set frequency
  if (on == "mins") {
    f = 24 * 60 / k
  } else if (on == "hours") {
    f = 24 / k
  }  else if (on == "days") {
    f = 1 / k
  }

  attr(aligned.data, "freq") = f

  return (aligned.data)
}


#' Aggregate data to a minutely time-series
#'
#' @param   data        Input \code{\link{xts}} object.
#' @param   mins        Minutes seperating two samples.
#' @param   FUN         Function to aggregate the data with. By default \code{sum} is used.
#' @param   start_date   First Date in the regular series. Calculated from the given series if NULL. Its type must match the type of \code{data}'s index.
#' @param   end_date     Last Date in the regular series. Calculated from the given series if NULL. Its type must match the type of \code{data}'s index.
#' @param   ...         Other parameters given to FUN
#'
#' @return  Minutely data time Series
#'
#' @export

create_minutely_xts <- function(data, mins, FUN = sum, start_date = NULL, end_date = NULL, ...) {
  aggregate_xts(data, on="mins", k = mins, FUN = FUN, na_handling = "auto", align_data = TRUE, start_date = start_date, end_date = end_date, ...)
}

#' Aggregate data to a hourly time-series
#'
#' @param   data        Input \code{\link{xts}} object.
#' @param   hours       Hours seperating two samples.
#' @param   FUN         Function to aggregate the data with. By default \code{sum} is used.
#' @param   start_date  First Date in the regular series. Calculated from the given series if not given.
#' @param   end_date    Last Date in the regular series. Calculated from the given series if not given.
#' @param   ...         Other parameters given to FUN
#'
#' @return  Hourly data time Series
#'
#' @export

create_hourly_xts <- function(data, hours = 1, FUN = sum, start_date = NULL, end_date = NULL, ...) {
  aggregate_xts(data, on="hours", k = hours, FUN = FUN, na_handling = "auto", align_data = TRUE, start_date = start_date, end_date = end_date, ...)
}

#' Aggregate data to a daily time-series
#'
#' @param   data        Input \code{\link{xts}} object.
#' @param   FUN         Function to aggregate the data with. By default \code{sum} is used.
#' @param   start_date   First Date in the regular series. Calculated from the given series if NULL. Its type must match the type of \code{data}'s index.
#' @param   end_date     Last Date in the regular series. Calculated from the given series if NULL. Its type must match the type of \code{data}'s index.
#' @param   ...         Other parameters given to FUN
#'
#' @return  Daily data time Series
#'
#' @export

create_daily_xts <- function(data, FUN = sum, start_date = NULL, end_date = NULL, ...) {
  aggregate_xts(data, on="days", k = 1, FUN = FUN, na_handling = "auto", align_data = TRUE, start_date = start_date, end_date = end_date, ...)
}


#' Aggregate xts matrix data to the given time granularity
#'
#' @param   data         Input \code{\link{xts}} object.
#' @param   on           Determines the new unit of the data frequency. Can be "minutes", "hours", or "days".
#' @param   k            The new value of frequency (only for hours and minutes).
#' @param   FUN          Function to aggregate the data with. Either a closure (function), or a string.
#'                       Strings currently supported are "sum", "prod", "min", "max", last", and "first".
#'                       By default \code{sum} is used.
#' @param   na_handling  What to do with NA's. "auto" fills 0 if FUN == sum, na.locf's last value else.
#'                       NULL does nothin to NAs. If a function is given, it is applied directly.
#' @param   align_data   Align data to the next truncated time-stamp
#' @param   start_date   First Date in the regular series. Calculated from the given series if NULL. Its type must match the type of \code{data}'s index.
#' @param   end_date     Last Date in the regular series. Calculated from the given series if NULL. Its type must match the type of \code{data}'s index.
#' @param   ...          Other parameters given to FUN
#'
#' @return  Aggregated Time-series
#'
#' @export

aggregate_xts_matrix <- function(data, on, k = 1, FUN = "sum", na_handling = "auto", align_data = TRUE, start_date = NULL, end_date = NULL, ...) {

  # aggregate each column
  agg_list = lapply(1:ncol(data), function(i) {
    aggregate_xts(data=data[,i], on=on, k=k, FUN=FUN, na_handling=na_handling, align_data=align_data, start_date=start_date, end_date=end_date, ...)
  })

  # concatenate columns
  ret = do.call(cbind, agg_list)

  return(ret)
}
