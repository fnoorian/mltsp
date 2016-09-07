#' Simple time-series cross-validation
#'
#' @param y           The time-series
#' @param func        Forecasting function. See details.
#' @param ...         Forcasting function
#' @param slices      Cross-validation slices, created by \code{\link{create_cv_timeslices}}
#' @param error_func  Error function for reporting cross-validation error.
#' @param break_err   If CV error passes this value, cross-validation is prematurely stopped.
#' @param verbose     If True, verbose debugging messages are printed out.
#'
#' @return Cross-validation error for the given forecast function
#'
#' @details
#' \code{func} expects a signature of type \code{function(y, h, ...)}, where \code{y} is the time-series to forecast,
#' \code{h} is the horizon to forecast, and \code{...} is any other parameter.
#'
#' Use \code{break_err} when you have to evaluate many functions quickly.
#' By setting this value to CV from a known good predictor, bad forecasters can be pruned quickly.
#'
#' @importFrom Metrics rmse
#'
ts_crossval_simple <- function(y, func, ...,
                              slices,
                              error_func=rmse,
                              break_err = Inf,
                              verbose = FALSE) {


  measure = 0
  for (i in seq_along(slices$train)) {

    train.ts = y[slices$train[[i]]]
    test.ts = y[slices$test[[i]]]

    fc = func(train.ts, h = length(test.ts), ...)

    err = error_func(fc, test.ts)

    if (is.infinite(err) |
        is.na(err) |
        is.nan(err)) {
      return (err)
    }

    measure = measure + err
    if (measure > break_err) {
      return (measure)
    }

    if (verbose) {
      print(paste("CV:", i, "of", length(slices$train), "Err: ", err))
    }
  }

  return (measure)
}

#' Time-series cross-validation
#'
#' @param y                 The time-series
#' @param func              Forecasting function. See details.
#' @param ...               Forcasting function parameters.
#' @param initial_window  initial number of consecutive values in each training set sample
#' @param horizon        forecat horizon in cross-validation. Number of consecutive values in test set sample
#' @param fixed_window    if TRUE, training window size is fixed to \code{initial_window}.
#'                          If \code{FALSE}, all available data from the start until testing point is used.
#' @param skip_slice           How many slices are to be skip_sliceped and not used for cross-validation.
#' @param slices            Cross-validation slices
#' @param error_func        Error function for reporting cross-validation error.
#' @param break_err         If CV error exceeds this value, cross-validation is prematurely stopped. See details.
#' @param break_err_batch   If CV error in each batch exceeds this value, cross-validation is prematurely stopped. See details.
#' @param break_batch_size  Batch size for \code{break_err_batch} and \code{plapply}. See details
#' @param plapply           Parallel processing apply function for each batch. \code{lapply} by default. See details.
#' @param verbose           If True, verbose debugging messages are printed out.
#'
#' @return Cross-validation error for the given forecast function
#'
#' @details
#' \code{func} expects function signature \code{function(y, h, ...)}, where \code{y} is the time-series to forecast,
#' \code{h} is the horizon to forecast, and \code{...} is any other parameter.
#'
#' \code{error_func} expects function signature \code{function(x, y)}, where \code{x} and \code{y} are both
#' time-series. It should return a number, which must be accumulative.
#'
#' Use \code{break_err} when you have to evaluate many functions quickly.
#' By setting this value to CV from a known good predictor, bad forecasters can be pruned quickly.
#' This value is only effective is \code{break_batch_size} is given.
#'
#' \code{plapply} is used to parallelize execution of \code{func}.
#' If \code{break_batch_size} is given, slices are divided to batches, and given to \code{plapply}.
#' This will be used in conjunction with \code{break_err} and \code{break_err_batch},
#' as parallel processing apply functions are often designed to run continously and
#' do not allow their results to be checked before all of the computation is done.
#' \code{break_err_batch} is specifically used to break the cross-validation operation
#' if the error in the current batch exceeds the given threshold.
#'
#' @importFrom Metrics rmse
#' @export

ts_crossval <- function(y, func, ...,
                        initial_window,
                        horizon = 1,
                        fixed_window = TRUE,
                        skip_slice = 0,
                        slices = create_cv_timeslices(y, initial_window = initial_window,
                                                      horizon = horizon,
                                                      fixed_window = fixed_window,
                                                      skip = skip_slice),
                        error_func=rmse,
                        break_err = Inf,
                        break_err_batch = Inf,
                        break_batch_size = NA,
                        plapply = NULL,
                        verbose = FALSE) {

  # Defailt values for
  if (is.null(plapply)) {
    plapply = lapply
    if (!is.finite(break_batch_size)) {
      break_batch_size = 1
    }
  }

  if ((is.finite(break_err) | is.finite(break_err_batch)) & (!is.finite(break_batch_size ))) {
    warning("Break error is given, but batch size is not.")
  }

  # find slices
  slice_inds = seq_along(slices$train)

  # split slices to different batches, or one big batch
  if (!is.na(break_batch_size)) {
    slice_batch_splits = split(slice_inds, ceiling(slice_inds/break_batch_size))
  } else {
    slice_batch_splits = list(slice_inds)
  }

  measure = 0
  worst_batch_measure = NA

  for (slice_batch in slice_batch_splits) {

    # apply error function to the current batch
    batch_measures = plapply(slice_batch, function(i){
      train.ts = y[slices$train[[i]]]
      test.ts = y[slices$test[[i]]]

      fc = func(train.ts, h = length(test.ts), ...)

      err = error_func(fc, test.ts)

      if (verbose) {
        print(paste("CV:", i, "of", length(slices$train), "Err: ", err))
      }

      err
    })

    if (!all(sapply(batch_measures, is.numeric))) {
      stop("plapply function returned non-numeric value.")
    }

    batch_measures = sum(as.numeric(batch_measures))
    measure = measure + batch_measures

    # check if the measure is still a finite number, otherwise terminate
    if (is.infinite(measure) |
        is.na(measure) |
        is.nan(measure)) {
      return (measure)
    }

    # keep the worst_bacth_measure
    if (is.na(worst_batch_measure)) {
      worst_batch_measure = batch_measures
    } else {
      worst_batch_measure = max(worst_batch_measure, batch_measures)
    }

    # CV error early break
    if (measure > break_err) {
      if (verbose) {
        print(paste("CV: Total error break: Err =", measure))
      }
      return (measure)
    }

    # batch error early break
    if (batch_measures > break_err_batch) {
      if (verbose) {
        print(paste("CV: Batch error break: Err =", batch_measures))
      }

      return (Inf)
    }
  }

  # TODO: later, return worst_batch_measure too
  return (measure)
}
