# Non-linear time-series forecasting tools
# Offers an interface similar to package forecast

#' Build a nonlinear auto-regressive time-series forecasting with exogenous input
#'
#' @param learner_func  Non-linear learning function
#' @param p             Auto-regressive order
#' @param P             Seasonal auto-regressive order
#' @param d             Model differencing order
#' @param D             Seasonal differencing order
#' @param freq          Time-series periodicity
#' @param lambda        Box-Cox transform's lambda
#' @param ...           Extra parameters passed to \code{learner_func}
#'
#' @return A NARX model object
#'
#' @seealso  \code{\link{narx}}
#' @export
build_narx <- function(learner_func, p, P = 0, d = 0, D = 0, freq = 1, lambda = NULL, ...) {

  learner.name = deparse(substitute(learner_func))

  ret = list(lambda = lambda,
             learner = learner_func,
             learner.name = learner.name,
             order = list(p = p, P = P, d = d, D = D, freq = freq),
             method = paste0("NARX ", learner.name,  "(", p, ", ", d, ")(", P, ", ", D, ")[", freq, "]")
  )

  return (structure(ret, class="narx"))
}

#' Nonlinear auto-regressive time-series forecasting with exogenous input
#'
#' @param x             Time-series input
#' @param ...           Extra parameters passed to \code{learner_func}
#'
#' @return A fitted NARX object
#'
#' @seealso  \code{\link{build_narx}}
#'
#' @export
narx <- function(...)
  UseMethod("narx")

#' @method summary narx
#' @export
summary.narx <- function(object, ...) { object$method}

#' @method print narx
#' @export
print.narx <- function(x, ...) {
  cat(summary(x), ...)
}

#' @name narx
#'
#' @param model         NARX model. A \code{narx} object.
#' @param xreg          External regression data
#'
#' @export
narx.narx <- function(model, x, xreg = NULL, ...) {
  ret = narx.default(x,
                     learner_func = model$learner,
                     p = model$order$p,
                     P = model$order$P,
                     d = model$order$d,
                     D = model$order$D,
                     freq = model$order$freq,
                     xreg = xreg,
                     lambda = model$lambda, ...)

  # replace strings which may get corrupted in cross function calls
  ret$y_name = deparse(substitute(x))
  ret$method = model$method
  ret$learner.name = model$learner.name

  return(ret)
}

#' @name narx
#'
#' @param learner_func  Non-linear learning function
#' @param p             Auto-regressive order
#' @param P             Seasonal auto-regressive order
#' @param d             Model differencing order
#' @param D             Seasonal differencing order
#' @param freq          Time-series periodicity
#' @param lambda        Box-Cox transform's lambda
#'
#' @export
#' @importFrom stats predict
#' @importFrom zoo coredata
#' @importFrom xts is.xts
#' @importFrom forecast BoxCox BoxCox.lambda
#' @importFrom utils head tail
narx.default <- function(x, learner_func, p, P = 0, d = 0, D = 0, freq = 1, xreg = NULL, lambda = NULL, ...) {

  # sanity check
  freq = floor(freq)
  if (max(p, P*freq) + D*freq + d > length(x)) {
    stop("Not enough data for the current model order.")
  }

  if (!is.null(xreg)) { # xreg must be a time-series or match length of x
    if(!(is.ts(xreg) | is.xts(xreg))) {
      if (.nsamples(xreg) != .nsamples(x)) {
        stop("If xreg is not a time-series, its length must match x.")
      }
    }

    xreg_name = deparse(substitute(xreg))
    if (is.null(colnames(xreg))) {
      colnames(xreg) = paste0(xreg_name, '.', 0:(ncol(xreg)-1))
    }
  }

  # keep the original x
  x_name = deparse(substitute(x))
  original_x = x
  x = as.numeric(coredata(x))

  # apply boxcox transform
  if (!is.null(lambda)) {
    if (lambda == "auto") {
      lambda = BoxCox.lambda(x)
    }
    x = BoxCox(x, lambda=lambda)
  }

  # apply the difference
  x0D = NULL
  x0d = NULL
  if ((D*freq) > 0) {
    x0D = tail(x, D*freq)
    x = diff(x, lag=freq, differences = D)
  }
  if (d > 0) {
    x0d = tail(x, d)
    x = diff(x, lag = 1, differences = d)
  }

  # create Lag window for training
  dtl = lag_windows(x, p=p, P=P, freq=freq, no_peeking = FALSE, pstr=x_name)

  if (.nsamples(dtl) != .nsamples(x)) {
    stop("Data lost in NARX pre-processing transformations")
  }

  # add external inputs
  if (!is.null(xreg)) {

    # subset the xreg where it matches the training data
    if (is.ts(xreg) | is.xts(xreg)) {
      index_x = tail(index(original_x), nrow(dtl))
      xreg_subset = .ts.subset(xreg, index_x)
    } else {
      xreg_subset = tail(xreg, nrow(dtl))
    }

    if (.nsamples(xreg_subset) != .nsamples(dtl)) {
      stop("Data lost in NARX exogenous data transformations")
    }

    xreg_subset = coredata(xreg_subset)

    dtl = cbind(dtl, xreg=xreg_subset)
  }

  # separate input and target
  input = dtl[,-1]
  target = dtl[,1]

  mdl = learner_func(input, target, ...)

  # for compatibility with forecast, compute fitted (one-step ahead)
  fitted = predict(mdl, input)
  fitted = c(rep(mean(x), length(x) - length(fitted)), fitted) # pad data with a "mean" prediction if required
  fitted = as.numeric(fitted)

  # compute residuals
  residuals = x - fitted
  residuals = as.numeric(residuals)

  # get the proper name for the mode
  learner.name = deparse(substitute(learner_func))

  ret = list(lambda = lambda,
             learner = learner_func,
             learner.name = learner.name,
             order = list(p = p, P = P, d = d, D = D, freq = freq),
             method = paste0("NARX ", learner.name,  "(", p, ", ", d, ")(", P, ", ", D, ")[", freq, "]"),
             x = original_x,
             xname = x_name,
             model = mdl,
             diff.x0 = list(d = x0d, D = x0D),
             fitted = fitted,
             residuals = residuals)

  return (structure(ret, class="narx"))
}

#' Predict a time-series using a NARX object
#'
#' @param object	An object of class "narx". Usually the result of a call to \code{\link{narx}}.
#' @param x	      A time-series to forecast
#' @param h	      Number of periods for forecasting. If \code{xreg} is used, \code{h} is ignored and the number of forecast periods is set to the number of rows of \code{xreg}.
#' @param xreg	  Future values of an exogenous regression variables. Overrides \code{h} and its timestamps are used for prediction time-stamps.
#' @param lambda	Box-Cox transformation parameter. Ignored if \code{NULL}. Otherwise, forecasts back-transformed via an inverse Box-Cox transformation.
#' @param ...     Inputs to the learning function's \code{\link{predict}}
#'
#' @return The predicted time-series
#'
#' @importFrom stats na.omit predict is.ts
#' @importFrom zoo index
#' @importFrom xts is.xts
#' @method predict narx
#' @export

predict.narx <- function(object, x, h = 10, xreg=NULL, lambda = object$lambda, ...) {

  if (is.null(object$model)) {
    stop("NARX learner model is empty.")
  }

  # override h if xreg present
  if (!is.null(xreg)) {
    h = .nsamples(xreg)
  }

  # find the optimal time-stamps
  pred_timestamps = NULL
  if (!is.null(xreg)) {
    if (is.ts(xreg) | is.xts(xreg)) {
      pred_timestamps = index(xreg)
    }
  }

  if (is.null(pred_timestamps)) {
    pred_timestamps = continue_timestamps(index(x), h)
  }

  # set xreg column names
  if (!is.null(xreg)) {
    xreg_name = deparse(substitute(xreg))
    if (is.null(colnames(xreg))) {
      colnames(xreg) = paste0(xreg_name, '.', 0:(ncol(xreg)-1))
    }
  }

  # prepare parameters
  order = object$order
  pred = rep.int(NaN, h)

  # keep the original x
  x_name = deparse(substitute(x))
  original_x = x
  x = as.numeric(coredata(x))

  # prepare time-series
  x0D = NULL
  x0d = NULL
  if ((order$D*order$freq) > 0) {
    x0D = tail(x, order$D*order$freq)
    x = na.omit(diff(x, lag=order$freq, differences = order$D))
  }
  if (order$d > 0) {
    x0d = tail(x, order$d)
    x = na.omit(diff(x, lag = 1, differences = order$d))
  }

  # prepare forecast data
  win = max(order$p, order$P * order$freq)
  last.dt = c(tail(x, win), NaN)

  for (i in 1:h) {
    last.dtl = lag_windows(last.dt, p=order$p, P=order$P, freq=order$freq, no_peeking = FALSE, pstr = object$xname)
    last.dtl = tail(last.dtl[,-1], 1)

    # add external inputs
    if (!is.null(xreg)) {

      # subset the xreg where it matches the training data
      if (is.ts(xreg) | is.xts(xreg)) {
        xreg_subset = .ts.subset(xreg, pred_timestamps[i])
      } else {
        xreg_subset = as.matrix(xreg)[i,]
      }

      if (.nsamples(xreg_subset) != .nsamples(last.dtl)) {
        stop("Data lost in NARX exogenous data transformations")
      }

      last.dtl = cbind(last.dtl, xreg=coredata(xreg_subset))
    }

    # do the prediction
    newdata = last.dtl
    pred.val = predict(object$model, newdata = newdata, ...)
    last.dt = c(head(last.dt[-1], -1), pred.val, NaN)
    pred[i] = pred.val
  }

  # inverse the difference
  if (order$d > 0) {
    pred = tail(diffinv(pred, lag=1, differences=order$d, xi=x0d), h)
  }
  if ((order$D*order$freq) > 0) {
    pred = tail(diffinv(pred, lag=order$freq, differences=order$D, xi=x0D), h)
  }

  # inverse the boxcox
  if (!is.null(lambda)) {
    pred = InvBoxCox(pred, lambda)
  }

  # add timestamp to pred
  pred = ts_continue(original_x, pred)

  return (pred)
}


#' Forecast a NARX object
#'
#' @param object	An object of class "narx". Usually the result of a call to \code{\link{narx}}.
#' @param h	      Number of periods for forecasting. If \code{xreg} is used, \code{h} is ignored and the number of forecast periods is set to the number of rows of \code{xreg}.
#' @param level	  Not used
#' @param fan	    Not used
#' @param xreg	  Future values of an exogenous regression variables.
#' @param lambda	Box-Cox transformation parameter. Ignored if \code{NULL}. Otherwise, forecasts back-transformed via an inverse Box-Cox transformation.
#' @param ...     Inputs to the learning function's \code{\link{predict}}
#'
#' @return A \code{\link{forecast}} package compatible \code{forecast} object
#'
#' @importFrom stats fitted residuals predict
#' @method forecast narx
#' @export

forecast.narx <- function(object, h = 10, level=c(80,95), fan=FALSE, xreg=NULL, lambda = object$lambda, ...) {

  pred = predict(object, object$x, h=h, xreg=xreg, lambda=lambda, ...)

  # convert to forecast compatible class
  ret <- list(method=object$method,
              x=object$x,
              xname=object$name,
              mean=pred,
              level=level,
              lower=matrix(NA,nrow=h,ncol=length(level)),
              upper=matrix(NA,nrow=h,ncol=length(level)),
              fitted=fitted(object),
              residuals=residuals(object))

  ret$model=object
  ret$model$x = NULL
  ret$model$xname = NULL
  ret$model$diff.x0 = NULL
  ret$model$fitted = NULL
  ret$model$residuals = NULL

  return(structure(ret,class="forecast"))
}
