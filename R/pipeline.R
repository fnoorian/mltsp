# Pipeline object

#' Create an ML TSP pipeline object
#'
#' @param preproc     list of preprocessing technique tuples
#' @param featext     a function to extract features from a time-series
#' @param learner     a function to create the ML model
#' @param predictor   ML prediction function (\code{\link{predict}} by default)
#'
#' @return An MLTS pipeline object
#'
#' @seealso \code{\link{mltsp}}, \code{\link{mltsp_forecaster}}
#' @export
build_mltsp_pipline <- function(preproc = NULL, featext, learner, predictor = predict) {

  # create the model
  model = list(
    preproc = preproc,
    featext = featext,
    learner = learner,
    predictor = predictor)

  class(model) = "mltsp_pipeline"

  return(model)
}

#' Create an ML TSP object
#'
#' @param x        xts time-series for training
#' @param ...      Other parameters passed to feature extraction, learning, and prediction function
#'
#' @return A fitted ML TSP model
#'
#' @seealso \code{\link{build_mltsp_pipline}}, \code{\link{mltsp_forecaster}}
#' @export
mltsp <- function(...)
  UseMethod("mltsp")

#' @name mltsp
#'
#' @param object   A mltsp object
#'
#' @method mltsp mltsp
#' @export
mltsp.mltsp <- function(object, x, ...) {

  mltsp(object$pipeline, x,  ...)
}

#' @name mltsp
#'
#' @param preproc     list of preprocessing technique tuples
#' @param featext     a function to extract features from a time-series
#' @param learner     a function to create the ML model
#' @param predictor   ML prediction function (\code{\link{predict}} by default)
#'
#' @method mltsp default
#' @export
mltsp.default <- function(x, preproc=NULL, featext, learner, predictor = predict, ...) {

  pipeline = build_mltsp_pipline(preproc=preproc, featext=featext, learner=learner, predictor=predictor)
  mltsp(pipeline, x, ...)
}

#' @name mltsp
#'
#' @param pipeline  A ML TSP pipeline object
#'
#' @method mltsp mltsp_pipeline
#' @export
mltsp.mltsp_pipeline <- function(pipeline, x, ...) {

  # do the preprocessing
  preproc = ts_preprocess(x, pipeline$preproc, ...)

  # extract features
  feats = pipeline$featext(preproc$x, ...)

  feats = as.data.frame(feats) # a cast to data.frame is necessary for
                               # formula based learners

  if (nrow(feats) < 2) {
    # Check your features, you may have too many NA's generated
    stop("Not enough features for training ML TS model.")
  }

  # build model
  ts.model = pipeline$learner(feats, ...)

  # create object
  ret = list(pipeline = pipeline,
             x = x,
             preproc = preproc,
             model = ts.model)
  class(ret) = "mltsp"

  return(ret)
}

#' Machine learning pipeline forecast
#'
#' Takes a \code{\link{mltsp}} object as an input. The machine learning should be configured in this object.
#'
#' @param object           A mlts model
#' @param h                Horizon of forecast
#' @param test_timestamps  Time-stamps of forecast. Will be generated automatically if \code{NULL}.
#' @param ...              Extra parameters passed to feature extraction, prediction, and inverse pre-processing in the pipeline
#'
#' @method forecast mltsp
#' @export
forecast.mltsp <- function(object, h, test_timestamps = NULL, ...) {

  # n-step ahead rolling forecast
  train.ts = object$preproc$x
  pred = NULL

  for (j in 1:h) {
    train.with.dummy = ts_append(train.ts, -1, test_timestamps[j])
    feats = as.data.frame(object$pipeline$featext(train.with.dummy, ...))

    # only predict the last time-stamp
    p = object$pipeline$predictor(object$model, tail(feats, 1), ...)
    train.ts = ts_append(train.ts, as.numeric(p), test_timestamps[j])
  }

  # reverse preprocessing
  reversed.pred = ts_inv_preprocess(train.ts, object$preproc$technique_list, ...)

  # return last h samples
  last.h.pred = tail(reversed.pred, h)

  return (last.h.pred)
}

#' Functional version of the mlts
#'
#' @param preproc    list of preprocessing technique tuples
#' @param featext    a function to extract features from a time-series
#' @param learner    a function to create the ML model
#' @param predictor  forecast function (\code{\link{predict}} by default)
#' @param ...        Other parameters passed to \code{\link{build_mltsp_pipline}}
#'
#' @return  A \code{function(x, h, ...)} that takes a time-series \code{x} and a horizon \code{h},
#' and returns the forecast as another time-series.
#'
#' @seealso \code{\link{build_mltsp_pipline}}, \code{\link{mltsp}}
#' @importFrom forecast forecast
#' @export
mltsp_forecaster <- function(preproc=NULL, featext, learner, predictor = predict, ...) {

  pipeline <- build_mltsp_pipline(preproc, featext, learner, predictor, ...)

  forecaster <- function(x, h, ...) {
    model = mltsp(pipeline, x)
    forecast(model, h, ...)
  }

  return(forecaster)
}

