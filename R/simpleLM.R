
#' A Simple linear model.
#'
#' Replaces lm where X and y are separate
#'
#' @param X    Input data for training
#' @param y    Target (output) data for training. If \code{NULL}, first column of X is used.
#' @param ...  Other parameters passed to \code{\link{lm}}
#'
#' @return  a \code{\link{lm}} model
#'
#' @importFrom stats lm
#' @export
SimpleLM <- function(X, y = NULL, ...) {

  if (is.null(y)) {
    Z = as.data.frame(X)
  } else {
    Z = as.data.frame(cbind(y, X))
  }

  colnames(Z)[1] = ".lm.target"

  mdl = lm(.lm.target ~ ., Z, ...)
  class(mdl) = c("SimpleLM", "lm")

  # mdl$has_y_column = !is.null(y)

  mdl
}

#' @name SimpleLM
#'
#' @param object  A \code{SimpleLM} object
#' @param newdata  Input data for prediction
#'
#' @importFrom stats predict
#'
#' @method predict SimpleLM
#' @export
predict.SimpleLM <- function(object, newdata, ...) {
  class(object) = "lm"

  # if (mdl$has_y_column) {
  #   newdata = newdata[, -1]
  # }

  predict(object, as.data.frame(newdata))
}
