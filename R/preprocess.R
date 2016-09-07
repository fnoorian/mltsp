# Preprocessing Time-series

#' Preprocess a time-series
#'
#' @param x                Input time-series
#' @param technique_list   List of the techniques. See details.
#'
#' @return A list
#' \itemize{
#'  \item{\code{x}}{Preprocessed xts}
#'  \item{\code{technique_list}}{List of techniques with their explicit parameters}
#' }
#'
#' @details Each technique is itself a list of \code{list("techninque name", param1, param2, ...)}.
#' Available techniques include: \itemize{
#'  \item{"diff"}{Differencing the time-series. Parameter can be a number, "kpss", "adf", or "auto".}
#'  \item{"boxcox"}{Box-cox transform. Parameter can be a numeric lambda, or "auto".}
#'  \item{"log"}{Log transform. Only works if time-series is positive.}
#'  \item{"log_abs"}{Log transform on absolute value. Destroys values between -1 and 1.}
#'  \item{"log+min"}{Log transform on (x - min(x) + 1)}
#' }
#'
#' @importFrom forecast ndiffs BoxCox.lambda BoxCox
#' @importFrom stats na.omit
#'
#' @export
ts_preprocess <- function(x, technique_list = list()) {

  # remove empty techniques from list
  technique_list = technique_list[!sapply(technique_list, is.null)]

  for (i in seq_along(technique_list)) {
    technique = technique_list[[i]]

    type = tolower(technique[[1]])
    if (type == "diff") {
    ## The difference

      # get the parameter
      d = technique[[2]]
      if (!is.numeric(d)) {
        if (d == "auto") {
          d = "kpss"
        }

        d = tolower(d)
        technique_list[[i]]$original_d = d

        if (d == "kpss" || d == "adf") {
          d = ndiffs(x, test = d)
        }
      }

      # do the transform
      technique_list[[i]][[2]] = d

      if (d > 0) {
        technique_list[[i]]$x0 = x[1:d]
        x = na.omit(diff(x, differences = d))
      }

    } else if (type == "boxcox") {
      ## The coxbox transform

      # get the parameter
      lambda = technique[[2]]
      if (!is.numeric(lambda)) {
        lambda = BoxCox.lambda(x)
      }

      # do the transform
      technique_list[[i]][[2]] = lambda
      x = BoxCox(x, lambda)

    } else if (type == "log") {
      ## The log transform

      if (any(x <= 0)) {
        stop("Log prerprocessing: Time-series has to be positive.")
      }
      x = log(x)
    } else if (type == "log_abs") {
      ## The log transform on absolute value
      # destroys values between -1 and 1
      x = ifelse(x > 1, log(x),
            ifelse(x < -1, log(-x),
                           0))
    } else if (type == "log+min") {
      ## The log transform on (x - min(x) + 1)
      min_x = min(x)
      technique_list[[i]] = list(type, min_x)
      x = x - min_x + 1
      x = log(x)
    }
  }

  return (list(x = x, technique_list = technique_list))
}

#' Inverse preprocessing of  a time-series
#'
#' @param x                Preprocessed time-series
#' @param technique_list   List of the techniques. Usually taken from output of \code{\link{ts_preprocess}}
#'
#' @return An xts object
#'
#' @importFrom forecast InvBoxCox
#' @importFrom stats diffinv
#' @importFrom xts xts is.xts
#' @export
ts_inv_preprocess <- function(x, technique_list) {

  for (i in rev(seq_along(technique_list))) {
    technique = technique_list[[i]]
    type = tolower(technique[[1]])

    ## The difference
    if (type == "diff") {

      # get the parameter
      d = technique[[2]]

      # only for really applied differences
      if (d > 0) {
        x0 = technique$x0

        # reverse the transform
        rx = diffinv(x, differences=d, xi=x0)

        if (is.xts(x)) {
          x = xts(rx, c(index(x0), index(x)))
        } else {
          x = rx
        }
      }
      ## The coxbox transform
    } else if (type == "boxcox") {

      # get the parameter
      lambda = technique[[2]]

      # reverse the transform
      x = InvBoxCox(x, lambda)

    } else if (type == "log") {

      # reverse the transform
      x = exp(x)
    } else if (type == "log_abs") {
      ## The log transform on absolute value
      # destroys values between -1 and 1
      x = ifelse(x > 1, exp(x),
            ifelse(x < -1, exp(-x),
                           0))
    } else if (type == "log+min") {
      ## The log transform on value - minimum (shift min to 1)
      min_x = as.numeric(technique[[2]])
      x = exp(x) + min_x - 1
    }
}

  return (x)
}
