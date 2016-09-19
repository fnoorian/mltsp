# Period apply tools for xts objects

#' Period apply with string function
#'
#' Period.apply with last function
#'
#' @param x a univariate data object
#' @param INDEX a numeric vector of endpoints to calculate maximum on
#'
#' @return An xts or zoo object of maximums, indexed by the period endpoints.
period.first <- function(x, INDEX) {
  len.x = length(x)

  INDEX = INDEX + 1
  INDEX = INDEX[INDEX <= len.x]

  x[INDEX]
}

#' Period apply with string function
#'
#' Period.apply with last function
#'
#' @param x a univariate data object
#' @param INDEX a numeric vector of endpoints to calculate maximum on
#'
#' @return An xts or zoo object of maximums, indexed by the period endpoints.
period.last <- function(x, INDEX) {
  x[INDEX]
}


#' Period apply with string function
#'
#' xts's period.apply is optimized for a few function. The current function takes these as
#' an arguement, and then calls the correct function.
#'
#' @param x a univariate data object
#' @param INDEX a numeric vector of endpoints to calculate maximum on
#' @param FUN A string. Currently supported are "sum", "prod", "min", "max", last", and "first".
#'
#' @return An xts or zoo object of maximums, indexed by the period endpoints.
#'
#' @export
period.apply.string <- function(x, INDEX, FUN) {

  # check FUN to be a valid closure or character
  if (is.character(FUN)) {
    if (! FUN %in% c("sum", "prod", "min", "max", "last", "first")) {
      stop("Function not supported.")
    }
  }

  # call the period function
  func.name = paste0("period.", FUN)
  ret = do.call(func.name, list(x = x, INDEX = INDEX))
  colnames(ret) = colnames(x)

  return(ret)
}

