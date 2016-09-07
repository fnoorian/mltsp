# Tools for extending native ts set to match xts

# Implementation of head for time-series
#' @importFrom stats window start end frequency tsp
#' @method head ts
#' @export
head.ts <- function(x, n = 5, ...) {
  len_x = dim(as.matrix(x))[1]
  if (n > len_x) {
    return(x)
  } else if (n == 0) {
    return (ts())
  } else if (n < 0) {
    # update n to be 1:(len_x - abs(n))
    n = len_x + n
  }

  return(window(x, start = start(x), end = start(x) + c(0, n - 1),
                  frequency=tsp(x)[3]))
}

# Implementation of tail for time-series
#' @method tail ts
#' @export
tail.ts <- function(x, n = 5, ...) {
  len_x = dim(as.matrix(x))[1]
  if (n > len_x) {
    return(x)
  } else if (n == 0) {
    return (ts())
  } else if (n < 0) {
    # update n to be (len_x - abs(n)):(len_x)
    n = len_x + n
  }

  return(window(x, start = end(x) - c(0, n - 1), end = end(x),
                  frequency = tsp(x)[3]))
}

# implementation of c for ts that doesn't destroy its timestamps
#' @method c ts
#' @export
c.ts <- function(..., recursive = FALSE) {

  # First item determines everything
  vals = list(...)
  a = vals[[1]]

  cc = do.call(c, lapply(vals, as.numeric))

  return (ts(cc,
             start = start(a),
             frequency = frequency(a),
             deltat = deltat(a)))
}
