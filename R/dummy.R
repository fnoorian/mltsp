#' Create dummy variables for day of week
#'
#' @param indices       Input time-stamps
#' @param no_dependency Remove linear dependency between variables
#'
#' @return An xts object
#'
#' @importFrom xts xts
#' @importFrom lubridate wday
#' @export

binary_day_of_week_matrix <- function(indices, no_dependency = TRUE) {
  ret = xts(matrix(0, length(indices), 7), indices)
  colnames(ret) = paste0("DAY", 1:7)

  index_wdays = wday(indices)
  for (i in seq_along(indices)) {
    ret[i, index_wdays[i]] = 1
  }

  return (ret)
}

#' Create dummy variables for hour of day
#'
#' @param indices       Input time-stamps
#' @param no_dependency Remove linear dependency between variables
#'
#' @return An xts object
#'
#' @importFrom xts xts
#' @importFrom lubridate hour
#' @export
binary_hour_matrix <- function(indices, no_dependency = TRUE) {
  ret = xts(matrix(0, length(indices), 24), indices)
  colnames(ret) = paste0("HOUR", 0:23)

  index_hours = hour(indices)

  for (i in seq_along(indices)) {
    ret[i, index_hours[i] + 1] = 1
  }

  return (ret)
}
