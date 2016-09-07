context("Crossvalidation tools")

set.seed(0)

n = 20
best_model = xts(1:25 + 0.5, as.POSIXct(timeSequence(from = "2010-01-01", "2010-01-25")))
train = xts(as.numeric(1:n) + runif(n), as.POSIXct(timeSequence(from = "2010-01-01", "2010-01-20")))

ln <- function(f) lm(x ~ ., f)

fx1 <- function(x) {r = na.omit(cbind(x, lag(x))); colnames(r) = c("x", "feat1"); r}
fcster1 <- mltsp_forecaster(NULL, fx1, ln)
a1 = ts_crossval(train, fcster1, horizon = 3, initial_window = 8, verbose = FALSE)

fx2 <- function(x) {r = na.omit(cbind(x, lag(x, 1), lag(x, 2))) ; colnames(r) = c("x", "feat1", "feat2") ; r}
fcster2 <- mltsp_forecaster(NULL, fx2, ln)
a2 = ts_crossval(train, fcster2, horizon = 3, initial_window = 8, verbose = FALSE)

fx3 <- function(x) {r = na.omit(cbind(x, lag(x, 1), lag(x, 2), lag(x, 3))) ; colnames(r) = c("x", "feat1", "feat2", "feat3"); r}
fcster3 <- mltsp_forecaster(NULL, fx3, ln)
a3 = ts_crossval(train, fcster3, horizon = 3, initial_window = 8, verbose = FALSE)

results =rbind(data.frame(Model="Lags = 1", Err=a1),
               data.frame(Model="Lags = 1,2", Err=a2),
               data.frame(Model="Lags = 1,2,3", Err=a3))

test_that("Worst and best models are found correctly", {
  expect_equal(which.max(results$Err), 3) # the worst is the one with too many params
  expect_equal(which.min(results$Err),  1)  # the worst is the one with 1 param
})

######
# Test batch mode
a1 = ts_crossval(train, fcster1, horizon = 5, initial_window = 10)
a2 = ts_crossval(train, fcster1, horizon = 5, initial_window = 10, break_batch_size = 2)
a3 = ts_crossval(train, fcster1, horizon = 5, initial_window = 10, break_batch_size = 4)

test_that("CV batch mode without setting batch size works correctly", {
  expect_equal(a1, a2)
  expect_equal(a1, a3)
})

# The early break, because of default settings
a4 = ts_crossval(train, fcster1, horizon = 5, initial_window = 10, break_err = 3)
test_that("CV early break works correctly", {
  expect_lt(a4, 3.6)
})

# earlier breaks, when batch size is set
a5 = ts_crossval(train, fcster1, horizon = 5, initial_window = 10, break_err = 3, break_batch_size = 2)
test_that("CV early break with batch works correctly", {
  expect_lt(a5, 4.94)
})
