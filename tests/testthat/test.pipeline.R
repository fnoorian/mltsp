library("forecast")

context("Pipeline")

test_that("mlts works for xts", {
  library("xts")
  train = xts(as.numeric(1:20), timeSequence(from = "2010-01-01", "2010-01-20"))

  fx <- function(x) {r = na.omit(cbind(x, lag(x))); colnames(r) = c("x", "y"); r}
  ln <- function(f) lm(x ~ ., f)
  mlts.spec <- build_mltsp_pipline(NULL, fx, ln)

  model = mltsp(mlts.spec, train)
  y = forecast(model, 5)

  y_ref = xts(as.numeric(21:25), timeSequence(from = "2010-01-21", "2010-01-25"))
  expect_equal(y, y_ref)

  forecaster <- mltsp_forecaster(NULL, fx, ln)
  y2 = forecaster(train, 5)
  expect_equal(y2, y_ref)
})

test_that("mlts works for xts with preprocessing", {

  library("xts")
  train = xts(exp((1:20)/5), timeSequence(from = "2010-01-01", "2010-01-20"))

  fx <- function(x) {r = na.omit(cbind(x, lag(x))); colnames(r) = c("x", "y"); r}
  ln <- function(f) lm(x ~ ., f)
  pp <- list(list("log"))
  mlts.spec <- build_mltsp_pipline(pp, fx, ln)

  model = mltsp(mlts.spec, train)
  y = forecast(model, 5)
  y_ref = xts(exp((21:25)/5), timeSequence(from = "2010-01-21", "2010-01-25"))
  expect_equal(y, y_ref)

  forecaster <- mltsp_forecaster(pp, fx, ln)
  y2 = forecaster(train, 5)
  expect_equal(y2, y_ref)
})

test_that("mlts works for ts", {
  testts = ts(c(1:50), start=1990, frequency=2)

  extract_feats <- function(x) {
    F = lag_windows(x, p = 1, no_peeking = TRUE)
    na.omit(cbind(x, F))
  }
  lin <- function(f) lm(x ~ ., f)

  mlts.spec = build_mltsp_pipline(NULL, extract_feats, lin)

  model = mltsp(mlts.spec, testts)
  y = forecast(model, 5)

  y_ref = ts(c(51:55), start=2015, frequency = 2)
  expect_equal(y, y_ref)
})

test_that("mlts works for ts, second test", {

  testts = ts(c(1:50), start=1990, frequency=2)

  fx <- function(x) lag_windows(x, p = 1, no_peeking = FALSE)
  ln <- function(f) lm(x.l0 ~ ., f)

  mlts.spec = build_mltsp_pipline(NULL, fx, ln)

  f1 = forecast(mltsp(mlts.spec, testts), 5)
  f2 = forecast(mltsp(testts, NULL, fx, ln), 5)
  expect_equal(f1, f2)
})
