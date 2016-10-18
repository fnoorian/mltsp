library("forecast")
library("fpp")
library("e1071")

context("Narx")

# Test a ts object
a = head(ausbeer, -20)
b = head(tail(ausbeer, 20), 10)

mmm = build_narx(svm, p = 5)
s1x = narx(mmm, a)
s1 = narx(a, svm, p=5)
s1d = narx(a, svm, p=2, d=1)
sp1 = narx(a, svm, p=2, P=2, freq=frequency(ausbeer))
sp1D = narx(a, svm, p=2, P=2, d=1, freq=frequency(ausbeer))
sp1d = narx(a, svm, p=2, P=2, D=1, freq=frequency(ausbeer))
sp1dD = narx(a, svm, p=2, P=2, d=1, D=1, freq=frequency(ausbeer))

rmse <- function(x,y) {
  expect_is(y, "ts")
  expect_equal(index(x), index(y))
  sqrt(mean((x-y)^2))
}

test_that("NARX forecast is acceptable for ts data", {
  expect_lt(rmse(b, forecast(s1, h=10)$mean), 27)
  expect_lt(rmse(b, forecast(s1x, h=10)$mean), 27)
  expect_lt(rmse(b, forecast(s1d, h=10)$mean), 31)
  expect_lt(rmse(b, forecast(sp1, h=10)$mean), 25)
  expect_lt(rmse(b, forecast(sp1D, h=10)$mean), 34)
  expect_lt(rmse(b, forecast(sp1d, h=10)$mean), 25)
  expect_lt(rmse(b, forecast(sp1dD, h=10)$mean), 17)
})

# plot tests
#plot(forecast(sp1dD, h=10))
#lines(b, lwd=2)

# test an xts object
a = as.xts(head(ausbeer, -20))
b = as.xts(head(tail(ausbeer, 20), 10))

mmm = build_narx(svm, p = 5)
s1x = narx(mmm, a)
s1 = narx(a, svm, p=5)
s1d = narx(a, svm, p=2, d=1)
sp1 = narx(a, svm, p=2, P=2, freq=frequency(ausbeer))
sp1D = narx(a, svm, p=2, P=2, d=1, freq=frequency(ausbeer))
sp1d = narx(a, svm, p=2, P=2, D=1, freq=frequency(ausbeer))
sp1dD = narx(a, svm, p=2, P=2, d=1, D=1, freq=frequency(ausbeer))

rmse <- function(x,y) {
  expect_is(y, "xts")
  expect_equal(index(x), index(y))
  sqrt(mean((x-y)^2))
}

test_that("NARX forecast is acceptable for xts data", {
  expect_lt(rmse(b, forecast(s1, h=10)$mean), 27)
  expect_lt(rmse(b, forecast(s1x, h=10)$mean), 27)
  expect_lt(rmse(b, forecast(s1d, h=10)$mean), 31)
  expect_lt(rmse(b, forecast(sp1, h=10)$mean), 25)
  expect_lt(rmse(b, forecast(sp1D, h=10)$mean), 34)
  expect_lt(rmse(b, forecast(sp1d, h=10)$mean), 25)
  expect_lt(rmse(b, forecast(sp1dD, h=10)$mean), 17)
})

test_that("NARX works for numeric types", {

  input1 = 1:20
  mdl1 = narx(input1, SimpleLM, p=3)
  fc1 = forecast(mdl1, h=5)
  expect_equal(fc1$mean, 21:25)

  input2 = array(1:20)
  mdl2 = narx(input2, SimpleLM, p=3)
  fc2 = forecast(mdl2, h=5)
  expect_equal(fc2$mean, array(21:25))

  input3 = seq(0, 5, by = 0.5)
  mdl3 = narx(input3, SimpleLM, p=3)
  fc3 = forecast(mdl3, h=5)
  expect_equal(fc3$mean, seq(5.5, 7.5, by = 0.5))

})
