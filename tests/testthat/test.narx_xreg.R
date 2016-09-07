library("e1071")
library("xts")
library("timeDate")

context("Narx with xreg")

set.seed(0)

#### XTS test
tstamps = as.POSIXct(timeSequence("2000-01-01", length.out = 110, by='1 d'))
x = xts(runif(length(tstamps)), tstamps)
xreg = 1 - 0.5 * x
yreg = xts(runif(110), tstamps)

colnames(xreg) = colnames(yreg) = "xreg"

x_train = head(x, 100)
x_test = tail(x, 10)
ind_test = index(x_test)

model = narx(x_train, SimpleLM, p = 2)
pred1 = forecast(model, h=10)

model2 = narx(x_train, SimpleLM, p = 2, xreg = xreg)
pred2 = forecast(model2, xreg=xreg[ind_test])

model3 = narx(x_train, SimpleLM, p = 2, xreg = yreg)
pred3 = forecast(model3, xreg=yreg[ind_test])

rmse <- function(x,y) {
  expect_is(y, "xts")
  expect_equal(index(x), index(y))
  sqrt(mean((x-y)^2))
}

# error for number 2 (using good xreg) must be significantly less
test_that("xreg should improve random prediction", {
  err1 = rmse(pred1$mean, x_test)
  err2 = rmse(pred2$mean, x_test)
  err3 = rmse(pred3$mean, x_test)

  expect_equal(pred2$mean, x_test)
  expect_gt(err1, 0.25)
  expect_gt(err3, 0.25)
})
