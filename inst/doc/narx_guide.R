## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library("mltsp")
library("fpp")
library("e1071")

## ----includes------------------------------------------------------------
library("fpp")

beer_train = head(ausbeer, -20)
beer_test = head(tail(ausbeer, 20), 10)

## ----buildnarx-----------------------------------------------------------
library("mltsp")
library("e1071")
spec = build_narx(svm, p=2, d=0, P=1, D=1, freq=frequency(ausbeer))
model = narx(spec, beer_train)

model

## ----buildnarx2----------------------------------------------------------
model = narx(beer_train, svm, p=2, d=0, P=1, D=1, freq=frequency(ausbeer))

## ----buildnarx3----------------------------------------------------------
beer_train2 = head(ausbeer, -10)

model2 = narx(model, beer_train2)

## ----testnarx------------------------------------------------------------
fcst = forecast(model, h = 10)
plot(fcst)
lines(beer_test, col="red")

## ----testnarx2-----------------------------------------------------------
beer_train2 = head(ausbeer, -10) # this is using data from another future!
beer_test2 = tail(ausbeer, 10) # this is using data from another future!

fcst2 = predict(model, beer_train2, h = 10)

plot(ausbeer)
lines(beer_test2, col="red")
lines(fcst2, col="blue")

## ----testnarx3-----------------------------------------------------------
model2 = narx(model, beer_train2)
fcst3 = forecast(model2, h = 10)

plot(fcst3)
lines(fcst2, col="red")

## ----xreg----------------------------------------------------------------
set.seed(0)

tstamps = seq(as.Date("2000-01-01"), length.out = 110, by='day')
x = xts(runif(length(tstamps)), tstamps)
xreg = 1 - 0.5 * x
yreg = xts(runif(110), tstamps)

colnames(xreg) = colnames(yreg) = "xreg"

# training and testing data
x_train = head(x, 100)
x_test = tail(x, 10)
ind_test = index(x_test)

## ----xreg1---------------------------------------------------------------
model = narx(x_train, SimpleLM, p = 2)
pred1 = forecast(model, h=10)

## ----xreg2---------------------------------------------------------------
model2 = narx(x_train, SimpleLM, p = 2, xreg = xreg)
pred2 = forecast(model2, xreg=xreg[ind_test])

## ----xreg3---------------------------------------------------------------
model3 = narx(x_train, SimpleLM, p = 2, xreg = yreg)
pred3 = forecast(model3, xreg=yreg[ind_test])

## ----xregres-------------------------------------------------------------
rmse <- function(x,y) sqrt(mean((x-y)^2))

c(Err_without_xreg= rmse(pred1$mean, x_test),
  Err_with_xreg= rmse(pred2$mean, x_test),
  Err_with_bad_xreg= rmse(pred3$mean, x_test))

