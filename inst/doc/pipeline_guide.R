## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library("fpp")
library("e1071")
library("forecast")
library("xts")
library("mltsp")

options(warn = 1)

set.seed(0)

## ----includes------------------------------------------------------------
library("xts")
library("mltsp")

n = 25
stamps <- seq(from = as.Date("2010-01-01"), to=as.Date("2010-01-25"), by = "day")
observed_data <- xts(as.numeric(1:n) + runif(n), stamps)

## ----setup2--------------------------------------------------------------
train_data <- head(observed_data, 20)

## ----model_steup---------------------------------------------------------
pp <- list(list("diff", "auto"))
fx <- function(x) cbind(x, lag_windows(x, p=1))
ln <- SimpleLM
fcster <- mltsp_forecaster(pp, fx, ln)

## ------------------------------------------------------------------------
fcster(train_data, h=5)

## ----echo=FALSE----------------------------------------------------------
pred = fcster(train_data, h=5)
plot(observed_data, main="Prediction Results")
lines(c(tail(train_data, 1), pred), col="red", lwd=2, lty=2)
legend("topleft",
       legend=c("Observed", "Forecast"),
       col=c("black", "red"),
       lty=c(1, 2))

## ----train1--------------------------------------------------------------
model <- mltsp(train_data, pp, fx, ln)

## ------------------------------------------------------------------------
forecast(model, h=5)

## ------------------------------------------------------------------------
model2 <- mltsp(model, observed_data)

## ----fx1-----------------------------------------------------------------
fx <- function(x) lag_windows(x, p=1, no_peeking = FALSE)

## ----ln1-----------------------------------------------------------------
ln <- function(f) lm(x ~ ., f)

## ----model_steup1--------------------------------------------------------
fcster_nodiff <- mltsp_forecaster(NULL, 
                                  function(x) lag_windows(x, p=3, no_peeking = TRUE),
                                  SimpleLM)

## ----cross_val1----------------------------------------------------------
ts_crossval(train_data, fcster, horizon = 5, initial_window = 10)

## ----cross_val2----------------------------------------------------------
ts_crossval(train_data, fcster_nodiff, horizon = 5, initial_window = 10)

## ----echo=FALSE----------------------------------------------------------
plot(observed_data, main="Prediction Results")
lines(c(tail(train_data, 1), fcster(train_data, 5)), col="red", lwd=2, lty=2)
lines(c(tail(train_data, 1), fcster_nodiff(train_data, 5)), col="blue", lwd=2, lty=2)
legend("topleft",
       legend=c("Observed", "Forecast with difference", "Forecast without difference"),
       col=c("black", "red", "blue"),
       lty=c(1, 2, 2))

## ------------------------------------------------------------------------
ts_crossval(train_data, fcster, horizon = 5, initial_window = 10, verbose = TRUE)

## ------------------------------------------------------------------------
ts_crossval(train_data, fcster_nodiff, horizon = 5, initial_window = 10, break_err = 2.7,  verbose = TRUE)

## ---- parallel1----------------------------------------------------------
library("parallel")
options(mc.cores=2)

system.time({
err <- ts_crossval(train_data, fcster_nodiff, horizon = 5, initial_window = 10, plapply = mclapply,
            break_err = 3,  verbose = TRUE)
})

paste("Without batch-size Error was ", err)

## ---- parallel2----------------------------------------------------------
system.time({
err <- ts_crossval(train_data, fcster_nodiff, horizon = 5, initial_window = 10, plapply = mclapply,
            break_err = 3,  break_batch_size = 2, verbose = TRUE)
})

paste("Without batch-size, final error was ", err)

