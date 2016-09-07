library("xts")
library("timeDate")

context("Preprocessing")

test_that("Preprocessing works", {
  train1 = xts(as.numeric(1:20), timeSequence(from = "2010-01-01", "2010-01-20"))
  p1 = ts_preprocess(train1, list(list("diff", 1), list("log")))
  r1 = ts_inv_preprocess(p1$x, p1$technique_list)

  train2 = ts(as.numeric(1:20), start = 1990, frequency = 2)
  p2 = ts_preprocess(train2, list(list("diff", 1), list("log")))
  r2 = ts_inv_preprocess(p2$x, p2$technique_list)

  p3 = ts_preprocess(train2, list(list("log+min")))
  r3 = ts_inv_preprocess(p3$x, p3$technique_list)

  expect_equal(train1, r1)
  expect_equal(train2, r2)
  expect_equal(train2, r3)
})
