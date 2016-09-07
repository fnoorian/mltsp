# Unit test for Lag windows
library("xts")
library("timeDate")

context("Lag windows")

a_vector = 1:10
a_matrix = matrix(c(1:10, 101:110), ncol=2)
colnames(a_matrix) = c("V1", "V2")
a_df = as.data.frame(a_matrix)
tstamps = timeSequence(from="2012-01-01", by ="1h", length.out=10)
a_xts_vec = xts(a_vector, tstamps)
a_xts_mat = xts(a_matrix, tstamps)

l_a_vec = lagmatrix(a_vector, c(0, 1, 2), pstr="a")
l_a_vec_reference = matrix(c(1:10, NA, 1:9, NA, NA, 1:8), nrow = 10, dimnames = list(NULL, c("a.l0", "a.l1", "a.l2")))
test_that("Ordinarly vector", {
  expect_equal(l_a_vec, l_a_vec_reference)
})

l_af_vec = lagmatrix(a_vector, c(-1, 1, 2), pstr="a")
l_af_vec_reference = matrix(c(2:10, NA, NA, 1:9, NA, NA, 1:8), nrow = 10, dimnames = list(NULL, c("a.lf1", "a.l1", "a.l2")))
expect_equal(l_af_vec, l_af_vec_reference)

l_a_mat = lagmatrix(a_matrix, c(0, 1, 2), pstr="a")
l_a_mat_reference = matrix(c(1:10, NA, 1:9, NA, NA, 1:8,
                             101:110, NA, 101:109, NA, NA, 101:108), nrow = 10,
                           dimnames = list(NULL, c("V1.l0", "V1.l1", "V1.l2", "V2.l0", "V2.l1", "V2.l2")))
test_that("Lead forward vector", {
  expect_equal(l_a_mat, l_a_mat_reference)
})

l_a_df = lagmatrix(a_df, c(0, 1, 2), pstr="a")
test_that("Ordinarly Matrix", {
  expect_equal(l_a_mat, l_a_df)
})

l_a_xts_vec = lagmatrix(a_xts_vec, c(0, 1, 2), pstr="a")
l_a_xts_vec_ref = xts(l_a_vec_reference, tstamps)
test_that("xts vector", {
  expect_equal(l_a_xts_vec, l_a_xts_vec_ref)
})

l_a_xts_mat = lagmatrix(a_xts_mat, c(0, 1, 2), pstr="a")
l_a_xts_mat_ref = xts(l_a_mat_reference, tstamps)
test_that("xts matrix", {
  expect_equal(l_a_xts_mat, l_a_xts_mat_ref)
})
