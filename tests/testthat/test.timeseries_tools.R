# Copyright (c) 2016 Farzad Noorian
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
################################################################################

# Unit test for time-series time-stamp continuation tools

library("xts")
library("timeDate")

context("Timestamp tools")

train1 = xts(as.numeric(1:20), timeSequence(from = "2010-01-01", "2010-01-20"))
train2 = ts(as.numeric(1:20), start = 1990, frequency = 2)
train3 = xts(as.numeric(1:20), as.POSIXct(timeSequence(from = "2010-01-01", "2010-01-20")))

test_that("ts_append works", {
  t1_ref = xts(as.numeric(c(1:20, 51)), timeSequence(from = "2010-01-01", "2010-01-21"))
  t1 = ts_append(train1, 51, new_timestamp = NULL)
  expect_equal(t1, t1_ref)

  t2_ref = ts(as.numeric(c(1:20, 51)), start = 1990, frequency = 2)
  t2 = ts_append(train2, 51, new_timestamp = NULL)
  expect_equal(t2, t2_ref)

  t3_ref = xts(as.numeric(c(1:20, 51)), as.POSIXct(timeSequence(from = "2010-01-01", "2010-01-21")))
  t3 = ts_append(train3, 51, new_timestamp = NULL)
  expect_equal(t3, t3_ref)
})

# Test continue time-series
test_that("ts_append works", {
  t1_c = ts_continue(train1, c(51,52,53))
  t1_c_ref = xts(as.numeric(c(51,52,53)), timeSequence(from = "2010-01-21", "2010-01-23"))
  expect_equal(t1_c, t1_c_ref)

  t2_c = ts_continue(train2, c(51,52, 53))
  t2_c_ref = ts(as.numeric(c(51,52, 53)), start = 2000, frequency = 2)
  expect_equal(t2_c, t2_c_ref)

  t3_c = ts_continue(train3, c(51,52, 53))
  t3_c_ref = xts(as.numeric(c(51,52, 53)), as.POSIXct(timeSequence(from = "2010-01-21", "2010-01-23")))
  expect_equal(t3_c, t3_c_ref)
})
# Test Clone time-series
test_that("clone_timestamps works", {
  t1_x = clone_timestamps(train1, 101:120)
  t1_x_ref = xts(as.numeric(101:120), timeSequence(from = "2010-01-01", "2010-01-20"))
  expect_equal(t1_x, t1_x_ref)

  t2_x = clone_timestamps(train2, 101:120)
  t2_x_ref = ts(as.numeric(c(101:120)), start = 1990, frequency = 2)
  expect_equal(t2_x, t2_x_ref)

  t3_x = clone_timestamps(train3, 101:120)
  t3_x_ref = xts(as.numeric(c(101:120)), as.POSIXct(timeSequence(from = "2010-01-01", "2010-01-20")))
  expect_equal(t3_x, t3_x_ref)
})
