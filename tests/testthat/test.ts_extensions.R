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

library("xts")

context("TS extensions")

test_that("c works for ts",{
  time1 = ts(as.numeric(1:20), start = 1990, frequency = 2)
  time2 = ts(as.numeric(21:40), start = 2000, frequency = 2)
  time12 = ts(as.numeric(1:40), start = 1990, frequency = 2)
  timec = c(time1, time2)

  expect_equal(timec, time12)
})

test_that("head and tail works for ts", {
  train2 = ts(as.numeric(1:20), start = 1990, frequency = 2)
  head_train2 = ts(as.numeric(1:17), start = 1990, frequency = 2)
  tail_train2 = ts(as.numeric(5:20), start = 1992, frequency = 2)

  expect_equal(head(train2, -3), head_train2)
  expect_equal(tail(train2, -4), tail_train2)
})
