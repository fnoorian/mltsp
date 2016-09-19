library("xts")
library("timeDate")

context("Aggregation")

tstamps = timeSequence("2010-01-01", "2010-01-04", by="5 sec")
tstamps = tstamps[-(1:3)]
test_series = xts(seq_along(tstamps), tstamps)

test_min = create_minutely_xts(test_series, mins = 3)

test_that("Minutely aggregation works", {
  expect_equal(as.numeric(test_min[1]), sum(1:33))
  expect_equal(as.character(index(test_min)),
               as.character(timeSequence("2010-01-01 00:03:00", "2010-01-04 00:03:00", by="3 mins")))
})

test_that("Only start dates with the same type as the data are accepted", {
  expect_error(create_minutely_xts(test_series, mins = 3, start_date = "2009-12-31 23:50:00"))
})

test_min2 = create_minutely_xts(test_series, mins = 3, start_date = as.timeDate("2009-12-31 23:50:00"))
test_that("Minutely aggregation with early start time works", {
  expect_equal(sum(abs(test_min2[1:4])), 0)
  expect_equal(as.character(index(test_min2)),
               as.character(timeSequence("2009-12-31 23:51:00", "2010-01-04 00:03:00", by="3 mins")))
})


test_min4 = aggregate_xts(test_series, on="mins", k=3, FUN = last,
                          start_date = as.timeDate("2009-12-31 23:50:00"),
                          end_date = as.timeDate("2010-01-04 01:00:00"), na_handling = NULL)
test_that("Minutely aggregation with early start and late end time works", {
  expect_true(all(is.na(test_min4[1:4])))
  expect_true(all(!(is.na(test_min4[5:1444]))))
  expect_true(all(is.na(test_min4[1445:1463])))
  expect_equal(as.character(index(test_min4)),
               as.character(timeSequence("2009-12-31 23:51:00", "2010-01-04 00:57:00", by="3 mins")))
})


test_min5 = aggregate_xts(test_series, on="mins", k=3, FUN = last,
                          start_date = as.timeDate("2009-12-31 23:50:00"),
                          end_date = as.timeDate("2010-01-04 01:00:00"))
test_that("Minutely aggregation with alternative aggregator works", {
  expect_equal(as.character(index(test_min5)),
               as.character(timeSequence("2009-12-31 23:51:00", "2010-01-04 00:57:00", by="3 mins")))
  expect_true(all(is.na(test_min5[1:4])))
  expect_true(all(!(is.na(test_min5[5:1444]))))
  expect_equal(as.numeric(test_min5[1445:1463]), rep(51838, 19))
})

test_min6 = aggregate_xts(test_series, on="mins", k=3, FUN = first,
                          start_date = as.timeDate("2009-12-31 23:50:00"),
                          end_date = as.timeDate("2010-01-04 01:00:00"), na_handling = NULL)
test_min6_1 = aggregate_xts(test_series, on="mins", k=3, FUN = "first",
                            start_date = as.timeDate("2009-12-31 23:50:00"),
                            end_date = as.timeDate("2010-01-04 01:00:00"), na_handling = NULL)
test_that("Minutely aggregation with string period.apply works", {
  expect_equal(test_min6, test_min6_1)
})


