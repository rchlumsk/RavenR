library(testthat)
library(RavenR)
context("Time series and general function checks")

test_that("date manipulations are consistent", {
  expect_equal(as.numeric(as.Date("2008-10-01")-as.Date("2007-10-01")),366)
  expect_equal(lubridate::year(as.Date("2008-10-01")),2008)
  expect_equal(lubridate::month(as.Date("2008-10-01")),10)
  expect_equal(lubridate::day(as.Date("2008-10-01")),1)
})

data("forcing.data")
ff <- forcing.data$forcings

test_that("xts format functioning as expected", {
  expect_equal(ncol(ff),21)
  expect_equal(length(lubridate::date(ff)),731)
})

test_that("apply.wyearly functioning", {
  expect_equal(nrow(apply.wyearly(ff,mean)),2)
  expect_equal(ncol(apply.wyearly.cols(ff,max)),22)
})


