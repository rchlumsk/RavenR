library(testthat)
library(RavenR)
context("Time series and general function checks")

test_that("date manipulations are consistent", {
  expect_equal(as.numeric(as.Date("2008-10-01")-as.Date("2007-10-01")),366)
  expect_equal(lubridate::year(as.Date("2008-10-01")),2008)
  expect_equal(lubridate::month(as.Date("2008-10-01")),10)
  expect_equal(lubridate::day(as.Date("2008-10-01")),1)
})

data("rvn_forcing_data")
ff <- rvn_forcing_data$forcings

test_that("xts format functioning as expected", {
  expect_equal(ncol(ff),21)
  expect_equal(length(lubridate::date(ff)),731)
})

test_that("rvn_apply_wyearly functioning", {
  expect_equal(nrow(rvn_apply_wyearly(ff,mean)),2)
  expect_equal(ncol(rvn_apply_wyearly(ff,cmax)),21)
})


