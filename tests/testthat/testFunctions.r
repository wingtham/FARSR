library(FARSR)
context("General")

test_that("File_name is the right format and character", {
  expect_equal(make_filename(2014), "accident_2014.csv.bz2")
  expect_equal(make_filename(2015), "accident_2015.csv.bz2")
  expect_equal(make_filename("2013"), "accident_2013.csv.bz2")
})

test_that("Error message is produced if file does not exist", {
  expect_error(fars_read(make_filename("ABC")))
  expect_error(fars_read("accident_14.csv.bz2"))
})

