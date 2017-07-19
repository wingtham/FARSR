library(FARSR)

test_that("File_name is the right format and character", {
  expect_equal(make_filename(2014), "accident_2014.csv.bz2")
  expect_equal(make_filename(2015), "accident_2015.csv.bz2")
  expect_equal(make_filename("2013"), "accident_2013.csv.bz2")
})

test_that("Loaded file is a data frame", {
  expect_is(fars_read("accident_2013.csv.bz2"), "data.frame")
  expect_is(fars_read("accident_2014.csv.bz2"), "data.frame")
})

test_that("Error message is produced if file does not exist", {
  expect_error(fars_read(make_filename("ABC")))
  expect_error(fars_read("accident_14.csv.bz2"))
})

test_that("Loaded files stored as list", {
  expect_is(fars_read_years(2013:2014), "list")
  expect_is(fars_read_years(2014:2015), "list")
  expect_is(fars_read_years(2013), "list")
})


test_that("Summarised data stored as data frame", {
  expect_is(fars_summarize_years(2013:2014), "data.frame")
  expect_is(fars_summarize_years(2014:2015), "data.frame")
  expect_is(fars_summarize_years(2013), "data.frame")
})


test_that("Messages produced for maps visualisation", {
  expect_error(fars_map_state(0,2013))
  expect_error(fars_map_state(1000,2013))
})
