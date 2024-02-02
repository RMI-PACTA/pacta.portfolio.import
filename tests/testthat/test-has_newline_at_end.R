test_that("has_newline_at_end identifies no trailing newline or return", {
  no_newline <- withr::local_tempfile()
  writeChar("A,B\n1,2", no_newline, eos = NULL)
  expect_true(tail(readBin(no_newline, what = "raw", n = 10), n = 1) == charToRaw("2"))
  expect_false(has_newline_at_end(no_newline))
})

test_that("has_newline_at_end identifies trailing newline", {
  newline <- withr::local_tempfile()
  writeChar("A,B\n1,2\n", newline, eos = NULL)
  expect_true(tail(readBin(newline, what = "raw", n = 10), n = 1) == charToRaw("\n"))
  expect_true(has_newline_at_end(newline))
})

test_that("has_newline_at_end identifies trailing return", {
  trailing_return <- withr::local_tempfile()
  writeChar("A,B\r1,2\r", trailing_return, eos = NULL)
  expect_true(tail(readBin(trailing_return, what = "raw", n = 10), n = 1) == charToRaw("\r"))
  expect_true(has_newline_at_end(trailing_return))
})

test_that("has_newline_at_end identifies trailing return and newline", {
  return_newline <- withr::local_tempfile()
  writeChar("A,B\r\n1,2\r\n", return_newline, eos = NULL)
  expect_true(all(tail(readBin(return_newline, what = "raw", n = 10), n = 2) == charToRaw("\r\n")))
  expect_true(has_newline_at_end(return_newline))
})

test_that("has_newline_at_end identifies a binary file", {
  binary <- withr::local_tempfile()
  writeBin(1:5, binary)
  expect_true(all(readBin(binary, integer(), 5) == 1:5))
  expect_equal(has_newline_at_end(binary), NA)
})

test_that("has_newline_at_end identifies an empty file", {
  empty <- withr::local_tempfile()
  write.table(data.frame(), file = empty, col.names = FALSE)
  expect_identical(as.integer(file.size(empty)), 0L)
  expect_identical(has_newline_at_end(empty), NA)
})

test_that("has_newline_at_end identifies an inaccessible filepath", {
  filepath <- "non-existant-file.txt"
  expect_false(file.exists(filepath))
  expect_equal(has_newline_at_end(filepath), NA)
})

test_that("has_newline_at_end behaves as expected when multiple filepaths are passed", {
  binary <- withr::local_tempfile()
  writeBin(1:5, binary)
  no_newline <- withr::local_tempfile()
  writeChar("A,B\n1,2", no_newline, eos = NULL)
  newline <- withr::local_tempfile()
  writeChar("A,B\n1,2\n", newline, eos = NULL)
  invalid_filepath <- "non-existant-file.txt"
  expect_equal(has_newline_at_end(c(binary, no_newline, newline, invalid_filepath)), c(NA, FALSE, TRUE, NA))
  expect_equal(has_newline_at_end(c(binary, binary)), c(NA, NA))
  expect_equal(has_newline_at_end(c(invalid_filepath, invalid_filepath)), c(NA, NA))
  expect_equal(has_newline_at_end(c(no_newline, no_newline)), c(FALSE, FALSE))
  expect_equal(has_newline_at_end(c(newline, newline)), c(TRUE, TRUE))
})

test_that("file_has_newline_at_end expects a single file", {
  f1 <- withr::local_tempfile()
  f2 <- withr::local_tempfile()
  expect_error(
    file_has_newline_at_end(c(f1, f2)),
    "`filepath` must be length 1"
  )
})

