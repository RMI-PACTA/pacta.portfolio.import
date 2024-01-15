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
  return <- withr::local_tempfile()
  writeChar("A,B\r1,2\r", return, eos = NULL)
  expect_true(tail(readBin(return, what = "raw", n = 10), n = 1) == charToRaw("\r"))
  expect_true(has_newline_at_end(return))
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

test_that("has_newline_at_end identifies an inaccessible filepath", {
  filepath <- "non-existant-file.txt"
  expect_false(file.exists(filepath))
  expect_equal(has_newline_at_end(filepath), NA)
})
