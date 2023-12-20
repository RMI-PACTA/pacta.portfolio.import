test_that("file_has_newline_at_end identifies file with trailing newline", {
  filepath <- tempfile()
  writeLines(c("yay", "newlines"), filepath, sep = "\n") # default behavior
  expect_true(file_has_newline_at_end(filepath))
})

#echo includes trailing newlines by default
test_that("file_has_newline_at_end identifies trailing newline - echo", {
  filepath <- tempfile()
  system(paste("echo", "foo", ">", filepath))
  expect_true(file_has_newline_at_end(filepath))
})

test_that("file_has_newline_at_end identifies file with no trailing newline", {
  filepath <- tempfile()
  writeLines(c("yay", "newlines"), filepath, sep = " ") # sep changed
  expect_false(file_has_newline_at_end(filepath))
})

#printf does not include trailing newlines by default
test_that("file_has_newline_at_end identifies no trailing newline - printf", {
  filepath <- tempfile()
  system(paste("printf", "%s", "no newlines", ">", filepath))
  expect_false(file_has_newline_at_end(filepath))
})

test_that("file_has_newline_at_end returns NA for empty file", {
  filepath <- tempfile()
  expect_identical(file_has_newline_at_end(filepath), NA) # logical NA
})

test_that("file_has_newline_at_end expects a single file", {
  filepath <- tempfile()
  f2 <- tempfile()
  expect_error(
    file_has_newline_at_end(c(filepath, f2)),
    "`filepath` must be length 1"
  )
})
