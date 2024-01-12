test_that(
  "file_has_newline_at_end identifies trailing newline (R writeLines())",
  {
    expect_true(
      file_has_newline_at_end(
        testthat::test_path("testdata", "newline", "writeLines.txt")
      )
    )
  }
)

#echo includes trailing newlines by default
test_that(
  "file_has_newline_at_end identifies trailing newline (POSIX echo)",
  {
    expect_true(
      file_has_newline_at_end(
      testthat::test_path("testdata", "newline", "echo.txt")
      )
    )
  }
)

test_that(
  "file_has_newline_at_end flags missing trailing newline (R writeLines())",
  {
    expect_false(
      file_has_newline_at_end(
        testthat::test_path("testdata", "newline", "writeLines_nonewline.txt")
      )
    )
  }
)

#printf does not include trailing newlines by default
test_that(
  "file_has_newline_at_end flags missing trailing newline (POSIX printf)",
  {
    expect_false(
      file_has_newline_at_end(
        testthat::test_path("testdata", "newline", "printf_nonewline.txt")
      )
    )
  }
)

test_that(
  "file_has_newline_at_end flags missing trailing newline (multiline file)",
  {
    expect_false(
      file_has_newline_at_end(
        testthat::test_path("testdata", "newline", "multiline_nonewline.txt")
      )
    )
  }
)

test_that(
  "file_has_newline_at_end returns NA for empty file (POSIX touch))",
  {
    expect_identical(
      file_has_newline_at_end(
        testthat::test_path("testdata", "newline", "touch.txt")
      ),
      NA
    )
  }
)

test_that("file_has_newline_at_end returns NA for non-existing file", {
  filepath <- tempfile() # file does not exist yet
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

## vector of filepaths
test_that("has_newline_at_end correctly identifies vector of paths", {
  has_newlines <- testthat::test_path(
    "testdata", "newline", "writeLines.txt"
  )
  no_newlines <- testthat::test_path(
    "testdata", "newline", "writeLines_nonewline.txt"
  )
  emptyfile <- testthat::test_path(
    "testdata", "newline", "touch.txt"
  )
  missing_file <- tempfile()
  multiline <- testthat::test_path(
    "testdata", "newline", "multiline_nonewline.txt"
  )
  path_vec <- c(has_newlines, no_newlines, emptyfile, missing_file, multiline)
  expect_identical(
    has_newline_at_end(path_vec),
    c(TRUE, FALSE, NA, NA, FALSE)
  )
})


test_that(
  "file_has_newline_at_end does not identify CR (\\r) as trailing newline)",
  {
    expect_false(
      file_has_newline_at_end(
        testthat::test_path("testdata", "newline", "writeLines_CR.txt")
      )
    )
  }
)

test_that(
  "file_has_newline_at_end identifies trailing newline (CRLF - Windows)",
  {
    expect_true(
      file_has_newline_at_end(
        testthat::test_path("testdata", "newline", "writeLines_CRLF.txt")
      )
    )
  }
)

test_that(
  "file_has_newline_at_end does not id LFCR (\\n\\r) as trailing newline)",
  {
    expect_false(
      file_has_newline_at_end(
        testthat::test_path("testdata", "newline", "writeLines_LFCR.txt")
      )
    )
  }
)

