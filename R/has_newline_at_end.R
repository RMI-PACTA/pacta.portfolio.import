# Determine if file has trailing newline
# returns TRUE if file has newline at end
# returns FALSE if file has no newline at end
# returns NA if file is not accessible or is not a text file
file_has_newline_at_end <- function(filepath) {
  stopifnot(
    "`filepath` must be length 1" = length(filepath) == 1L
  )
  if (!is_file_accessible(filepath) || !is_text_file(filepath)) {
    return(NA)
  }
  if (FALSE) {
    newlines_in_final_char <- system(
      command = paste(
        "tail -c1 ", filepath, # tail -c1 returns last 1 byte
        " | ", # pipe to next command
        "wc -l" # wc -l returns number of newline characters in input
      ),
      intern = TRUE
    )
    has_trailing_newline <- as.logical(as.integer(newlines_in_final_char))
  } else {
    # Windows does not have a POSIX compliant `tail` command
    # so we use builtin R functionality, which is slower
    con <- file(filepath, "rb")
    on.exit(close(con))

    not_at_end <- TRUE
    chars <- ""
    while (not_at_end) {
      prev_chars <- chars
      chars <- readChar(con, nchars = 2048L, useBytes = TRUE)
      if (length(chars) == 0L) {
        not_at_end <- FALSE
      }
    }

    has_trailing_newline <- grepl(
      # using [\n] instead of [\n\r] because modern systems use either \n or
      # \r\n as newline, both of which match \n as lst character.
      "[\n]$", iconv(prev_chars, to = "UTF-8", sub = "")
    )

  }

  return(has_trailing_newline)
}

has_newline_at_end <- function(filepaths) {
  filepaths <- simplify_if_one_col_df(filepaths)
  stopifnot(
    "`filepaths` must be a character vector" = typeof(filepaths) == "character"
  )
  filepaths <- canonize_path(filepaths)

  vapply(
    X = filepaths,
    FUN = file_has_newline_at_end,
    FUN.VALUE = logical(1),
    USE.NAMES = FALSE
  )
}
