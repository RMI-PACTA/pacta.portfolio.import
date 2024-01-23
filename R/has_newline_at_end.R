has_newline_at_end <- function(filepaths) {
  filepaths <- simplify_if_one_col_df(filepaths)
  stopifnot("`filepaths` must be a character vector" = typeof(filepaths) == "character")
  filepaths <- canonize_path(filepaths)

  vapply(
    X = filepaths,
    FUN = function(filepath) {
      if (!is_file_accessible(filepath) || !is_text_file(filepath)) {
        return(NA)
      }

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

      # using [\n] instead of [\n\r] because modern systems use either \n or
      # \r\n as newline, both of which match \n as last character.
      # \r as last character should not be recognized as trailing newline.
      grepl("[\n]$", iconv(prev_chars, to = "UTF-8", sub = ""))
    },
    FUN.VALUE = logical(1),
    USE.NAMES = FALSE
  )
}
