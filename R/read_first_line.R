read_first_line <- function(filepath, encoding = NULL, delimiter = NULL) {
  if (!is_text_file(filepath)) {
    return(NA)
  }

  if (is.null(encoding)) {
    encoding <- guess_file_encoding(filepath)
  }

  if (is.null(delimiter)) {
    delimiter <- guess_delimiter(filepath)
  }

  if (any(is.na(c(encoding, delimiter)))) {
    return(NA)
  }

  readr::read_delim(
    file = filepath,
    delim = delimiter,
    n_max = 1L,
    locale = readr::locale(encoding = encoding),
    col_names = FALSE,
    show_col_types = FALSE,
    progress = FALSE
  )
}
