determine_header_types <- function(filepath, encoding = NULL, delimiter = NULL) {
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

  first_line <-
    read_first_line(
      filepath = filepath,
      encoding = encoding,
      delimiter = delimiter
    )

  vapply(
    X = readr::spec(first_line)$cols,
    FUN = function(x) {
      sub(
        pattern = "^collector_",
        replacement = "",
        x = class(x)[[1]]
      )
    },
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )
}
