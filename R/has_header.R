has_header <- function(filepath, encoding = NULL, delimiter = NULL) {
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

  header_types <-
    determine_header_types(
      filepath = filepath,
      encoding = encoding,
      delimiter = delimiter
    )

  if (all(header_types %in% c("character", "logical"))) {
    has_header <- TRUE
  } else {
    has_header <- FALSE
  }

  has_header
}
