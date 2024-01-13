char_to_hex <- function(x) {
  cat(paste(paste0("\\x", charToRaw(x)), collapse = ""))
}

char_to_unicode <- function(x) {
  cat(paste0("\\U", format(as.hexmode(utf8ToInt(x)), width = 2 * length(charToRaw(x)), upper = TRUE)))
}
