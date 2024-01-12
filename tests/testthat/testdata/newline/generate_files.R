# to be run in-place (in directory where file is located)
# require system utilities.
echo_cmd <- Sys.which("echo")
if (echo_cmd == "") {
  stop("`echo` not found on `$PATH`")
}
printf_cmd <- Sys.which("printf")
if (printf_cmd == "") {
  stop("`printf` not found on `$PATH`")
}


# writeLines includes trailing newlines by default
writeLines(c("yay", "newlines"), "writeLines.txt", sep = "\n") # default behavior

# echo includes trailing newlines by default
system(paste(echo_cmd, "has newline", ">", "echo.txt"))

# change sep to not include trailing newlines
writeLines(c("no", "newlines"), "writeLines_nonewline.txt", sep = " ")

# printf does not include trailing newlines by default
system(paste(printf_cmd, "%s", "no newlines", ">", "printf_nonewline.txt"))

#printf does not include trailing newlines by default
system(paste("echo", "first line", ">", "multiline_nonewline.txt"))
system(paste("printf", "%s", "second line", ">>", "multiline_nonewline.txt"))
