% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/guess_file_encoding.R
\name{guess_file_encoding}
\alias{guess_file_encoding}
\title{Guess the file encoding for a vector of filenames or filepaths}
\usage{
guess_file_encoding(filepaths, threshold = 0.2)
}
\arguments{
\item{filepaths}{A character vector}

\item{threshold}{A single element numeric (minimum confidence level of the guess [0-1])}
}
\value{
A character vector the same length as \code{filepaths}.
}
\description{
This function will guess the file encoding of a vector of filenames or
filepaths and return the file encoding as a string. It primarily uses
\code{stringi::stri_enc_detect()} to guess the encoding. Additionally, it
searches for known CP850 and CP1252 characters and will return the
appropriate encoding if found, because ICU/stringi cannot detect them. If a
file is a binary file, it will return \code{"binary"}. If a file is inaccessible
it will return \code{NA} for that element.
}
