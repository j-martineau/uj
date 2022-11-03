#' @name readr_tools
#' @title Aliases and wraps for functions in package \code{readr}.
#' @description Alias for \code{\link[readr]{read_csv}} to read comma-separated
#'   values from text files.
#' @export
rdcsv <- readr::read_csv

#' @describeIn readr_tools Alias for \code{\link[readr]{clipboard}} to read text
#'   from the system clipboard.
#' @export
rdclip <- readr::clipboard

#' @describeIn readr_tools Alias for \code{\link[readr]{read_delim}} to read
#'   (arbitrary) delimiter-separated values from text files.
#' @export
rddsv <- readr::read_delim

#' @describeIn readr_tools Alias for \code{\link[readr]{read_tsv}} to read tab
#'   separated values from text files.
#' @export
rdtsv <- readr::read_tsv
