#' @name rd
#' @encoding UTF-8
#' @family extensions
#' @family wraps
#' @family files
#' @title Thin and extended functionality wrappers of `readr` functions.
#' @description Read the clipboard and text files, but prompting user to choose a file if no file name/path is provided.
#' @details
#' \tabular{ll}{  `rd_clip`   \tab Thinly wraps \code{\link[readr]{clipboard}}         \cr   \tab   \cr
#'                `rd_csv`    \tab Extends \code{\link[readr]{read_csv}}\eqn{^{(1)}}   \cr
#'                `rd_tsv`    \tab Extends \code{\link[readr]{read_tsv}}\eqn{^{(1)}}   \cr
#'                `rd_xsv`    \tab Extends \code{\link[readr]{read_delim}}\eqn{^{(1)}} \cr   \tab     }
#'  \tabular{l}{  \eqn{^{(1)}} `file = NULL` prompts the user to select a file.                       }
#' @param file Either `NULL` or a \link[=cmp_chr_vec]{complete character vector} that resolves to a file path. When `file = NULL` the user is asked to select a file using a system dialog box.
#' @param .D Either `NULL` or a \link[=cmp_ch1_scl]{complete onechar scalar} text delimiter.
#' @return **Varies**       \cr\cr `rd_clip`
#' \cr\cr  **A data.frame** \cr\cr `rd_csv, rd_tsv, rd_xsv`
#' @export
rd <- function() {utils::help("rd", "uj")}

#' @rdname rd
#' @export
rd_csv <- function(file = NULL, ...) {
  file <- uj:::.rd_filename(file, "csv")
  readr::read_csv(file, ...)
}

#' @rdname rd
#' @export
rd_clip <- function() {readr::clipboard()}

#' @rdname rd
#' @export
rd_xsv <- function(file = NULL, .D = NULL, ...) {
  file <- uj:::.rd_filename(file, "xsv")
  readr::read_delim(file, delim = .D, ...)
}

#' @rdname rd
#' @export
rd_tsv <- function(file = NULL, ...) {
  file <- uj:::.rd_filename(file, "tsv")
  readr::read_tsv(file, ...)
}
