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
#'  \tabular{l}{  \eqn{^{(1)}} `File = NULL` prompts the user to select a file.                       }
#' @param File Either `NULL` or a \link[=cmp_chr_vec]{complete character vector} that resolves to a file path. When `File = NULL` the user is asked to select a file using a system dialog box.
#' @return **Varies**       \cr\cr `rd_clip`
#' \cr\cr  **A data.frame** \cr\cr `rd_csv, rd_tsv, rd_xsv`
#' @export
rd <- function() {utils::help("rd", "uj")}

#' @rdname rd
#' @export
rd_csv <- function(File = NULL, ...) {
  File <- uj:::.rd_filename(File, "csv")
  readr::read_csv(File, ...)
}

#' @rdname rd
#' @export
rd_clip <- function() {readr::clipboard()}

#' @rdname rd
#' @export
rd_xsv <- function(File = NULL, Delim = NULL, ...) {
  File <- uj:::.rd_filename(File, "tsv")
  readr::read_delim(File, delim = Delim, ...)
}

#' @rdname rd
#' @export
rd_tsv <- function(File = NULL, ...) {
  File <- uj:::.rd_filename(File, "xsv")
  readr::read_tsv(File, ...)
}
