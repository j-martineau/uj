.rd_filename <- function(file, type, d) {
  if (base::is.null(file)) {
    prompt <- uj::f0(d == ",", "comma (,)", uj::f0(d == "\t", "tab", uj::f0(d == " ", "space", uj::f0(d == ".", "dot (.)", uj::f0(d == "|", "pipe (|)", uj::f0(d == ":", "colon (:)", uj::f0(d == ";", "semicolon (;)", uj::f0(d == "`", "backtick (`)", uj::f0(d == "^", "caret (^)", uj::f0(d == "~", "tilde (~)", uj::g("`", d, "`")))))))))))
    prompt <- base::paste0(type, "in rectangular", prompt, "delimited text format", collapse = " ")
    file   <- uj::choose_doc(prompt)
  }
  if (!uj::.cmp_chr_vec(file)) {uj::stopperr("[file] must be NULL or a complete character vec (?cmp_chr_vec)")}
  file <- base::paste0(file, collapse = "")
  if (!base::file.exists(file)) {uj::stopperr(base::paste0("the specified file ('", file, "') does not exist."))}
  file
}

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
#'  \tabular{l}{  \eqn{^{(1)}} `file = NULL` prompts the user to select a file.}
#' @param d Either `NULL` or a \link[=cmp_ch1_scl]{complete onechar scalar} text delimiter.
#' @param type A \link[=cmp_str_scl]{complete string scalar} describing the type of file to be read.
#' @param file Either `NULL` or a \link[=cmp_chr_vec]{complete character vector} that resolves to a file path. When `file = NULL` the user is asked to select a file using a system dialog box.
#' @return **Varies**       \cr\cr `rd_clip`
#' \cr\cr  **A data.frame** \cr\cr `rd_csv, rd_tsv, rd_xsv`
#' @export
rd <- function() {utils::help("rd", package = "uj")}

#' @rdname rd
#' @export
rd_xsv <- function(d, type = "data file", file = NULL, ...) {
  errs <- NULL
  if (!uj::.cmp_ch1_scl(d   )) {errs <- base::c(errs, "[d] must be a complete onechar scalar (?cmp_ch1_scl).")}
  if (!uj::.cmp_str_scl(type)) {errs <- base::c(errs, "[type] must be a complete string scalar (?cmp_str_scl).")}
  if (!base::is.null(file    )) {if (!uj::.cmp_str_scl(file)) {errs <- base::c(errs, "[file] must be a complete string scalar (?cmp_str_scl).")}}
  if (!base::is.null(errs    )) {uj::stopperr(errs)}
  readr::read_delim(uj:::.rd_filename(type, file, d), delim = d, ...)
}

#' @rdname rd
#' @export
rd_csv <- function(type = "data file", file = NULL, ...) {
  errs <- NULL
  if (!uj::.cmp_str_scl(type)) {errs <- base::c(errs, "[type] must be a complete string scalar (?cmp_str_scl).")}
  if (!base::is.null(file    )) {if (!uj::.cmp_str_scl(file)) {errs <- base::c(errs, "[file] must be a complete string scalar (?cmp_str_scl).")}}
  if (!base::is.null(errs    )) {uj::stopperr(errs)}
  readr::read_csv(uj:::.rd_filename(type, file, ","), ...)
}

#' @rdname rd
#' @export
rd_tsv <- function(type = "data file", file = NULL, ...) {
  errs <- NULL
  if (!uj::.cmp_str_scl(type)) {errs <- base::c(errs, "[type] must be a complete string scalar (?cmp_str_scl).")}
  if (!base::is.null(file    )) {if (!uj::.cmp_str_scl(file)) {errs <- base::c(errs, "[file] must be a complete string scalar (?cmp_str_scl).")}}
  if (!base::is.null(errs    )) {uj::stopperr(errs)}
  readr::read_tsv(uj:::.rd_filename(type, file, "\t"), ...)
}

#' @rdname rd
#' @export
rd_clip <- function() {readr::clipboard()}
