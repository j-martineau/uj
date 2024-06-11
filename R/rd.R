rd_filename <- function(type, file, d) {
  if (base::is.null(file)) {
    sep <- uj::f0(d == "," , "comma (,)"    ,
           uj::f0(d == "\t", "tab"          ,
           uj::f0(d == " " , "space"        ,
           uj::f0(d == "." , "dot (.)"      ,
           uj::f0(d == "|" , "pipe (|)"     ,
           uj::f0(d == ":" , "colon (:)"    ,
           uj::f0(d == ";" , "semicolon (;)",
           uj::f0(d == "`" , "backtick (`)" ,
           uj::f0(d == "^" , "caret (^)"    ,
           uj::f0(d == "~" , "tilde (~)"    , uj::g("`", d, "`")))))))))))
    prompt <- base::paste0(type, "in rectangular", sep, "delimited text format", collapse = " ")
    file   <- uj::choose_doc(prompt)
  }
  if (!uj::.cmp_chr_vec(file)) {uj::stopperr("[file] must be NULL or a complete character vec (?cmp_chr_vec)")}
  file <- base::paste0(file, collapse = "")
  if (!base::file.exists(file)) {uj::stopperr(base::paste0("the specified file ('", file, "') does not exist."))}
  file
}

#' @encoding UTF-8
#' @family wraps
#' @family files
#' @title Thin and extended functionality wrappers of `readr` functions.
#' @description Read the clipboard and rectangular, delimited data files, prompting user to choose a file if no file name/path is provided.
#' @param d Either `NULL` or a \link[=cmp_ch1_scl]{complete onechar scalar} text delimiter.
#' @param type A \link[=cmp_str_scl]{complete string scalar} describing the type of file to be read.
#' @param file Either `NULL` or a \link[=cmp_chr_vec]{complete character vector} that resolves to a file path. When `file = NULL` the user is asked to select a file using a system dialog box.
#' @export
rd_help <- function() {utils::help("rd_help", package = "uj")}

#' @describeIn rd_help Extends \code{\link[readr]{read_delim}} by prompting the user to select a file. Returns a data frame.
#' @export
rd_xsv <- function(d, type = "data file", file = NULL, ...) {
  errs <- NULL
  if (!uj::.cmp_ch1_scl(d            )) {errs <- base::c(errs, "[d] must be a complete onechar scalar (?cmp_ch1_scl).")}
  if (!uj::.cmp_str_scl(type         )) {errs <- base::c(errs, "[type] must be a complete string scalar (?cmp_str_scl).")}
  if (!uj::nll_or(file, "cmp_str_scl")) {errs <- base::c(errs, "[file] must be a complete string scalar (?cmp_str_scl).")}
  if (!base::is.null(errs            )) {uj::stopperr(errs)}
  readr::read_delim(uj:::rd_filename(type, file, d), delim = d, ...)
}

#' @describeIn rd_help Extends \code{\link[readr]{read_csv}} by prompting the user to select a file. Returns a data frame.
#' @export
rd_csv <- function(type = "data file", file = NULL, ...) {
  errs <- NULL
  if (!uj::.cmp_str_scl(type         )) {errs <- base::c(errs, "[type] must be a complete string scalar (?cmp_str_scl).")}
  if (!uj::nll_or(file, "cmp_str_scl")) {errs <- base::c(errs, "[file] must be a complete string scalar (?cmp_str_scl).")}
  if (!base::is.null(errs            )) {uj::stopperr(errs)}
  readr::read_csv(uj:::rd_filename(type, file, ","), ...)
}

#' @describeIn rd_help Extends \code{\link[readr]{read_tsv}} by prompting the user to select a file. Returns a data frame.
#' @export
rd_tsv <- function(type = "data file", file = NULL, ...) {
  errs <- NULL
  if (!uj::.cmp_str_scl(type         )) {errs <- base::c(errs, "[type] must be a complete string scalar (?cmp_str_scl).")}
  if (!uj::nll_or(file, "cmp_str_scl")) {errs <- base::c(errs, "[file] must be a complete string scalar (?cmp_str_scl).")}
  if (!base::is.null(errs            )) {uj::stopperr(errs)}
  readr::read_tsv(uj:::rd_filename(type, file, "\t"), ...)
}

#' @describeIn rd_help Thinly wraps \code{\link[readr]{clipboard}}. Return value depends on the contents of the clipboard.
#' @export
rd_clip <- function() {readr::clipboard()}
