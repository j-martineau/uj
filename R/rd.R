rd_filename <- function(type, file, d) {
  if (base::is.null(file)) {
    sep <- uj::f0(d == "\t", "tab"            ,
           uj::f0(d == " " , "space"          ,
           uj::f0(d == "." , "dot (.)"        ,
           uj::f0(d == "|" , "pipe (|)"       ,
           uj::f0(d == "," , "comma (,)"      ,
           uj::f0(d == ":" , "colon (:)"      ,
           uj::f0(d == "^" , "caret (^)"      ,
           uj::f0(d == "~" , "tilde (~)"      ,
           uj::f0(d == "`" , "backtick (`)"   ,
           uj::f0(d == ";" , "semicolon (;)"  ,
           uj::f0(d == "¦" , "broken pipe (¦)", base::paste0("`", d, "`"))))))))))))
    prompt <- base::paste0(type, " in rectangular ", sep, " delimited text format", collapse = "")
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
rd_xsv <- function(d = NULL, type = "data file", file = NULL, ...) {
  if (base::is.null(d)) {                                                                                                  # IF [d] is NULL (delimiter needs to be selected) TJEM
    opts <- base::c("dot         (.)", "tab         (⇥)", "pipe        (|)", "caret       (^)", "colon       (:)",         # : vector of user-friendly delimiter options
                    "comma       (,)", "space       ( )", "tilde       (~)", "backtick    (`)", "semicolon   (;)",         # :   ...
                    "broken pipe (¦)", "other..."                                                                )         # :   ...
    OPTS <- base::c(".", "\t", "|", "^", ":", ",", " ", "~", "`", ";", "¦", "...")                                         # : associated vector of single-character delimiters
    d <- uj::choose1(opts, "What delimiter does your ", type, " use?", clear = T)                                          # : ask the user to choose a delimiter from among the user-friendly vector
    d <- OPTS[opts == d]                                                                                                   # : get the associated single-character delimiter
    if (d == "...") {d <- uj::ask("Enter a single-character delimiter to use:")}                                           # : IF user selects other THEN ask for a onechar scalar
  }                                                                                                                        # END IF
  errs <- NULL                                                                                                             # init error bank
  if (!uj::.cmp_ch1_scl(d            )) {errs <- base::c(errs, "[d] must be a complete onechar scalar (?cmp_ch1_scl).")}   # IF [d] is not a complete ch1 scalar THEN bank an error
  if (!uj::.cmp_str_scl(type         )) {errs <- base::c(errs, "[type] must be a complete string scalar (?cmp_str_scl).")} # IF [type] is not a complete string scalar THEN bank an error
  if (!uj::nll_or(file, "cmp_str_scl")) {errs <- base::c(errs, "[file] must be a complete string scalar (?cmp_str_scl).")} # IF [file] is neither [NULL] nor a complete string scalar THEN bank an error
  if (!base::is.null(errs            )) {uj::stopperr(errs)}                                                               # IF there are any banked errors THEN stop
  readr::read_delim(uj:::rd_filename(type, file, d), delim = d, ...)                                                       # read a delimited file
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
