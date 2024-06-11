#' @encoding UTF-8
#' @family conversions
#' @family strings
#' @title Convert string case
#' @description Convert among lower, sentence, title, and upper cases.
#' @param ... An arbitrary number of non-empty character arguments.
#' @param case A case insensitive \link[=cmp_ch1_scl]{complete onechar scalar}. Either `'l'`, `'s'`, `'t'`, or `'u'` to indicate lower, sentence, title, or upper case, respectively.
#' @return A character vector.
#' @examples
#' egSentence <- "This is a sentence."
#' egTitle <- "This is a Title"
#' egWords <- c("word1", "word2")
#' egList <- list(egWords, "word3")
#'
#' egWords
#' egList
#'
#' tc(egSentence, egTitle)
#' sc(egSentence, egTitle)
#' lc(egSentence, egTitle)
#' uc(egSentence, egTitle)
#'
#' tosent(egSentence, egTitle, egWords, egList)
#' totitle(egSentence, egTitle, egWords, egList)
#'
#' tocase("s", egTitle, egList)
#' tocase("u", egList)
#' @export
tocase <- function(case, ...) {
  x <- uj::av(...)
  okCase <- uj::.cmp_chr_scl(case, valid = base::c("l", "s", "t", "u"))
  errs <- NULL
  if (!okCase) {errs <- base::c(errs, "[case] must be either 'l', 's', 't', or 'u'.")}
  if (!uj::.cmp_chr_vec(x)) {errs <- base::c(errs, "[...] must be contain character values.")}
  if (!base::is.null(errs)) {uj::stopperr(errs)}
  if      (case == "l") {base::tolower(x)}
  else if (case == "u") {base::toupper(x)}
  else if (case == "t") {stringr::str_to_title(x)}
  else                  {stringr::str_to_sentence(x)}
}

#' @describeIn tocase Converts to \link[stringr:str_to_sentence]{sentence case}.
#' @export
tosent <- function(...) {uj::tocase("s", ...)}

#' @describeIn tocase Converts to \link[stringr:str_to_sentence]{title case}.
#' @export
totitle <- function(...) {uj::tocase("t", ...)}

#' @describeIn tocase Converts to \link[base:tolower]{lower case}.
#' @export
lc <- function(...) {uj::tocase("l", ...)}

#' @describeIn tocase Converts to \link[stringr:str_to_sentence]{sentence case}.
#' @export
sc <- function(...) {uj::tocase("s", ...)}

#' @describeIn tocase Converts to \link[stringr:str_to_sentence]{title case}.
#' @export
tc <- function(...) {uj::tocase("t", ...)}

#' @describeIn tocase Converts to \link[base:toupper]{upper case}.
#' @export
uc <- function(...) {uj::tocase("u", ...)}
