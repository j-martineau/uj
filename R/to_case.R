#' @encoding UTF-8
#' @family conversions
#' @family strings
#' @title Convert string Case
#' @description Convert among lower, sentence, title, and upper cases.
#' @details Functions in this family convert string Case as follows:
#' \tabular{ll}{  `totitle, tc`   \tab Convert to \link[stringr:str_to_sentence]{title Case}.    \cr
#'                `tosent, sc`    \tab Convert to \link[stringr:str_to_sentence]{sentence Case}. \cr
#'                `tocase`        \tab Convert to Case `Case`.                                   \cr
#'                `lc`            \tab Convert to \link[base:tolower]{lower Case}.               \cr
#'                `uc`            \tab Convert to \link[base:toupper]{upper Case}.                 }
#' @param ... An arbitrary number of non-empty character arguments.
#' @param Case A Case insensitive \link[=cmp_ch1_scl]{complete onechar scalar}. Either `'l'`, `'s'`, `'t'`, or `'u'` to indicate lower, sentence, title, or upper Case, respectively.
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
tocase <- function(Case, ...) {
  X <- uj::av(...)
  OkCase <- uj:::.cmp_chr_scl(Case, Valid = base::c("l", "s", "t", "u"))
  Errors <- NULL
  if (!OkCase) {Errors <- base::c(Errors, "[Case] must be either 'l', 's', 't', or 'u'.")}
  if (!uj:::.cmp_chr_vec(X)) {Errors <- base::c(Errors, "[...] must be contain character values.")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  if      (Case == "l") {base::tolower(X)}
  else if (Case == "u") {base::toupper(X)}
  else if (Case == "t") {stringr::str_to_title(X)}
  else                  {stringr::str_to_sentence(X)}
}

#' @rdname tocase
#' @export
tosent <- function(...) {uj::tocase("s", ...)}

#' @rdname tocase
#' @export
totitle <- function(...) {uj::tocase("t", ...)}

#' @rdname tocase
#' @export
lc <- function(...) {uj::tocase("l", ...)}

#' @rdname tocase
#' @export
sc <- function(...) {uj::tocase("s", ...)}

#' @rdname tocase
#' @export
tc <- function(...) {uj::tocase("t", ...)}

#' @rdname tocase
#' @export
uc <- function(...) {uj::tocase("u", ...)}
