#' @encoding UTF-8
#' @family conversions
#' @family strings
#' @title Convert string case
#' @description Functions in this family convert string case as follows:
#' \tabular{rl}{
#'      `totitle, tc` \tab   Convert to \link[stringr:str_to_sentence]{title case}.
#'   \cr `tosent, ts` \tab   Convert to \link[stringr:str_to_sentence]{sentence case}.
#'   \cr         `lc` \tab   Convert to \link[base:tolower]{lower case}.
#'   \cr         `uc` \tab   Convert to \link[base:toupper]{upper case}.
#'   \cr     `tocase` \tab   Convert to case `case.`.
#' }
#' @param ... An arbitrary number of non-empty character arguments.
#' @param case. A \link[=cmp_ch1_scl]{complete onechar scalar}. Either `'l'`, `'s'`, `'t'`, or `'u'` to indicate lower, sentence, title, or upper case, respectively (case insensitive).
#' @return A character vector.
#' @examples
#' sentence. <- "This is a sentence."
#' title. <- "This is a Title"
#' words. <- c("word1", "word2")
#' list. <- list(words., "word3")
#'
#' words.
#' list.
#'
#' tc(sentence., title.)
#' sc(sentence., title.)
#' lc(sentence., title.)
#' uc(sentence., title.)
#'
#' tosent(sentence., title., words., list.)
#' totitle(sentence., title., words., list.)
#'
#' tocase("s", title., list.)
#' tocase("u", list.)
#' @export
tocase <- function(case., ...) {
  library(stringr)
  x <- uj::av(...)
  ok.case <- uj::isIN(case., c("l", "s", "t", "u"))
  ok.dots <- uj::cmp_chr_vec(x)
  errs <- base::c(uj::f0(ok.case , NULL, "[case.] must be either 'l', 's', 't', or 'u'."),
                  uj::f0(ok.dots , NULL, "[...] must be contain character values."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  if      (case. == "l") {base::tolower(x)}
  else if (case. == "u") {base::toupper(x)}
  else if (case. == "t") {stringr::str_to_title(x)}
  else if (case. == "s") {stringr::str_to_sentence(x)}
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
