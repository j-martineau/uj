#' @encoding UTF-8
#' @family conversions
#' @family strings
#' @title Convert string case
#' @description Functions in this family convert string case as follows:
#' \tabular{rl}{
#'      `totitle, tc` \tab   Convert to \link[stringr:str_to_sentence]{title case}.
#'   \cr              \tab  
#'   \cr `tosent, ts` \tab   Convert to \link[stringr:str_to_sentence]{sentence case}.
#'   \cr              \tab  
#'   \cr         `lc` \tab   Convert to \link[base:tolower]{lower case}.
#'   \cr         `uc` \tab   Convert to \link[base:toupper]{upper case}.
#'   \cr              \tab  
#'   \cr     `tocase` \tab   Convert to case in argument `case.`.
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
  x <- av(...)
  ok.case <- isIN(case., c("l", "s", "t", "u"))
  ok.dots <- cmp_chr_vec(x)
  errs <- c(f0(ok.case , NULL, "[case.] must be either 'l', 's', 't', or 'u'."),
            f0(ok.dots , NULL, "[...] must be contain character values."))
  if (!is.null(errs)) {stop(.errs(errs))}
  if      (case. == "l") {tolower(x)}
  else if (case. == "u") {toupper(x)}
  else if (case. == "t") {stringr::str_to_title(x)}
  else if (case. == "s") {stringr::str_to_sentence(x)}
}

#' @rdname tocase
#' @export
tosent <- function(...) {tocase("s", ...)}

#' @rdname tocase
#' @export
totitle <- function(...) {tocase("t", ...)}

#' @rdname tocase
#' @export
lc <- function(...) {tocase("l", ...)}

#' @rdname tocase
#' @export
sc <- function(...) {tocase("s", ...)}

#' @rdname tocase
#' @export
tc <- function(...) {tocase("t", ...)}

#' @rdname tocase
#' @export
uc <- function(...) {tocase("u", ...)}
