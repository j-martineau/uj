#' @name tocase
#' @family strings
#' @title Convert string case
#' @description Functions in this family convert string case as follows:\tabular{rl}{
#'      `totitle,tc`   \tab To \link[stringr:str_to_title]{title case}.
#'   \cr `tosent,sc`   \tab To \link[stringr:str_to_sentence]{sentence case}.
#'   \cr    `tocase`   \tab Among cases.
#'   \cr        `lc`   \tab To \link[base:tolower]{lower case}.
#'   \cr        `uc`   \tab To \link[base:toupper]{upper case}.
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
  errs <- c(f0(ok.case , NULL, .errx("[case.] must be either 'l', 's', 't', or 'u'.")),
            f0(ok.dots , NULL, .errx("[...] must be contain character values.")))
  if (!is.null(errs)) {stop(errs)}
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
