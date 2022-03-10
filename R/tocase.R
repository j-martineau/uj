#' @name tocase
#' @family strings
#' @title Convert String Case
#' @description Convert strings to lower, upper, sentence, and title cases
#'   (supplements \code{tolower} and \code{toupper}).
#' @description \strong{\code{tocase}}
#'   \cr Converts to the case specified in \code{case}.
#'   \cr\cr
#'   \strong{\code{tosentence, sc}}
#'   \cr Converts to sentence case via \code{\link[stringr]{str_to_sentence}}.
#'   \cr\cr
#'   \strong{\code{totitle, tc}}
#'   \cr Converts to sentence case via \code{\link[stringr]{str_to_title}}.
#'   \cr\cr
#'   \strong{\code{lc}}
#'   \cr Converts to lower case via \code{\link[base]{tolower}}.
#'   \cr\cr
#'   \strong{\code{uc}}
#'   \cr Converts to upper case via \code{\link[base]{toupper}}.
#' @param ... An arbitrary number of atomic arguments to be processed.
#' @param case \code{'l'}, \code{'s'}, \code{'t'}, or \code{'u'} to indicate
#'   lower, sentence, title, or upper case. Case insensitive.
#' @param a .\code{TRUE} or \code{FALSE} indicating whether to
#'   \link[=a.]{atomize} \code{...} before processing.
#' @examples
#' TestTitle    <- "This is a Title"
#' TestSentence <- "This is a sentence."
#' TestWords    <- "this is an ordered group of words"
#' TestSet      <- c("a", "set", "of", "words")
#' TestVector   <- c(TestTitle, TestSentence, TestWords, TestSet)
#' lc(TestVector)
#' lc(TestTitle, TestSentence, TestWords, TestSet)
#' sc(TestTitle, TestSentence, TestWords, TestSet)
#' tc(TestTitle, TestSentence, TestWords, TestSet)
#' uc(TestTitle, TestSentence, TestWords, TestSet)
#' scase(TestTitle, TestSentence, TestWords, TestSet, case = "LOWER")
#' scase(TestVector, case = "l")
#' scase(TestVector, case = "s")
#' scase(TestTitle, TestSentence, TestWords, TestSet, case = "T")
#' scase(TestVector, case = "UPPER")
#' @export
tocase <- function(case, ..., a. = T) {
  x <- list(...)
  N <- length(x)
  AA <- all(sapply(x), xchr)
  AT <- all(sapply(x), chr_atb)
  AL <- all(sapply(x), chr_avl)
  if (N > 0) {L <- lengths(x)} else {L <- 1}
  VC <- isIN(case, c("l", "s", "t", "u"))
  VX <- f0(N == 0, F, f0(any(lengths(x) == 0), F,
        f0(all(sapply(x, is.atomic)), T, AA | AT | AL)))
  VA <- isTF(a.)
  E <- NULL
  if (!VC) {E <- c(E, "\n  * [case] must be 'l', 's', 't', or 'u'.")}
  if (!VX) {E <- c(E, "\n  * [...] must be contain non-empty character objects, character tibbles, or character vlists.")}
  if (!VA) {E <- c(E, "\n  * [a.] must be TRUE or FALSE.")}
  if (xdef(E)) {stop(E)}
  if (isT(a.)) {x <- list(av(...))} else {x <- list(...)}
  for (i in 1:length(x)) {
    X <- x[[i]]
    if (xchr(X)) {
      if      (case == "l") {X <- tolower(X)}
      else if (case == "u") {X <- toupper(X)}
      else if (case == "t") {X <- stringr::str_to_title(X)}
      else if (case == "s") {X <- stringr::str_to_sentence(X)}
    }
    else {
      if      (case == "l") {X <- apply(X, 2, tolower)}
      else if (case == "u") {X <- apply(X, 2, toupper)}
      else if (case == "t") {X <- apply(X, 2, stringr::str_to_title)}
      else if (case == "s") {X <- apply(X, 2, stringr::str_to_sentence)}
    }
    x[[i]] <- X
  }
  if (length(x) == 1) {x <- x[[1]]}
  x
}

#' @rdname tocase
#' @export
tosentence <- function(..., a. = T) {tocase("s", ..., a. = a.)}

#' @rdname tocase
#' @export
totitle <- function(..., a. = T) {tocase("t", ..., a. = a.)}

#' @rdname tocase
#' @export
lc <- function(..., a. = T) {tocase("l", ..., a. = a.)}

#' @rdname tocase
#' @export
sc <- function(..., a. = T) {tocase("s", ..., a. = a.)}

#' @rdname tocase
#' @export
tc <- function(..., a. = T) {tocase("t", ..., a. = a.)}

#' @rdname tocase
#' @export
uc <- function(..., a. = T) {tocase("u", ..., a. = a.)}
