#' @name tocase.
#' @family strings
#' @title Convert string case
#' @description Convert strings to lower, upper, sentence, and title cases
#'   (supplements \code{tolower} and \code{toupper}).
#' @description Convert to the case specified in \code{case}.
#' @param ... An arbitrary number of atomic arguments to be processed.
#' @param case. \link[cmp_ch1_scl]{Complete onechar scalar}: \code{'l'},
#'   \code{'s'}, \code{'t'}, or \code{'u'} to indicate lower, sentence, title,
#'   or upper case, respectively. Case insensitive.
#' @param a. \link[cmp_lgl_scl]{Complete logical scalar} indicating whether to
#'  \link[a]{atomize} \code{...} before processing.
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
tocase. <- function() {help("tocase.", package = "j")}

#' @describeIn tocase. Convert among lower, upper, sentence, and title case.
#' @export
tocase <- function(case., ..., a. = T) {
  x <- list(...)
  n <- length(x)
  all.chr <- all(sapply(x), ichr)
  all.dtf <- all(sapply(x), chr_dtf)
  all.vls <- all(sapply(x), chr_vls)
  if (n > 0) {ns <- lengths(x)} else {ns <- 1}
  ok.case <- isIN(case., c("l", "s", "t", "u"))
  ok.dots <- f0(n == 0, F, f0(any(lengths(x.) == 0), F, f0(all(sapply(x., is.atomic)), T, all.chr | all.dtf | all.vls)))
  errs <- c(f0(ok.case , NULL, "\n \u2022 [case.] must be either 'l', 's', 't', or 'u'."),
            f0(ok.dots , NULL, "\n \u2022 [...] must be contain non-empty character objects (?ichr), character dtfs (?chr_dtf), or character vlists (?chr_vls)."),
            f0(isTF(a.), NULL, "\n \u2022 [a.] must be TRUE or FALSE."))
  if (idef(errs)) {stop(errs)}
  if (isT(a.)) {x <- list(av(x))}
  for (i in 1:length(x)) {
    xi <- x[[i]]
    if (ichr(xi)) {
      if      (case. == "l") {xi <- tolower(xi)}
      else if (case. == "u") {xi <- toupper(xi)}
      else if (case. == "t") {xi <- stringr::str_to_title(xi)}
      else if (case. == "s") {xi <- stringr::str_to_sentence(xi)}
    } else {
      if      (case. == "l") {xi <- apply(xi, 2, tolower)}
      else if (case. == "u") {xi <- apply(xi, 2, toupper)}
      else if (case. == "t") {xi <- apply(xi, 2, stringr::str_to_title)}
      else if (case. == "s") {xi <- apply(xi, 2, stringr::str_to_sentence)}
    }
    x[[i]] <- xi
  }
  if (length(x) == 1) {x <- x[[1]]}
  x
}

#' @describeIn tocase. Convert to sentence case via
#'   \code{\link[stringr]{str_to_sentence}}.
#' @export
tosentence <- function(..., a. = T) {tocase("s", ..., a. = a.)}

#' @describeIn tocase. Convert to sentence case via
#'   \code{\link[stringr]{str_to_title}}.
#' @export
totitle <- function(..., a. = T) {tocase("t", ..., a. = a.)}

#' @describeIn tocase. Converts to lower case via \code{\link[base]{tolower}}.
#' @export
lc <- function(..., a. = T) {tocase("l", ..., a. = a.)}

#' @describeIn tocase. Convert to sentence case via
#'   \code{\link[stringr]{str_to_sentence}}.
#' @export
sc <- function(..., a. = T) {tocase("s", ..., a. = a.)}

#' @describeIn tocase. Convert to title case via
#'   \code{\link[stringr]{str_to_title}}.
#' @export
tc <- function(..., a. = T) {tocase("t", ..., a. = a.)}

#' @describeIn tocase. Converts to upper case via \code{\link[base]{tolower}}.
#' @export
uc <- function(..., a. = T) {tocase("u", ..., a. = a.)}
