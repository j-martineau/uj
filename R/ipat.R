#' @encoding UTF-8
#' @family strings
#' @title Simplified string indexing for fixed patterns
#' @description Logically index, numerically index, and count the number of times a pattern occurs.
#' @details
#' \tabular{ll}{  `has_pat`   \tab Evaluates whether each element of `X` contains the string in `Pat`.                                                        \cr   \tab   \cr
#'                `ppats`     \tab Identifies the position of the first character of *every* instance of a pattern in each element of a character vector.      \cr   \tab   \cr
#'                `ppat`      \tab Identifies the position of *the first* character of the first instance of a pattern in each element of a character vector.  \cr   \tab   \cr
#'                `npat`      \tab Counts the number of times a pattern appears in each element of a character vector.                                         \cr   \tab   \cr
#'                `ipat`      \tab Logically indexes character vector elements containing a pattern as `TRUE` or `FALSE`.                                      \cr   \tab   \cr
#'                `wpat`      \tab Numerically indexes character vector elements containing a pattern as integer element numbers.                                             }
#' \cr\cr These functions Always \link[=a]{atomizes} `...` to create a single character vector to search for a fixed pattern.
#' @param X A character vector.
#' @param Pat A fixed value \link[=cmp_str_scl]{complete string scalar} pattern to search for in `X`.
#' @return **A positive whole number \link[=VLS]{vlist}** \cr\cr `ppats`
#' \cr\cr  **A positive whole number vector**             \cr\cr `wpat, npat, ppat`
#' \cr\cr  **A logical scalar**                           \cr\cr `has_pat, ipat`
#' @examples
#' words <- c("apple", "banana", "carrot")
#' has_pat(words, "a")
#' ipat(words, "a")
#' has_pat(words, "b")
#' ipat(words, "b")
#' wpat(words, "a")
#' wpat(words, "b")
#' npat(words, "a")
#' npat(words, "b")
#' ppat(words, "a")
#' ppat(words, "b")
#' ppats(words, "a")
#' ppats(words, "b")
#' @export
ipat <- function(X, Pat) {
  uj:::.pat_errs(X, Pat, uj::callers())
  uj::av(base::grepl(Pat, X, fixed = T))
}

#' @rdname ipat
#' @export
has_pat <- function(X, Pat) {
  uj:::.pat_errs(X, Pat, uj::callers())
  uj::av(base::nchar(X) != base::nchar(base::gsub(Pat, "", X, fixed = T)))
}

#' @rdname ipat
#' @export
wpat <- function(X, Pat) {
  uj:::.pat_errs(X, Pat, uj::callers())
  uj::av(base::grep(Pat, X, fixed = T))
}

#' @rdname ipat
#' @export
npat <- function(X, Pat) {
  uj:::.pat_errs(X, Pat, uj::callers())
  grgxpr <- function(x) {
    y <- uj::av(base::gregexpr(Pat, x))
    n <- base::length(y)
    if (n > 1) {n} else if (y == -1) {0} else {1}
  }
  uj::av(base::sapply(X, grgxpr))
}

#' @rdname ipat
#' @export
ppat <- function(X, Pat) {
  uj:::.pat_errs(X, Pat, uj::callers())
  Y <- base::regexpr(Pat, X, fixed = T)
  Y[Y == -1] <- NA
  if (base::all(base::is.na(Y))) {Y <- NA}
  uj::av(Y)
}

#' @rdname ipat
#' @export
ppats <- function(X, Pat) {
  uj:::.pat_errs(X, Pat, uj::callers())
  grgxpr <- function(x) {y <- uj::av(base::gregexpr(Pat, x)); y[y == -1] <- NA; y}
  base::lapply(X, grgxpr)
}
