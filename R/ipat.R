# fixed pattern indexing error checking
.pat_errs <- function(x, pat, FUN, PKG, STACK) {
  uj::errs_if_nots(uj::cmp_chr_vec(x)  , "[x] must be a complete character vec (?cmp_chr_vec)."  ,
                   uj::cmp_str_scl(pat), "[pat] must be a complete string scalar (?cmp_str_scl).", FUN = FUN, PKG = PKG, STACK = STACK)
}

#' @encoding UTF-8
#' @family strings
#' @title Simplified string indexing for fixed patterns
#' @description Logically index, numerically index, and count the number of times a pattern occurs.
#' @details
#' \tabular{ll}{  `has_pat`   \tab Evaluates whether each element of `x` contains the string in `pat`.                                                        \cr   \tab   \cr
#'                `ppats`    \tab Identifies the position of the first character of *every* instance of a pattern in each element of a character vector.      \cr   \tab   \cr
#'                `ppat`     \tab Identifies the position of *the first* character of the first instance of a pattern in each element of a character vector.  \cr   \tab   \cr
#'                `npat`     \tab Counts the number of times a pattern appears in each element of a character vector.                                         \cr   \tab   \cr
#'                `ipat`     \tab Logically indexes character vector elements containing a pattern as `TRUE` or `FALSE`.                                      \cr   \tab   \cr
#'                `wpat`     \tab Numerically indexes character vector elements containing a pattern as integer element numbers.                                             }
#' \cr\cr These functions Always \link[=a]{atomizes} `...` to create a single character vector to search for a fixed pattern.
#' @param x A character vector.
#' @param pat A fixed value \link[=cmp_str_scl]{complete string scalar} pattern to search for in `x`.
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
ipat <- function(x, pat) {
  uj:::.pat_errs(x, pat, "ipat", "uj", uj::callers())
  uj::av(base::grepl(pat, x, fixed = T))
}

#' @rdname ipat
#' @export
has_pat <- function(x, pat) {
  uj:::.pat_errs(x, pat, "haspat", "uj", uj::callers())
  uj::av(uj::LEN(x) != uj::LEN(base::gsub(pat, "", x, fixed = T)))
}

#' @rdname ipat
#' @export
wpat <- function(x, pat) {
  uj:::.pat_errs(x, pat, "wpat", "uj", uj::callers())
  uj::av(base::grep(pat, x, fixed = T))
}

#' @rdname ipat
#' @export
npat <- function(x, pat) {
  uj:::.pat_errs(x, pat, "npat", "uj", uj::callers())
  grgxpr <- function(xx) {
    yy <- uj::av(base::gregexpr(pat, xx))
    nn <- uj::N(yy)
    uj::f0(nn > 1, nn, uj::f0(yy == -1, 0, 1))
  }
  uj::av(base::sapply(x, grgxpr))
}

#' @rdname ipat
#' @export
ppat <- function(x, pat) {
  uj:::.pat_errs(x, pat, "ppat", "uj", uj::callers())
  y <- base::regexpr(pat, x, fixed = T)
  y[y == -1] <- NA
  if (base::all(uj::na(y))) {y <- NA}
  uj::av(y)
}

#' @rdname ipat
#' @export
ppats <- function(x, pat) {
  uj:::.pat_errs(x, pat, "ppats", "uj", uj::callers())
  grgxpr <- function(xx) {yy <- uj::av(base::gregexpr(pat, xx)); yy[yy == -1] <- NA; yy}
  base::lapply(x, grgxpr)
}
