#' @name spacing
#' @family strings
#' @title Simple spacing functions
#' @description Create a string of spaces, pad a string with spaces, trim spaces
#'   from strings, and squeeze extra spaces from the middle of strings.
#' @details \strong{\code{spaces}}
#'   \cr Creates a string of \code{n.} spaces.
#'   \cr\cr
#'   \strong{\code{pad}}
#'   \cr Pads strings to length \code{N} using the padding character \code{p} on
#'   side(s) \code{s.} (via \code{\link[stringr]{str_pad}}).
#'   \cr\cr
#'   \strong{\code{trm}}
#'   \cr Trims whitespace the the left and right ends of strings (via
#'   \code{\link[stringr]{str_trim}}).
#'   \cr\cr
#'   \strong{\code{sqz}}
#'   \cr Trims whitespace from the left and right ends and extra whitespace from
#'   the middle of strings (via \code{\link[stringr]{str_squish}}).
#' @param ... An arbitrary number of atomic arguments to be processed.
#' @param n. Whole-number scalar indicating the number of spaces or pad
#'   characters.
#' @param s. Character scalar "side" padded ("l", "r", or "b" for left, right,
#'   or both).
#' @param p. Character scalar containing a single character used to pad strings.
#' @examples
#' spaces(0)
#' spaces(4)
#' pad(0:9, N = 3)
#' pad(0:9, N = 3, S = "r")
#' pad(0:9, N = 3, S = "l")
#' pad(0:9, N = 3, S = "b")
#' pad(0:9, N = 3, S = "b", P = "*")
#' trm(c("a ", "b", "c", " ", "d  ", ""))
#' sqz(c("a ", "b", "c", " ", "d  ", ""))
#' sqz(trm(c("a ", "b", "c", " ", "d  ", "")))
#' @export
spaces <- function(n.) {
  if (!cmp_nnw_scl(n.)) {stop("\n  * [n.] must be a positive, whole-number scalar.")}
  paste0(rep(" ", n.), collapse = "")
}

#' @rdname spacing
#' @export
pad <- function(..., n. = 0, s. = "r", p. = " ") {
  VX <- all(sapply(list(...), cmp_chr))
  VN <- cmp_nnw_scl(n.)
  VS <- isIN(s., "l", "r", "b")
  VP <- cmp_ch1_scl(p.)
  E <- NULL
  if (!VX) {E <- c(E, "\n  * [...] must contain at least one argument, and all must be complete text vects.")}
  if (!VN) {E <- c(E, "\n  * [n.] must be a non-NA, non-negative, whole-number scalar.")}
  if (!VS) {E <- c(E, "\n  * [s.] must be 'r', 'r', or 'b'.")}
  if (!VP) {E <- c(E, "\n  * [p.] must be a single-character scalar.")}
  if (xdef(E)) {stop(E)}
  sapply(av(...), stringr::str_pad, width = n., side = s., pad = p.)
}

#' @rdname spacing
#' @export
sqz <- function(...) {
  if (!all(sapply(list(...), cmp_chr))) {stop("\n  * [...] must contain at least one argument, and all must be complete character vects.")}
  stringr::str_squish(av(...))
}

#' @rdname spacing
#' @export
trm <- function(...) {
  if (!all(sapply(list(...), cmp_chr))) {stop("\n  * [...] must contain at least one argument, and all must be complete character vects.")}
  stringr::str_trim(av(...))
}
