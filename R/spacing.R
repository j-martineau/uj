#' @name spacing
#' @family strings
#' @title Simple spacing functions
#' @description Create a string of spaces.
#' @param ... An arbitrary number of atomic arguments to be processed.
#' @param n. Whole-number scalar indicating the number of spaces or pad
#'   characters.
#' @param s. Character scalar "side" padded ("l", "r", or "b" for left, right,
#'   or both).
#' @param p. Character scalar containing a single character used to pad strings.
#' @examples
#' spaces(0)
#' spaces(4)
#' pad(0:9, n. = 3)
#' pad(0:9, n. = 3, s. = "r")
#' pad(0:9, n. = 3, s. = "l")
#' pad(0:9, n. = 3, s. = "b")
#' pad(0:9, n. = 3, s. = "b", p. = "*")
#' trm(c("a ", "b", "c", " ", "d  ", ""))
#' sqz(c("a ", "b", "c", " ", "d  ", ""))
#' sqz(trm(c("a ", "b", "c", " ", "d  ", "")))
#' @export
spaces <- function(n.) {
  if (!cmp_nnw_scl(n.)) {stop("\n • [n.] must be a positive, whole-number scalar.")}
  paste0(rep(" ", n.), collapse = "")
}

#' @describeIn spacing Pad a string with leading and/or trailing spaces
#'    using \code{\link[stringr]{pad}}.
#' @export
pad <- function(..., n. = 0, s. = "r", p. = " ") {
  vx. <- all(sapply(list(...), cmp_chr))
  vn. <- cmp_nnw_scl(n.)
  vs. <- isIN(s., "l", "r", "b")
  vp. <- cmp_ch1_scl(p.)
  err. <- NULL
  if (!vx.) {err. <- c(err., "\n • [...] must contain at least one argument, and all must be complete text vects.")}
  if (!vn.) {err. <- c(err., "\n • [n.] must be a non-NA, non-negative, whole-number scalar.")}
  if (!vs.) {err. <- c(err., "\n • [s.] must be 'r', 'c', or 'b'.")}
  if (!vp.) {err. <- c(err., "\n • [p.] must be a single-character scalar.")}
  if (idef(err.)) {stop(err.)}
  sapply(av(...), stringr::str_pad, width = n., side = s., pad = p.)
}

#' @describeIn spacing Trim leading, trailing, and extra internal spaces
#'   spaces using \code{\link[stringr]{str_squish}}.
#' @export
sqz <- function(...) {
  if (!all(sapply(list(...), cmp_chr))) {stop("\n • [...] must contain at least one argument, and all must be complete character vects.")}
  stringr::str_squish(av(...))
}

#' @describeIn spacing Trim leading and trailing spaces using
#'   \code{\link[stringr]{str_trim}}.
#' @export
trm <- function(...) {
  if (!all(sapply(list(...), cmp_chr))) {stop("\n • [...] must contain at least one argument, and all must be complete character vects.")}
  stringr::str_trim(av(...))
}
