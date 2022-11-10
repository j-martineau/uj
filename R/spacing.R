#' @name spacing.
#' @family strings
#' @title Simple spacing functions
#' @param ... An arbitrary number of atomic arguments to be processed.
#' @param n \link[cmp_psw_scl]{Complete positive whole-number scalar} indicating
#'   the number of spaces.
#' @param n. \link[cmp_psw_scl]{Complete positive whole-number scalar}
#'   indicating the number of pad characters.
#' @param s. \link[cmp_ch1_scl]{Complete onechar scalar} indicate side(s) to
#'   pad: "l", "r", or "b" for left, right, or both, respectively.
#' @param p. \link[cmp_ch1_scl]{Complete onechar scalar} containing a single
#'   character used to pad strings.
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
spacing. <- function() {help("spacing.", package = "uj")}

#' @describeIn spacing. Create a character vector of \code{n.} spaces.
#' @export
spaces <- function(n) {
  if (!cmp_nnw_scl(n)) {stop("\n • [n] must be a complete positive whole-number scalar (?cmp_psw_scl).")}
  paste0(rep(" ", n), collapse = "")
}

#' @describeIn spacing. Pad a string with leading and/or trailing spaces
#'    using \code{\link[stringr]{pad}}.
#' @export
pad <- function(..., n. = 0, s. = "r", p. = " ") {
  vx <- all(sapply(list(...), cmp_chr))
  vn <- cmp_nnw_scl(n.)
  vs <- isIN(s., "l", "r", "b")
  vp <- cmp_ch1_scl(p.)
  err <- NULL
  if (!vx) {err <- c(err, "\n • [...] must contain at least one argument, and all must be complete character vecs (?cmp_chr_vec).")}
  if (!vn) {err <- c(err, "\n • [n.] must be a complete non-negative whole-number scalar (?cmp_nnw_scl).")}
  if (!vs) {err <- c(err, "\n • [s.] must be a complete onechar scalar (?cmp_ch1_scl) in c('r', 'l', 'b').")}
  if (!vp) {err <- c(err, "\n • [p.] must be a complete onechar scalar (?cmp_ch1_scl).")}
  if (idef(err)) {stop(err)}
  sapply(av(...), stringr::str_pad, width = n., side = s., pad = p.)
}

#' @describeIn spacing. Trim leading, trailing, and extra internal spaces
#'   using \code{\link[stringr]{str_squish}}.
#' @export
sqz <- function(...) {
  if (!all(sapply(list(...), cmp_chr))) {stop("\n • [...] must contain at least one argument, and all must be complete character vecs (?cmp_chr_vec).")}
  stringr::str_squish(av(...))
}

#' @describeIn spacing. Trim leading and trailing spaces using
#'   \code{\link[stringr]{str_trim}}.
#' @export
trm <- function(...) {
  if (!all(sapply(list(...), cmp_chr))) {stop("\n • [...] must contain at least one argument, and all must be complete character vecs (?cmp_chr_vec)")}
  stringr::str_trim(av(...))
}
