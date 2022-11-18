#' @name spacing.
#' @family strings
#' @title Error-Checked \code{stringr}-Based Spacing Functions
#' @description \tabular{ll}{
#'   \code{spaces}   \tab Create a character vector of \code{n} spaces.      \cr
#'   \code{pad}      \tab Pad a string with leading and/or trailing spaces using
#'                        \code{\link[stringr]{pad}}.                        \cr
#'   \code{sqz}      \tab Trim leading, trailing, and extra internal spaces
#'                        using \code{\link[stringr]{str_squish}}.           \cr
#'   \code{trm}      \tab Trim leading and trailing spaces using
#'                        \code{\link[stringr]{str_trim}}.                     }
#' @param ... An arbitrary number of atomic arguments to be processed.
#' @param n \link[=cmp_psw_scl]{Complete positive whole-number scalar}
#'   indicating the number of spaces.
#' @param n. \link[=cmp_psw_scl]{Complete positive whole-number scalar}
#'   indicating the number of pad characters.
#' @param s. \link[=cmp_ch1_scl]{Complete onechar scalar} indicate side(s) to
#'   pad: "l", "r", or "b" for left, right, or both, respectively.
#' @param p. \link[=cmp_ch1_scl]{Complete onechar scalar} containing a single
#'   character used to pad strings.
#' @return \tabular{lll}{
#'   \code{spaces}                     \tab   \tab A character scalar.       \cr
#'   \code{pad}, \code{trm}, \code{sqz}\tab   \tab A character object.         }
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

#' @rdname spacing.
#' @export
spaces <- function(n) {
  if (!cmp_nnw_scl(n)) {stop("\n \u2022 [n] must be a complete positive whole-number scalar (?cmp_psw_scl).")}
  paste0(rep(" ", n), collapse = "")
}

#' @rdname spacing.
#' @export
pad <- function(..., n. = 0, s. = "r", p. = " ") {
  errs <- c(f0(all(sapply(list(...), cmp_chr)), NULL, "\n \u2022 [...] must contain at least one argument, and all must be complete character vecs (?cmp_chr_vec)."),
            f0(cmp_nnw_scl(n.)                , NULL, "\n \u2022 [n.] must be a complete non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(isIN(s., "l", "r", "b")        , NULL, "\n \u2022 [s.] must be a complete onechar scalar (?cmp_ch1_scl) in c('r', 'l', 'b')."),
            f0(cmp_ch1_scl(p.)                , NULL, "\n \u2022 [p.] must be a complete onechar scalar (?cmp_ch1_scl)."))
  if (idef(errs)) {stop(errs)}
  sapply(av(...), stringr::str_pad, width = n., side = s., pad = p.)
}

#' @rdname spacing.
#' @export
sqz <- function(...) {
  if (!all(sapply(list(...), cmp_chr))) {stop("\n \u2022 [...] must contain at least one argument, and all must be complete character vecs (?cmp_chr_vec).")}
  stringr::str_squish(av(...))
}

#' @rdname spacing.
#' @export
trm <- function(...) {
  if (!all(sapply(list(...), cmp_chr))) {stop("\n \u2022 [...] must contain at least one argument, and all must be complete character vecs (?cmp_chr_vec)")}
  stringr::str_trim(av(...))
}
