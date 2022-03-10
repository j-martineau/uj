#' @name ss
#' @family strings
#' @title Split strings and select/check for elements
#' @details \strong{\code{ss}} splits strings using the delimiter(s) in
#'   \code{d} following these sequential steps:
#'   \enumerate{
#'     \item Reduce \code{...} to an atomic vector containing the constituent
#'           atomic elements of each argument in \code{...}.
#'     \item Convert the result to mode character.
#'     \item Replace each element of the result with that element's constituent
#'           parts as delimited by \code{d}, producting a potentially longer
#'           character vector.
#'     \item If \code{n} is not \code{NULL}, extracts the \code{n}-th
#'           elements(s) from the result.
#'     \item If \code{trm} is \code{TRUE}, trims whitespace (i.e., spaces, tabs,
#'           newlines) from both ends of each element of the result.
#'     \item If \code{sqz} is \code{TRUE}, removes blank strings elements
#'           (i.e., \code{""}) from the result.
#'     \item If \code{u} is \code{TRUE}, reduces the result to unique values.
#'   }
#'   \cr\cr
#'   \strong{\code{ch}} splits strings into their constituent characters
#'   following these sequential steps:
#'   \enumerate{
#'     \item Reduce \code{...} to an atomic vector containing the constituent
#'           atomic elements of each argument in \code{...}.
#'     \item Convert the result to mode character.
#'     \item Replace each element of the result with a character vector of that
#'           element's constituent characters, producing a potentially longer
#'           character vector.
#'     \item If \code{trm} is \code{TRUE}, trims whitespace (i.e., spaces, tabs,
#'           newlines) from both ends of each element of the result.
#'     \item If \code{sqz} is \code{TRUE}, removes characters that are neither
#'           letters, digits, nor spaces from the result.
#'     \item If \code{n} is not \code{NULL}, extracts the \code{n}-th
#'           elements(s) from the result.
#'     \item If \code{u} is \code{TRUE}, reduces the result to unique values.
#'   }
#'   \strong{Extensions}
#'   \cr Functions are extended for specific delimiters, signified by the
#'   following extension characters:
#'   \cr\tabular{lll}{
#'     \strong{Character}\tab\strong{Name}\tab\strong{Delimiter Invoked}
#'     \cr\code{'P'}     \tab Pipe        \tab \code{"|"}
#'     \cr\code{'D'}     \tab Dot         \tab \code{"."}
#'     \cr\code{'B'}     \tab Broken pipe \tab \code{"¦"}
#'   }
#'   \strong{\code{ssP}, \code{ssD}, and \code{ssB}}
#'   \cr Split strings using, respectively, the delimiters \code{'|'},
#'   \code{'.'}, and \code{'¦'}.
#'   \cr\cr
#'   \strong{\code{ssPD}, \code{ssPB}, \code{ssDB}}
#'   \cr split strings using, respectively, the following pairs of delimiters:
#'   \code{c('|', '.')}, \code{c('|', '¦')}, and \code{c('.', '¦')}.
#'   \cr\cr
#'   \strong{\code{ssPDB}}
#'   \cr Splits strings using the delimiters \code{c('|', '.', '¦')}.
#' @param ... An arbitrary number of objects to be split
#'   (\code{\link[=a]{atomized}} before splitting).
#' @param d A character vect of delimiters.
#' @param trm Logical scalar indicating whether to trim whitespace from each
#'   element of the result.
#' @param sqz Logical scalar indicating whether to squeeze the result by
#'   removing either empty strings (for \code{ss(.)} functions) or characters
#'   that are neither letters, digits, nor spaces (for \code{ch(.)}).
#' @param n An optional integer scalar giving the number of the split or
#'   character to extract.
#' @param u Logical scalar indicating whether to reduce the result to unique
#'   values.
#' @return A character vector (when \code{vals = NULL}) or a logical vector
#'   (when \code{vals} is not \code{NULL}).
#' @export
ss <- function(..., d = "|", trm = T, sqz = T, u = F, n = NULL) {
  VX <- all(sapply(list(...), xchr))
  VD <- cmp_chr_vec(d)
  VT <- isTF(trm); VS <- isTF(sqz); VU <- isTF(u)
  VN <- f0(xnll(n), T, cmp_psw_vec(n))
  E <- NULL
  if (!VX) {E <- c(E, "\n  * [...] must contain at least one argument, all of which must be character generics.")}
  if (!VD) {E <- c(E, "\n  * [d] must be a complete character vect.")}
  if (!VT) {E <- c(E, "\n  * [trm] must be TRUE or FALSE.")}
  if (!VS) {E <- c(E, "\n  * [sqz] must be TRUE or FALSE.")}
  if (!VU) {E <- c(E, "\n  * [u] must be TRUE or FALSE.")}
  if (!VN) {E <- c(E, "\n  * [n] must be NULL or a complete, positive, whole-number vect.")}
  if (xdef(E)) {stop(E)}
  x <- av(...)
  for (D in d) {x <- av(strsplit(as.character(av(x)), D, fixed = T))}
  if (trm) {x <- trimws(x)}
  if (sqz) {x <- x[x != ""]}
  if (!xnll(n)) {x <- x[n]}
  if (u) {x <- unique(x)}
  x
}

#' @rdname ss
#' @export
ssP <- function(..., trm = T, sqz = T, n = NULL, u = F) {ss(..., d = "|", trm = trm, sqz = sqz, n = NULL, u = F)}

#' @rdname ss
#' @export
ssD <- function(..., trm = T, sqz = T, n = NULL, u = F) {ss(..., d = ".", trm = trm, sqz = sqz, n = NULL, u = F)}

#' @rdname ss
#' @export
ssB <- function(..., trm = T, sqz = T, n = NULL, u = F) {ss(..., d = "¦", trm = trm, sqz = sqz, n = NULL, u = F)}

#' @rdname ss
#' @export
ssPD <- function(..., trm = T, sqz = T, n = NULL, u = F) {ss(..., d = c("|", "."), trm = trm, sqz = sqz, n = NULL, u = F)}

#' @rdname ss
#' @export
ssPB <- function(..., trm = T, sqz = T, n = NULL, u = F) {ss(..., d = c("|", "¦"), trm = trm, sqz = sqz, n = NULL, u = F)}

#' @rdname ss
#' @export
ssDB <- function(..., trm = T, sqz = T, n = NULL, u = F) {ss(..., d = c(".", "¦"), trm = trm, sqz = sqz, n = NULL, u = F)}

#' @rdname ss
#' @export
ssPDB <- function(..., trm = T, sqz = T, n = NULL, u = F) {ss(..., d = c("|", ".", "¦"), trm = trm, sqz = sqz, n = NULL, u = F)}

#' @rdname ss
#' @export
ch <- function(..., trm = T, sqz = T, n = NULL, u = F) {
  VX <- all(sapply(list(...), xchr))
  VT <- isTF(trm); VS <- isTF(sqz); VU <- isTF(u)
  VN <- f0(is.null(n), T, cmp_psw_vec(n))
  E <- NULL
  if (!VX) {E <- c(E, "\n  * [...] must contain at least one argument, all of which must be character generics.")}
  if (!VT) {E <- c(E, "\n  * [trm] must be TRUE or FALSE.")}
  if (!VS) {E <- c(E, "\n  * [sqz] must be TRUE or FALSE.")}
  if (!VU) {E <- c(E, "\n  * [u] must be TRUE or FALSE.")}
  if (!VN) {E <- c(E, "\n  * [n] must be NULL or a complete, positive, whole-number vect.")}
  if (xdef(E)) {stop(E)}
  x <- ss(av(...), d = "", trm = trm, u = u)
  if (sqz) {x <- x[x %in% c(letters, LETTERS, 0:9, " ")]}
  if (xdef(n)) {x <- x[n]}
  x
}
