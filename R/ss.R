#' @name ss.
#' @family strings
#' @title Split strings and select/check for elements
#' @description \strong{\code{ss}} splits strings using the delimiter(s) in
#'   \code{d.} following these sequential steps:\enumerate{
#'     \item Reduce \code{...} to an atomic vector containing the constituent
#'           atomic elements of each argument in \code{...}.
#'     \item Convert the result to mode character.
#'     \item Replace each element of the result with that element's constituent
#'           parts as delimited by \code{d.}, producting a potentially longer
#'           character vector.
#'     \item If \code{n.} is not \code{NULL}, extracts the \code{n.}-th
#'           elements(s) from the result.
#'     \item If \code{trm.} is \code{TRUE}, trims whitespace (i.e., spaces,
#'           tabs, newlines) from both ends of each element of the result.
#'     \item If \code{sqz.} is \code{TRUE}, removes blank strings elements
#'           (i.e., \code{""}) from the result.
#'     \item If \code{u.} is \code{TRUE}, reduces the result to unique values. }
#'   \strong{\code{ch}} splits strings into their constituent characters
#'   following these sequential steps:\enumerate{
#'     \item Reduce \code{...} to an atomic vector containing the constituent
#'           atomic elements of each argument in \code{...}.
#'     \item Convert the result to mode character.
#'     \item Replace each element of the result with a character vector of that
#'           element's constituent characters, producing a potentially longer
#'           character vector.
#'     \item If \code{trm.} is \code{TRUE}, trims white space (i.e., spaces,
#'           tabs, newlines) from both ends of each element of the result.
#'     \item If \code{sqz.} is \code{TRUE}, removes characters that are neither
#'           letters, digits, nor spaces from the result.
#'     \item If \code{n.} is not \code{NULL}, extracts the \code{n.}-th
#'           elements(s) from the result.
#'     \item If \code{u.} is \code{TRUE}, reduces the result to unique values. }
#'   \strong{Extensions}
#'   \cr Functions are extended for specific delimiters, signified by the
#'   following suffixes on function names:\tabular{lll}{
#'     SUFFIX     \tab SUFFIX       \tab DELIMITER                           \cr
#'     CHARACTER  \tab NAME         \tab INVOKED                             \cr
#'     \code{'1'} \tab Space        \tab \code{" "}                          \cr
#'     \code{'P'} \tab Pipe         \tab \code{"|"}                          \cr
#'     \code{'D'} \tab Dot          \tab \code{"."}                          \cr
#'     \code{'B'} \tab Broken pipe  \tab \code{"¦"}                            }
#' @param ... An arbitrary number of objects to be (\code{\link[a]{atomized}}
#'   before splitting).
#' @param x. Character scalar or vector of string(s) to be split.
#' @param s. Character scalar delimiter for splitting strings.
#' @param name. Character scalar name of the variable to hold the original
#'   strings.
#' @param part. Character scalar or character vector with as many elements as
#'   each string will be split into for naming each variable containing a part
#'   of each string.
#' @param d. A character vect of delimiters.
#' @param trm. Logical scalar indicating whether to trim whitespace from each
#'   element of the result.
#' @param sqz. Logical scalar indicating whether to squeeze the result by
#'   removing either empty strings (for \code{ss} functions) or characters
#'   that are neither letters, digits, nor spaces (for \code{ch}).
#' @param n. An optional integer scalar giving the number of the split or
#'   character to extract.
#' @param u. Logical scalar indicating whether to reduce the result to unique
#'   values.
#' @return A character vector (when \code{vals = NULL}) or a logical vector
#'   (when \code{vals} is not \code{NULL}).
#' @export
ss. <- function() {help("ss.", package = "uj")}

#' @describeIn ss. Flexible fixed-string string splitting function.
#' @export
ss <- function(..., d. = "|", trm. = T, sqz. = T, u. = F, n. = NULL) {
  vx. <- all(sapply(list(...), ichr))
  vd. <- cmp_chr_vec(d.)
  vt. <- isTF(trm.)
  vs. <- isTF(sqz.)
  vu. <- isTF(u.)
  vn. <- f0(inll(n.), T, cmp_psw_vec(n.))
  err. <- NULL
  if (!vx.) {err. <- c(err., "\n • [...] must contain at least one argument, all of which must be character generics (?chr_gen).")}
  if (!vd.) {err. <- c(err., "\n • [d.] must be a complete character vec (?cmp_chr_vec).")}
  if (!vt.) {err. <- c(err., "\n • [trm.] must be TRUE or FALSE.")}
  if (!vs.) {err. <- c(err., "\n • [sqz.] must be TRUE or FALSE.")}
  if (!vu.) {err. <- c(err., "\n • [u.] must be TRUE or FALSE.")}
  if (!vn.) {err. <- c(err., "\n • [n.] must be NULL or a complete positive whole-number vec (?cmp_psw_vec).")}
  if (idef(err.)) {stop(err.)}
  x. <- av(...)
  for (dd. in d.) {x. <- av(strsplit(as.character(av(x.)), dd., fixed = T))}
  if (trm.) {x. <- trimws(x.)}
  if (sqz.) {x. <- x.[x. != ""]}
  if (!inll(n.)) {x. <- x.[n.]}
  if (u.) {x. <- unique(x.)}
  x.
}

#' @describeIn ss. Split strings using a pipe (\code{'|'}) delimiter.
#' @export
ss1 <- function(..., trm. = T, sqz. = T, n. = NULL, u. = F) {ss(..., d. = " ", trm. = trm., sqz. = sqz., n. = NULL, u. = F)}

#' @describeIn ss. Split strings using a pipe (\code{'|'}) delimiter.
#' @export
ssP <- function(..., trm. = T, sqz. = T, n. = NULL, u. = F) {ss(..., d. = "|", trm. = trm., sqz. = sqz., n. = NULL, u. = F)}

#' @describeIn ss. Split strings using a dot/period (\code{'.'}) delimiter.
#' @export
ssD <- function(..., trm. = T, sqz. = T, n. = NULL, u. = F) {ss(..., d. = ".", trm. = trm., sqz. = sqz., n. = NULL, u. = F)}

#' @describeIn ss. Split strings using a broken pipe (\code{'¦'}) delimiter.
#' @export
ssB <- function(..., trm. = T, sqz. = T, n. = NULL, u. = F) {ss(..., d. = "¦", trm. = trm., sqz. = sqz., n. = NULL, u. = F)}

#' @describeIn ss. Split a string using pipe and dot delimiters.
#' @export
ssPD <- function(..., trm. = T, sqz. = T, n. = NULL, u. = F) {ss(..., d. = c("|", "."), trm. = trm., sqz. = sqz., n. = NULL, u. = F)}

#' @describeIn ss. Split a string using pipe and broken pipe delimiters.
#' @export
ssPB <- function(..., trm. = T, sqz. = T, n. = NULL, u. = F) {ss(..., d. = c("|", "¦"), trm. = trm., sqz. = sqz., n. = NULL, u. = F)}

#' @describeIn ss. Split a string using dot and broken pipe delimiters.
#' @export
ssDB <- function(..., trm. = T, sqz. = T, n. = NULL, u. = F) {ss(..., d. = c(".", "¦"), trm. = trm., sqz. = sqz., n. = NULL, u. = F)}

#' @describeIn ss. Split a string using pipe, dot, and broken pipe delimiters.
#' @export
ssPDB <- function(..., trm. = T, sqz. = T, n. = NULL, u. = F) {ss(..., d. = c("|", ".", "¦"), trm. = trm., sqz. = sqz., n. = NULL, u. = F)}

#' @describeIn ss. Split a string into constituent characters.
#' @export
ch <- function(..., trm. = T, sqz. = T, n. = NULL, u. = F) {
  vd. <- all(sapply(list(...), ichr))
  vt. <- isTF(trm.)
  vs. <- isTF(sqz.)
  vu. <- isTF(u.)
  vn. <- f0(is.null(n.), T, cmp_psw_vec(n.))
  err. <- NULL
  if (!vd.) {err. <- c(err., "\n • [...] must contain at least one argument, all of which must be character generics (?chr_gen).")}
  if (!vt.) {err. <- c(err., "\n • [trm.] must be TRUE or FALSE.")}
  if (!vs.) {err. <- c(err., "\n • [sqz.] must be TRUE or FALSE.")}
  if (!vu.) {err. <- c(err., "\n • [u.] must be TRUE or FALSE.")}
  if (!vn.) {err. <- c(err., "\n • [n.] must be NULL or a complete positive whole-number vec (?cmp_psw_vec).")}
  if (idef(err.)) {stop(err.)}
  x. <- ss(av(...), d. = "", trm. = trm., u. = u.)
  if (sqz.) {x. <- x.[x. %in% c(letters, LETTERS, 0:9, " ")]}
  if (idef(n.)) {x. <- x.[n.]}
  x.
}

#' @describeIn ss. Split a string into unique constituent characters.
#' @export
uch <- function(..., trm. = T, sqz. = T, n. = NULL) {ch(..., trm. = trm., sqz. = sqz., n. = n., u. = T)}

#' @describeIn ss. Split strings and place results in a tibble, including
#'   original values
#' @export
sstb <- function(x., d., name. = "string", parts. = "part") {                    # BODY
  x. <- tibble::tibble(x.)                                                       # : convert to tibble
  y. <- t(sapply(x., ss, d. = d.))                                               # : transpose matrix after applying split string to each value of [x.]
  if (n1(parts.)) {colnames(y.) <- da0(parts., v(.), 1:nc(y.))}                  # : IF the same part label should be used, build the labels
  else {colnames(y.) <- parts.}                                                  # : ELSE name with the full specified vector
  colnames(x.) <- name.                                                          # : put [x.] in its own column tibble
  x. <- cb(x., y.)                                                               # : column bind the original and split versions
  rownames(x.) <- NULL                                                           # : remove row names
  x.                                                                             # : return value
}                                                                                # END
