#' @name ss
#' @family strings
#' @title Split strings and select/check for elements
#' @description \strong{\code{ss}} splits strings using the delimiter(s) in
#'   \code{d} following these sequential steps:\enumerate{
#'   \item Reduce \code{...} to an atomic vector containing the constituent
#'         atomic elements of each argument in \code{...}.
#'   \item Convert the result to mode character.
#'   \item Replace each element of the result with that element's constituent
#'         parts as delimited by \code{d.}, producing a potentially longer
#'         character vector.
#'   \item If \code{n} is not \code{NULL}, extracts the \code{n}-th
#'         elements(s) from the result.
#'   \item If \code{trm} is \code{TRUE}, trims white space (i.e., spaces,
#'         tabs, newlines) from both ends of each element of the result.
#'   \item If \code{sqz} is \code{TRUE}, removes leading and trailing white
#'         space and replaces any multi-character strings of white-space
#'         strings inside the result with a single space.
#'   \item If \code{u} is \code{TRUE}, reduces the result to unique values.    }
#'   \strong{\code{ch}} splits strings into their constituent characters
#'   following these sequential steps:\enumerate{
#'   \item Reduce \code{...} to an atomic vector containing the constituent
#'         atomic elements of each argument in \code{...}.
#'   \item Convert the result to mode character.
#'   \item Replace each element of the result with a character vector of that
#'         element's constituent characters, producing a potentially longer
#'         character vector.
#'   \item If \code{trm} is \code{TRUE}, trims white space (i.e., spaces,
#'         tabs, newlines) from both ends of each element of the result.
#'   \item If \code{sqz} is \code{TRUE}, removes leading and trailing white
#'         space and replaces any multi-character strings of white-space
#'         strings inside the result with a single spac\
#'   \item If \code{n} is not \code{NULL}, extracts the \code{n}-th
#'         elements(s) from the result.
#'   \item If \code{u} is \code{TRUE}, reduces the result to unique values.    }
#'   \strong{Extensions}
#'   \cr Functions are extended for specific delimiters, signified by the
#'   following suffixes on function names:\tabular{lll}{
#'   SUFFIX      \tab SUFFIX        \tab DELIMITER                           \cr
#'   CHARACTER   \tab NAME          \tab INVOKED                             \cr
#'   \code{'1'}  \tab Space         \tab \code{" "}                          \cr
#'   \code{'P'}  \tab Pipe          \tab \code{"|"}                          \cr
#'   \code{'D'}  \tab Dot           \tab \code{"."}                          \cr
#'   \code{'B'}  \tab Broken pipe   \tab \code{"¦"}                            }
#' @param ... An arbitrary number of objects to be \link[=av]{atomized} before
#'   splitting.
#' @param x \link[=chr_vec]{character vec} of string(s) to be split.
#' @param name \link[=cmp_scl_scl]{Complete character scalar} name of the
#'   variable to hold the original strings.
#' @param part \link[=cmp_chr_vec]{Complete character vec} with as many elements
#'   as each string will be split into for naming variables containing a part of
#'   each string.
#' @param d \link[=cmp_chr_vec]{Complete character vec} delimiter or delimiters
#'   to use in splitting strings.
#' @param trm \link[=cmp_lgl_scl]{Complete logical scalar} indicating whether to
#'   trim white space from each side of each element of the result.
#' @param sqz link[=cmp_lgl_scl]{Complete logical scalar} indicating whether to
#'   squeeze the result by removing either empty strings (for \code{ss}
#'   functions) or characters that are neither letters, digits, nor spaces (for
#'   \code{ch}).
#' @param n An optional \link[=cmp_psw_scl]{complete positive whole-number
#'   scalar} specifying an element to be extracted from the result.
#' @param u \link[=cmp_lgl_scl]{Complete logical scalar} indicating whether to
#'   reduce the result to unique values.
#' @return Either a \link[=chr_vec]{character vector}, a
#'   \link[=chr_vls]{character vlist}, or a \link[=chr_dtf]{character
#'   data.frame}.
#' @export
ss <- function(d, ..., trm = T, sqz = T, u = F, n = NULL) {
  errs <- c(f0(all(sapply(list(...), ichr))  , NULL, "\n \u2022 [...] must contain at least one argument, all of which must be character generics (?chr_gen)."),
            f0(cmp_chr_vec(d)                , NULL, "\n \u2022 [d] must be a complete character vec (?cmp_chr_vec)."),
            f0(isTF(trm)                     , NULL, "\n \u2022 [trm] must be TRUE or FALSE."),
            f0(isTF(sqz)                     , NULL, "\n \u2022 [sqz] must be TRUE or FALSE."),
            f0(isTF(u)                       , NULL, "\n \u2022 [u] must be TRUE or FALSE."),
            f0(f0(inll(n), T, cmp_psw_vec(n)), NULL, "\n \u2022 [n] must be NULL or a complete positive whole-number vec (?cmp_psw_vec)."))
  if (idef(errs)) {stop(errs)}
  x <- av(...)
  for (dd in d) {x <- av(strsplit(as.character(av(x)), dd, fixed = T))}
  if (trm) {x <- trimws(x)}
  if (sqz) {x <- x[x != ""]}
  if (!inll(n)) {x <- x[n]}
  if (u) {x <- unique(x)}
  x
}

#' @rdname ss
#' @export
ss1 <- function(..., trm = T, sqz = T, n = NULL, u = F) {ss(" ", ..., trm = trm, sqz = sqz, n = NULL, u = F)}

#' @rdname ss
#' @export
ssP <- function(..., trm = T, sqz = T, n = NULL, u = F) {ss("|", ..., trm = trm, sqz = sqz, n = NULL, u = F)}

#' @rdname ss
#' @export
ssD <- function(..., trm = T, sqz = T, n = NULL, u = F) {ss(".", ..., trm = trm, sqz = sqz, n = NULL, u = F)}

#' @rdname ss
#' @export
ssB <- function(..., trm = T, sqz = T, n = NULL, u = F) {ss("¦", ..., trm = trm, sqz = sqz, n = NULL, u = F)}

#' @rdname ss
#' @export
ssPD <- function(..., trm = T, sqz = T, n = NULL, u = F) {ss(c("|", "."), ..., trm = trm, sqz = sqz, n = NULL, u = F)}

#' @rdname ss
#' @export
ssPB <- function(..., trm = T, sqz = T, n = NULL, u = F) {ss(c("|", "¦"), ..., trm = trm, sqz = sqz, n = NULL, u = F)}

#' @rdname ss
#' @export
ssDB <- function(..., trm = T, sqz = T, n = NULL, u = F) {ss(c(".", "¦"), ..., trm = trm, sqz = sqz, n = NULL, u = F)}

#' @rdname ss
#' @export
ssPDB <- function(..., trm = T, sqz = T, n = NULL, u = F) {ss(c("|", ".", "¦"), ..., trm = trm, sqz = sqz, n = NULL, u = F)}

#' @rdname ss
#' @export
ch <- function(..., trm = T, sqz = T, n = NULL, u = F) {
  errs <- c(f0(all(sapply(list(...), ichr))     , NULL, "\n \u2022 [...] must contain at least one argument, all of which must be character generics (?chr_gen)."),
            f0(isTF(trm)                        , NULL, "\n \u2022 [trm] must be TRUE or FALSE."),
            f0(isTF(sqz)                        , NULL, "\n \u2022 [sqz] must be TRUE or FALSE."),
            f0(isTF(u)                          , NULL, "\n \u2022 [u] must be TRUE or FALSE."),
            f0(f0(is.null(n), T, cmp_psw_vec(n)), NULL, "\n \u2022 [n] must be NULL or a complete positive whole-number vec (?cmp_psw_vec)."))
  if (idef(errs)) {stop(errs)}
  x <- ss("", av(...), trm = trm, u = u)
  if (sqz) {x <- x[x %in% c(letters, LETTERS, 0:9, " ")]}
  if (idef(n)) {x <- x[n]}
  x
}

#' @rdname ss
#' @export
uch <- function(..., trm = T, sqz = T, n = NULL) {ch(..., trm = trm, sqz = sqz, n = n, u = T)}

#' @rdname ss
#' @export
sstb <- function(x, d, name = "string", parts = "part") {
  ss0 <- function(xx, dd) {ss(dd, xx)}
  x <- tibble::tibble(x)
  y <- NULL
  for (xx in x) {}
  y <- t(sapply(x, ss0, dd = d))                                                 # : transpose matrix after applying split string to each value of [x.]
  if (n1(parts)) {colnames(y) <- da0(parts, v(dot), 1:nc(y))}                    # : IF the same part label should be used, build the labels
  else {colnames(y) <- parts}                                                    # : ELSE name with the full specified vector
  colnames(x) <- name                                                            # : put [x.] in its own column tibble
  x <- cb(x, y)                                                                  # : column bind the original and split versions
  rownames(x) <- NULL                                                            # : remove row names
  x                                                                              # : return value
}                                                                                # END
