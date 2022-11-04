#' @name fx_uj
#' @family meta
#' @family fork
#' @title Robust Fork
#' @description Robust extended functionality for \code{\link[base]{ifelse}}.
#' @param test For \code{f0(.)}, \code{test = TRUE} or \code{test = FALSE} is
#'   expected, but any other value of \code{test} will return \code{no}. For
#'   \code{f1(.)}, \code{test = TRUE} or \code{test = FALSE} is also expected,
#'   but allows for specification of how \code{test = NA} should be handled and
#'   separate specification of how any other value of \code{test} should be
#'   handled. For \code{fx(.)}, expects \code{test} to be a logical scalar or
#'   vector containing no \code{NA} values.
#' @param yes For \code{f0(.)} and \code{f1(.)}, an object to be returned when
#'   \code{test = TRUE}. For \code{fx(.)}, an atomic vector of length 1 or of
#'   the same length as \code{x}.
#' @param no For \code{f0(.)} and \code{f1(.)}, an object to be returned when
#'   \code{test = TRUE}. For \code{fx(.)}, an atomic vector of length 1 or of
#'   the same length as \code{x} \link[=compatible]{compatible} with \code{yes}.
#' @param na For \code{f1(.)}, \code{'err'} or an object to be returned when
#'   \code{test = NA}. For \code{fx(.)}, \code{'err'} or an atomic scalar
#'   \link[=compatible]{compatible} with \code{yes} and \code{no}..
#' @param err \code{'err'} or an object to be returned when \code{test} is not
#'   an atomic scalar in \code{c(TRUE, FALSE, NA)}.
#' @return An object.
#' @export
fx_uj <- function() {help("fx_uj", package = "uj")}

#' @describeIn fx_uj If \code{test = TRUE}, returns \code{yes}. If \code{test}
#'   is anything else, returns \code{no}.
f0 <- function(test, yes, no) {if (isTRUE(test)) {yes} else {no}}

#' @describeIn fx_uj  If \code{test = TRUE}, returns \code{yes}. If \code{test =
#'   FALSE}, returns \code{no}. If \code{test = NA} and \code{na = 'err'},
#'   throws an error, otherwise returns \code{na}. If \code{test} is anything
#'   else and \code{err = 'err'}, throws an error, otherwise returns \code{err}.
#' @export
f1 <- function(test, yes, no, na = no, err = no) {
  nas  <-  isNa(test); err.na  <- isID(na , 'err')
  err  <- !isLG(test); err.err <- isID(err, 'err')
  test <- failsafe(test)
  f0(isTRUE(test) , yes, f0(isFALSE(test) , no ,
  f0(nas & !err.na, na , f0(err & !err.err, err,
  f0(nas, stop("\n • [test] must be atomic, scalar, and TRUE, FALSE, or NA."),
          stop("\n • [test] must be atomic, scalar and TRUE or FALSE."  ))))))
}

#' @describeIn fx_uj Evaluates \code{test} (which must be a logical scalar or
#'   vector containing no \code{NA} values) and returns an object of the same
#'   dimension as \code{test} where \code{TRUE} values are exchanged for the
#'   associated value of \code{yes} and \code{FALSE} values are exchanged for
#'   the associated value of \code{no}. \code{yes} and \code{no} must be
#'   recyclable with \code{test}. \code{yes} and \code{no} must also be
#'   \link[=compatible]{compatible} with each other.
#' @export
fx <- function(test, yes, no, na = 'err') {
  nt <- length(test); ny <- length(yes); nn <- length(no)
  vt <- lgl_vec(test)
  vy <- f0(!ivec(yes), F, f0(!vt, T, ny %in% c(1, max(1, nt))))
  vn <- f0(!ivec(no ), F, f0(!vt, T, nn %in% c(1, max(1, nt))))
  va <- iscl(na)
  er <- isID(na, 'err')
  ms <- isNa(na)
  ia <- er | ms
  vv <- vt & vy & vn
  vx <- f0(!er | !vt, T, cmp_lgl_vec(test))
  vc <- f0(!vv, T, f0(ia, compatible(yes, no), compatible(yes, no, na)))
  err <- NULL
  err <- c(f0(vt, NULL, "\n • [test] must be a logical scalar or vector containing no NA values."),
         f0(vy, NULL, "\n • [yes] must be of length 1 or the same length as [test]."),
         f0(vn, NULL, "\n • [no] must be of length 1 or the same length as [test]."),
         f0(va, NULL, "\n • [na] must an atomic scalar."),
         f0(vx, NULL, "\n • [na = 'err'] but [test] contains NA values."),
         f0(vc, NULL, f0(ia, "\n • [yes], [no], and [na] must be of compatible modes.",
                             "\n • [yes] and [no] must be of compatible modes.")))
  if (idef(err)) {stop(err)}
  if (ny == 1) {yes <- rep.int(yes, nt)}
  if (nn == 1) {no  <- rep.int(no , nt)}
  out <- rep.int(na, nt)
  out[ isTRUE(test)] <- yes[isTRUE(test)]
  out[isFALSE(test)] <- no[isFALSE(test)]
  out
}

#' @name fork_uj.
#' @family meta
#' @family fork
#' @title Bare Bones Fork
#' @description Bare bones extended functionality for
#'   \code{\link[base]{ifelse}}. Functions wrapping \code{\link[base]{ifelse}},
#'   expecting (but not checking for) characteristics of \code{test},
#'   \code{yes}, and \code{no}.
#' @param test A logical scalar (\code{f0.}) or a logical vector (\code{f1.} and
#'   \code{ff.}).
#' @param yes,no Any object (\code{f0.}), an atomic scalar \code{f1.}, or an
#'   atomic vector of the same length as \code{test}.
#' @return An arbitrary type of object (\code{f0.}) or an atomic vector
#'   (\code{f1.} and \code{ff.}).
#' @export
fork_uj. <- function() {help("fork_uj.", package = "uj")}

#' @describeIn fork_uj. If \code{test} is logical scalar \code{TRUE}, return
#'   \code{yes} (all of it), otherwise return \code{no} (all of it).
#' @export
f0. <- function(test, yes, no) {if (isTRUE(test)) {yes} else {no}}

#' @describeIn fork_uj. Create a new vector the same length as \code{test},
#'   placing the atomic scalar value \code{yes} into the elements of the new
#'   vector corresponding to the \code{TRUE} elements of \code{test} and placing
#'   the atomic scalar value in \code{no} into elements of the new vector
#'   corresponding to \code{FALSE} elements of \code{test}.
#' @export
f1. <- function(test, yes, no) {out <- rep(NA, length(test)); out[test] <- yes; out[!test] <- no; out}

#' @describeIn fork_uj. Create a new vector the same length as \code{test},
#'   placing values of atomic vector \code{yes} corresponding to \code{TRUE}
#'   elements of \code{test} into those same elements of the new vector and
#'   placing values of atomic vector \code{no} corresponding to \code{FALSE}
#'   elements of \code{test} into those same elements of the new vector.
#' @export
ff. <- function(test, yes, no) {out <- rep(NA, length(test)); out[test] <- yes[test]; out[!test] <- no[!test]; out}
