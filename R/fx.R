#' @name fx
#' @family meta
#' @title Fork
#' @description Extended Functionality for \code{\link[base]{ifelse}}
#' @details \strong{\code{f0}}
#'   \cr If \code{test = TRUE}, returns \code{yes}. If \code{test} is anything
#'   else, returns \code{no}.
#'   \cr\cr
#'   \strong{\code{f1}}
#'   \cr If \code{test = TRUE}, returns \code{yes}. If \code{test = FALSE},
#'   returns \code{no}. If \code{test = NA} and \code{na = 'err'}, throws an
#'   error, otherwise returns \code{na}. If \code{test} is anything else and
#'   \code{err = 'err'}, throws an error, otherwise returns \code{err}.
#'   \cr\cr
#'   \strong{\code{fx}}
#'   \cr Evaluates \code{test} (which must be a logical scalar or vector
#'   containing only no \code{NA} values) and returns an object of the same
#'   dimension as \code{test} where \code{TRUE} values are exchanged for the
#'   associated value of \code{yes} and \code{FALSE} values are exchanged for
#'   the associated value of \code{no}. \code{yes} and \code{no} must be
#'   recyclable with \code{test}. \code{yes} and \code{no} must also be
#'   \link[=compatible]{compatible} with each other.
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
f0 <- function(test, yes, no) {if (isTRUE(test)) {yes} else {no}}

#' @rdname fx
#' @export
f1 <- function(test, yes, no, na = no, err = no) {
  Nas  <-  isNa(test); err.na  <- isID(na , 'err')
  Err  <- !isLG(test); err.err <- isID(err, 'err')
  test <- failsafe(test)
  f0(isTRUE(test) , yes, f0(isFALSE(test) , no ,
  f0(Nas & !err.na, na , f0(Err & !err.err, err,
  f0(Nas, stop("\n  * [test] must be atomic, scalar, and TRUE, FALSE, or NA."),
          stop("\n  * [test] must be atomic, scalar and TRUE or FALSE."  ))))))
}

#' @rdname fx
#' @export
fx <- function(test, yes, no, na = 'err') {
  NT <- length(test); NY <- length(yes); NN <- length(no)
  VT <- lgc_vec(test)
  VY <- f0(!xvec(yes), F, f0(!VT, T, NY %in% c(1, max(1, NT))))
  VN <- f0(!xvec(no ), F, f0(!VT, T, NN %in% c(1, max(1, NT))))
  VA <- xscl(na)
  ER <- isID(na, 'err')
  MS <- isNa(na)
  IA <- ER | MS
  VV <- VT & VY & VN
  VX <- f0(!ER | !VT, T, cmp_lgc_vec(test))
  VC <- f0(!VV, T, f0(IA, compatible(yes, no), compatible(yes, no, na)))
  E <- NULL
  E <- c(f0(VT, NULL, "\n  * [test] must be a logical scalar or vector containing no NA values."),
         f0(VY, NULL, "\n  * [yes] must be of length 1 or the same length as [test]."),
         f0(VN, NULL, "\n  * [no] must be of length 1 or the same length as [test]."),
         f0(VA, NULL, "\n  * [na] must an atomic scalar."),
         f0(VX, NULL, "\n  * [na = 'err'] but [test] contains NA values."),
         f0(VC, NULL,
         f0(IA, "\n  * [yes], [no], and [na] must be of compatible modes.",
                "\n  * [yes] and [no] must be of compatible modes.")))
  if (xdef(E)) {stop(E)}
  if (NY == 1) {yes <- rep.int(yes, NT)}
  if (NN == 1) {no  <- rep.int(no , NT)}
  R <- rep.int(na, NT)
  R[ isTRUE(test)] <- yes[isTRUE(test)]
  R[isFALSE(test)] <- no[isFALSE(test)]
  R
}

