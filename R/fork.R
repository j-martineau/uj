#' @name fork.
#' @family extensions
#' @family fork
#' @title Forking as an extension of \code{\link[base]{ifelse}}
#' @description Robust extended functionality for \code{\link[base]{ifelse}}.
#' @section The \code{test} argument:
#'   \code{f0} and \code{ff0} expect \code{test} to be a non-\code{NA} logical
#    scalar. If that is not the case, \code{f0} will return the argument
#'   \code{no}, whereas \code{ff0}'s return value will be unpredictable.
#'   \cr\cr
#'   \code{f1} and \code{ff1} by default also expect \code{test} to be a
#'   non-\code{NA} logical scalar. However, \code{f1} allows for specifying
#'   replacement values for \code{NA} values in \code{test}. \code{ff1}'s return
#'   value is unpredictable when \code{test = NA}.
#'   \cr\cr
#'   \code{fx} and \code{ffx} expect a logical vector without any \code{NA}
#'   values.  However, \code{fx} allows for specifying either throwing an error
#'   if \code{test} contains any \code{NA} values or a return value to replace
#'   \code{NA} values. \code{ffx}'s return value is unpredictable when
#'   \code{test} contains \code{NA} values.
#' @param test \link[cmp_lgl_scl]{Complete logical scalar} for \code{f0},
#'   \code{ff0}, \code{f1}, and \code{ff1}. \link[cmp_lgl_vec]{Complete logical
#'   vec} for \code{fx} and \code{ffx}. \code{f0}, \code{f1}, and \code{f2} can
#'   manage \code{NA} values in \code{test}. The others do not.
#' @param yes An object of any type for \code{f0}, \code{ff0}, \code{f1}, and
#'   \code{ff1}. An \link[atm_scl]{atomic scalar} or an \link[atm_vec]{atomic
#'   vec} of the same length as \code{test} for \code{fx} and \code{ffx}.
#' @param no An object of any type for \code{f0}, \code{ff0}, \code{f1}, and
#'   \code{ff1}. An \link[atm_scl]{atomic scalar} or an \link[atm_vec]{atomic
#'   vec} of the same length as \code{test} for \code{fx} and \code{ffx}. Must
#'   be \code{\link{compatible}} with \code{yes}.
#' @param na An object of any type for \code{f1}. An \link[atm_scl]{atomic
#'   scalar} \code{\link{compatible}} with \code{yes} and \code{no} for
#'   \code{fx} and \code{ffx}, with the additional possibility of \code{na =
#'   'err'} for \code{fx} to indicate an error should be thrown if any values in
#'   \code{test} are \code{NA}.
#' @param err \code{'err'} or an object to be returned when \code{test} is not
#'   an atomic scalar in \code{c(TRUE, FALSE, NA)}.
#' @return An object.
#' @export
fork. <- function() {help("fork.", package = "uj")}

#' @describeIn fork. If \code{test = TRUE}, returns \code{yes}. If \code{test}
#'   is anything else, returns \code{no}.
#' @export
f0 <- function(test, yes, no) {if (isTRUE(test)) {yes} else {no}}

#' @describeIn fork. If \code{test = TRUE}, returns \code{yes}. If
#'   \code{test = FALSE}, returns \code{no}. If \code{test = NA} and
#'   \code{na = 'err'}, throws an error, otherwise returns \code{na}. If
#'   \code{test} is anything else and \code{err = 'err'}, throws an error,
#'   otherwise returns \code{err}.
#' @export
f1 <- function(test, yes, no, na = no, err = no) {
  nas <- isNa(test)
  test <- failsafe(test)
  f0(isTRUE(test), yes,
     f0(isFALSE(test), no ,
        f0(nas & !isID(na , 'err'), na ,
           f0(!isLG(test) & !isID(err, 'err'), err,
              f0(nas, stop("\n \u2022 [test] must be atomic, scalar, and TRUE, FALSE, or NA."),
                      stop("\n \u2022 [test] must be atomic, scalar and TRUE or FALSE."  ))))))
}

#' @describeIn fork. Evaluates \code{test} (which must be a logical scalar or
#'   vector) and returns an object of the same dimension as \code{test} where
#'   \code{TRUE} values are exchanged for the corresponding value of \code{yes},
#    \code{FALSE} values are exchanged for corresponding values of \code{no},
#'   and \code{NA} values are exchanged for \code{na} (unless \code{na = 'err'},
#'   in which case if there are \code{NA} values in \code{test}, an error is
#'   thrown).
#' @export
fx <- function(test, yes, no, na = 'err') {
  nt <- length(test)
  ny <- length(yes)
  nn <- length(no)
  ok.test <- lgl_vec(test)
  ok.yes <- f0(!ivec(yes), F, f0(!ok.test, T, ny %in% c(1, max(1, nt))))
  ok.no <- f0(!ivec(no), F, f0(!ok.test, T, nn %in% c(1, max(1, nt))))
  na.err <- isID(na, 'err')
  inc.na <- na.err | isNa(na)
  errs <- c(f0(ok.test , NULL, "\n \u2022 [test] must be a complete logical scalar/vec (?cmp_lgl_scl, ?cmp_lgl_vec)."),
            f0(ok.yes  , NULL, "\n \u2022 [yes] must be of length 1 or a vector of the same length as [test]."),
            f0(ok.no   , NULL, "\n \u2022 [no] must be of length 1 or a vector of the same length as [test]."),
            f0(iscl(na), NULL, "\n \u2022 [na] must be an atomic scalar."),
            f0(f0(!na.err | !ok.test, T, cmp_lgl_vec(test)) , NULL, "\n \u2022 [na = 'err'] but [test] contains NA values."),
            f0(f0(!ok.test & ok.yes & ok.no, T, f0(inc.na, compatible(yes, no), compatible(yes, no, na))), NULL, f0(inc.na, "\n \u2022 [yes], [no], and [na] must be of compatible (?compatible) modes.",
                                                                                                                            "\n \u2022 [yes] and [no] must be of compatible (?compatible) modes.")))
  if (idef(errs)) {stop(errs)}
  if (ny == 1) {yes <- rep.int(yes, nt)}
  if (nn == 1) {no  <- rep.int(no , nt)}
  out <- rep.int(na, nt)
  out[ isTRUE(test)] <- yes[isTRUE(test)]
  out[isFALSE(test)] <- no[isFALSE(test)]
  out[  is.na(test)]
  out
}

#' @describeIn fork. Bare-bones version of \code{f0} without error-checking.
#'   If \code{test} is logical scalar \code{TRUE}, return \code{yes} (all of
#'   it), otherwise return \code{no} (all of it).
#' @export
ff0 <- function(test, yes, no) {if (isTRUE(test)) {yes} else {no}}

#' @describeIn fork. Bare-bones version of \code{f1} without error checking.
#'   Creates a new vector the same length as \code{test}, placing the atomic
#'   scalar value \code{yes} into the elements of the new vector corresponding
#'   to the \code{TRUE} elements of \code{test} and placing the atomic scalar
#'   value in \code{no} into elements of the new vector corresponding to
#'   \code{FALSE} elements of \code{test}.
#' @export
ff1 <- function(test, yes, no) {out <- rep(NA, length(test)); out[test] <- yes; out[!test] <- no; out}

#' @describeIn fork. Bare-bones version of \code{fx} without error checking.
#'   Creates a new vector the same length as \code{test}, placing values of
#'   atomic vector \code{yes} corresponding to \code{TRUE} elements of
#'   \code{test} into those same elements of the new vector and placing values
#'   of atomic vector \code{no} corresponding to \code{FALSE} elements of
#'   \code{test} into those same elements of the new vector.
#' @export
ffx <- function(test, yes, no) {out <- rep(NA, length(test)); out[test] <- yes[test]; out[!test] <- no[!test]; out}
