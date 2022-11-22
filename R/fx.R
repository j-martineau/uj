#' @name fork
#' @title Forking as an Extension of \code{base::ifelse}
#' @family extensions
#' @family forking
#' @description Robust extended functionality for \code{\link[base]{ifelse}}.
#' @section Functions in this Family:
#'   \strong{\code{fork}}
#'   \cr Evaluates logical scalar or logical vector \code{test} and returns an
#'   object of the same length as \code{test} where:\itemize{
#'     \item \code{TRUE} values of \code{test} are replaced by corresponding
#'           values of \code{yes}.
#'     \item \code{FALSE} values of \code{test} are replaced by corresponding
#'           values of \code{no}.
#'     \item \code{NA} values of \code{test} are replaced by \code{na} (unless
#'           \code{na = 'err'}, in which case if there are any \code{NA} values
#'           in \code{test}, throws an error).                                 }
#'   \cr\cr
#'   \strong{\code{f0}}
#'   \cr If \code{test} is scalar \code{TRUE}, returns \code{yes}. If
#'   \code{test} is anything else, returns \code{no}.
#'   \cr\cr
#'   \strong{\code{f1}}
#'   \cr Error-checked version of \code{f0}. Evaluates and processes logical
#'   scalar \code{test} in the following manner:\itemize{
#'     \item If \code{test = TRUE}, returns \code{yes}.
#'     \item If \code{test = FALSE}, returns \code{no}.
#'     \item If \code{test = NA}, returns \code{na} unless \code{na = 'err'}, in
#'           which case, an error is thrown.
#'     \item If \code{test} is neither a logical scalar nor scalar \code{na},
#'           returns \code{err} unless \code{err = 'err'}, in which case an
#'           error is thrown.                                                  }
#' @param test A logical scalar or vector is anticipated, but this argument may
#'   be any R object.
#' @param yes,no Objects of any type for \code{f0} and \code{f1}.
#'   \link[=atm_scl]{Atomic scalars} or \link[=atm_vec]{atomic vecs} of the same
#'   length as \code{test} for \code{fork}.
#' @param na An object of any type for \code{f1}. An \link[=atm_scl]{atomic
#'   scalar} \code{\link{compatible}} with \code{yes} and \code{no} for
#'   \code{fork}, with the additional possibility of \code{na = 'err'} to
#'   indicate an error should be thrown if any values in
#'   \code{test} are \code{NA}.
#' @param err \code{'err'} or an object to be returned when \code{test} is not
#'   an atomic scalar in \code{c(TRUE, FALSE, NA)}.
#' @return An R object.
#' @export
fork <- function(test, yes, no, na = 'err') {
    nt <- length(test)
    ny <- length(yes)
    nn <- length(no)
    na.err <- isID(na, 'err')
    inc.na <- na.err | isNa(na)
    ok.test <- lgl_vec(test)
    ok.yes <- f0(!ivec(yes), F, f0(!ok.test, T, ny %in% c(1, max(1, nt))))
    ok.no <- f0(!ivec(no), F, f0(!ok.test, T, nn %in% c(1, max(1, nt))))
    errs <- c(f0(ok.test , NULL, "\n \u2022 [test] must be a complete logical scalar/vec (?cmp_lgl_scl, ?cmp_lgl_vec)."),
              f0(ok.yes  , NULL, "\n \u2022 [yes] must be of length 1 or a vector of the same length as [test]."),
              f0(ok.no   , NULL, "\n \u2022 [no] must be of length 1 or a vector of the same length as [test]."),
              f0(iscl(na), NULL, "\n \u2022 [na] must be an atomic scalar."))
    if (!is.null(errs)) {stop(errs)}
    ok.tny <- ok.test & ok.yes & ok.no
    ok.test <- f0(!na.err | !ok.test, T, cmp_lgl_vec(test))
    ok.comp <- f0(!ok.tny, NULL, compatible(yes, no, f0(inc.na, na, NULL)))
    errs <- c(f0(ok.test , NULL, "\n \u2022 [na = 'err'] but [test] contains NA values."),
              f0(ok.comp , NULL, f0(inc.na, "\n \u2022 [yes], [no], and [na] must be of compatible (?compatible) modes.",
                                            "\n \u2022 [yes] and [no] must be of compatible (?compatible) modes.")))
    if (!is.null(errs)) {stop(errs)}
    if (ny == 1) {yes <- rep.int(yes, nt)}
    if (nn == 1) {no  <- rep.int(no , nt)}
    out <- rep.int(na, nt)
    out[ isTRUE(test)] <- yes[isTRUE(test)]
    out[isFALSE(test)] <- no[isFALSE(test)]
    out[  is.na(test)]
    out
  }

#' @rdname fork
#' @export
f0 <- function(test, yes, no) {if (isTRUE(test)) {yes} else {no}}

#' @rdname fork
#' @export
f1 <- function(test, yes, no, na = no, err = no) {
  nas <- isNa(test)
  test <- failsafe(test)
  f0(isTRUE(test), yes,
     f0(isFALSE(test), no,
        f0(nas & !isID(na , 'err'), na,
           f0(!isLG(test) & !isID(err, 'err'), err,
              f0(nas, stop("\n \u2022 [test] must be atomic, scalar, and TRUE, FALSE, or NA."),
                      stop("\n \u2022 [test] must be atomic, scalar and TRUE or FALSE."  ))))))
}


