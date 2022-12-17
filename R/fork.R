#' @name fork
#' @family forks
#' @title Robust extended functionality for \code{\link[base]{ifelse}}.
#' @description \tabular{rl}{
#'     `fork`   \tab Evaluates logical scalar or logical vector `test` and return an object of the same length as `test` where:
#'   \cr        \tab \itemize{
#'                     \item `TRUE` values of `test` are replaced by corresponding values of `yes`.
#'                     \item `FALSE` values of `test` are replaced by corresponding values of `no`.
#'                     \item `NA` values of `test` are replaced by `na` (unless `na = 'err'`, in which case if there are any `NA` values in `test`, throws an error).
#'                   }
#'   \cr `f0`   \tab If `test` is scalar `TRUE`, returns `yes`. If `test` is anything else, returns `no`.
#'   \cr `f1`   \tab Error-checked version of `f0`. Evaluates and processes logical scalar `test` in the following manner:
#'   \cr        \tab \itemize{
#'                     \item If `test = TRUE`, returns `yes`.
#'                     \item If `test = FALSE`, returns `no`.
#'                     \item If `test = NA`, returns `na` unless `na = 'err'`, in which case, an error is thrown.
#'                     \item If `test` is neither a logical scalar nor scalar `NA`, returns `err` unless `err = 'err'`, in which case an error is thrown.
#'                   }
#' }
#' @param x A logical scalar or vector is anticipated for `f0` and `f1` vs. `fork` respectively, but this argument may be any R object.
#' @param y,n Objects of any type for `f0` and `f1`. \link[=atm_scl]{Atomic scalars} or \link[=atm_vec]{atomic vecs} of the same length as `test` for `fork`.
#' @param na An object of any type for `f1`. An atomic scalar \link[=compatible]{compatible} with `yes` and `no` for `fork`, with the additional possibility of `na = 'err'` to indicate an error should be thrown if any values in `test` are `na`.
#' @param err Either `'err'` or an object to be returned when `test` is not an atomic scalar in `c(TRUE, FALSE, NA)`.
#' @return \tabular{rl}{
#'     `fork` \tab   A length-`length(x)` atomic object.
#'   \cr `f0` \tab   An R object.
#'   \cr `f1` \tab   An R object.
#' }
#' @examples
#' Vec. <- sample(c(TRUE, FALSE, NA), 10, replace = TRUE)
#' Yes. <- list(a = "yes", b = "yes")
#' No. <- data.frame(a = "no", b = "no")
#'
#' Vec.
#' Yes.
#' No.
#'
#' fork(Vec., "y", "n", na = "NA")
#' fork(Vec., "y", "n")
#'
#' f0(FALSE, Yes., No.)
#' f0(TRUE, Yes., No.)
#' f0(Vec., Yes., No.)
#' f0(No., Yes., Vec.)
#' f0(NA, Yes., No.)
#'
#' f1(FALSE, Yes., No.)
#' f1(TRUE, Yes., No.)
#'
#' f1(NA, Yes., No., na = Vec.)
#' f1(NA, Yes., No.)
#'
#' f1(7, Yes., No., err = Vec.)
#' f1(7, Yes., No.)
#' @export
f0 <- function(x, y, n) {if (isTRUE(x)) {y} else {n}}

#' @rdname fork
#' @export
fork <- function(x, y, n, na = n) {
  nx <- length(x)
  ny <- length(y)
  nn <- length(n)
  nna <- length(na)
  na.err <- isID(na, 'err')
  inc.na <- na.err | isNAS(na)
  ok.x <- lgl_vec(x)
  ok.y <- f0(!ivec(y), F, f0(!ok.x, T, ny %in% c(1, max(1, nx))))
  ok.n <- f0(!ivec(n), F, f0(!ok.x, T, nn %in% c(1, max(1, nx))))
  ok.na <- f0(!ivec(na), F, f0(!ok.x, T, nna %in% c(1, max(1, nx))))
  errs <- c(f0(ok.x , NULL, "[x] must be a complete logical vec (?cmp_lgl_vec)."),
            f0(ok.y , NULL, "[y] must be of length 1 or a vector of the same length as [x]."),
            f0(ok.n , NULL, "[n] must be of length 1 or a vector of the same length as [x]."),
            f0(ok.na, NULL, "[na] must be of length 1 or a vector of the same length as [x]."))
  if (!is.null(errs)) {stop(.errs(errs))}
  ok.tny <- ok.x & ok.y & ok.n
  ok.arg <- f0(!na.err | !ok.x, T, cmp_lgl_vec(x))
  ok.tny <- f0(!ok.tny, NULL, f0(inc.na, compatible(y, n, na), compatible(y, n)))
  errs  <- c(f0(ok.arg, NULL,            "[na = 'err'] but [x] contains NA values."),
             f0(ok.tny, NULL, f0(inc.na, "[y], [n], and [na] must be of compatible (?compatible) modes.",
                                         "[y] and [n] must be of compatible (?compatible) modes.")))
  if (!is.null(errs)) {stop(.errs(errs))}
  if (ny  == 1) {y  <- rep.int(y , nx)}
  if (nn  == 1) {n  <- rep.int(n , nx)}
  if (nna == 1) {na <- rep.int(na, nx)}
  out <- rep.int(NA, nx)
  iT <- sapply(x, isTRUE)
  iF <- sapply(x, isFALSE)
  iN <- is.na(x)
  out[iT] <-  y[iT]
  out[iF] <-  n[iF]
  out[iN] <- na[iN]
  out
}

#' @rdname fork
#' @export
f1 <- function(x, y, n, na = n, err = n) {
  nas <- isNAS(x)
  x <- failsafe(x)
  f0(isTRUE(x), y,
     f0(isFALSE(x), n,
        f0(nas & !isID(na, 'err'), na,
           f0(!isLG(x) & !isID(err, 'err'), err,
              f0(nas, stop(.errs("[x] must be atomic, scalar, and TRUE, FALSE, or NA.")),
                      stop(.errs("[x] must be atomic, scalar, and TRUE or FALSE." )))))))
}


