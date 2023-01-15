#' @name fork
#' @encoding UTF-8
#' @family extensions
#' @family forks
#' @title Robust extended functionality for \code{\link[base]{ifelse}}.
#' @description \tabular{rl}{
#'           `f0`   \tab If `test` is scalar `TRUE`, returns `yes`. If `test` is anything else, returns `no`.
#'   \cr            \tab  
#'   \cr     `f1`   \tab Error-checked version of `f0`. Evaluates and processes logical scalar `test` in the following manner:
#'   \cr            \tab \itemize{
#'                         \item If `test = TRUE`, returns `yes`.
#'                         \item If `test = FALSE`, returns `no`.
#'                         \item If `test = NA`, returns `na` unless `na = 'err'`, in which case, an error is thrown.
#'                         \item If `test` is neither a logical scalar nor scalar `NA`, returns `err` unless `err = 'err'`, in which case an error is thrown.
#'                       }
#'   \cr   `fork`   \tab Evaluates logical scalar or logical vector `test` and return an object of the same length as `test` where:
#'   \cr            \tab \itemize{
#'                         \item `TRUE` values of `test` are replaced by corresponding values of `yes`.
#'                         \item `FALSE` values of `test` are replaced by corresponding values of `no`.
#'                         \item `NA` values of `test` are replaced by `na` (unless `na = 'err'`, in which case if there are any `NA` values in `test`, throws an error).
#' }}
#' @param x A logical scalar or vector is anticipated for `f0` and `f1` vs. `fork` respectively, but this argument may be any R object.
#' @param y,n Objects of any type for `f0` and `f1`. \link[=atm_scl]{Atomic scalars} or \link[=atm_vec]{atomic vecs} of the same length as `test` for `fork`.
#' @param na An object of any type for `f1`. An atomic scalar \link[=compatible]{compatible} with `yes` and `no` for `fork`, with the additional possibility of `na = 'err'` to indicate an error should be thrown if any values in `test` are `na`.
#' @param err Either `'err'` or an object to be returned when `test` is not an atomic scalar in `c(TRUE, FALSE, NA)`.
#' @return *A length-`length(x)` atomic object* \cr   `fork`
#'  \cr\cr *An* R *object* \cr   `f0, f1`
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
f0 <- function(x, y, n) {if (base::isTRUE(x)) {y} else {n}}

#' @rdname fork
#' @export
fork <- function(x, y, n, na = n) {
  nx <- base::length(x)
  ny <- base::length(y)
  nn <- base::length(n)
  nna <- base::length(na)
  na.err <- uj::isID(na, 'err')
  inc.na <- na.err | uj::isNAS(na)
  ok.x <- uj::lgl_vec(x)
  ok.y <- uj::f0(!uj::ivec(y), F, uj::f0(!ok.x, T, ny %in% base::c(1, base::max(1, nx))))
  ok.n <- uj::f0(!uj::ivec(n), F, uj::f0(!ok.x, T, nn %in% base::c(1, base::max(1, nx))))
  ok.na <- uj::f0(!uj::ivec(na), F, uj::f0(!ok.x, T, nna %in% base::c(1, base::max(1, nx))))
  errs <- base::c(uj::f0(ok.x , NULL, "[x] must be a complete logical vec (?cmp_lgl_vec)."),
                  uj::f0(ok.y , NULL, "[y] must be of length 1 or a vector of the same length as [x]."),
                  uj::f0(ok.n , NULL, "[n] must be of length 1 or a vector of the same length as [x]."),
                  uj::f0(ok.na, NULL, "[na] must be of length 1 or a vector of the same length as [x]."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  ok.tny <- ok.x & ok.y & ok.n
  ok.arg <- uj::f0(!na.err | !ok.x, T, uj::cmp_lgl_vec(x))
  ok.tny <- uj::f0(!ok.tny, NULL, uj::f0(inc.na, uj::compatible(y, n, na), uj::compatible(y, n)))
  errs  <- base::c(uj::f0(ok.arg, NULL, "[na = 'err'] but [x] contains NA values."),
                   uj::f0(ok.tny, NULL, uj::f0(inc.na, "[y], [n], and [na] must be of compatible (?compatible) modes.",
                                                       "[y] and [n] must be of compatible (?compatible) modes.")))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  if (ny  == 1) {y  <- base::rep.int(y , nx)}
  if (nn  == 1) {n  <- base::rep.int(n , nx)}
  if (nna == 1) {na <- base::rep.int(na, nx)}
  out <- base::rep.int(NA, nx)
  iT <- base::sapply(x, isTRUE)
  iF <- base::sapply(x, isFALSE)
  iN <- base::is.na(x)
  out[iT] <-  y[iT]
  out[iF] <-  n[iF]
  out[iN] <- na[iN]
  out
}

#' @rdname fork
#' @export
f1 <- function(x, y, n, na = n, err = n) {
  nas <- uj::isNAS(x)
  x <- uj::failsafe(x)
  uj::f0(base::isTRUE(x), y,
  uj::f0(base::isFALSE(x), n,
  uj::f0(nas & !uj::isID(na, 'err'), na,
  uj::f0(!uj::isLG(x) & !uj::isID(err, 'err'), err,
  uj::f0(nas, stop(uj::format_errs(pkg = "uj", "[x] must be atomic, scalar, and TRUE, FALSE, or NA.")),
              stop(uj::format_errs(pkg = "uj", "[x] must be atomic, scalar, and TRUE or FALSE." )))))))
}
