#' @encoding UTF-8
#' @family environments
#' @family meta
#' @title Recycling and recyclability
#' @description Check for and conduct recycling in the environment that calls functions from this family.
#' @details Arguments are recyclable if all of their lengths are divisors of the argument with the greatest length subject to any settings in the optional arguments `ns.`, `min.`, `max.`, and/or `targ.`.
#' \cr\cr Functions in this family are:
#' \tabular{ll}{  `recyclableN`   \tab Are lengths in `...` recyclable?.                 \cr   \tab   \cr
#'                `recyclable`    \tab Are `...` arguments are recyclable?               \cr   \tab   \cr
#'                `recycle`       \tab Recycles `...` arguments in the calling function.                }
#' @param ... For `recycle`, named arguments to be recycled in the environment of the calling function. For `recyclable_n`, one or more objects containing only positive whole number values (\link[=av]{atomized} before processing).
#' @param N Either `NULL` or a \link[=cmp_psw_vec]{complete positive whole-number vec} (?cmp_psw_vec) giving the set of valid recycled argument lengths.
#' @param MIN Either `NULL` or a \link[=cmp_psw_scl]{complete positive whole-number scalar} (?cmp_psw_scl) giving the minimum valid recycled argument length.
#' @param MAX Either `NULL` or a complete positive whole-number scalar giving the maximum valid recycled argument length.
#' @param ERR `TRUE` or `FALSE` indicating whether to throw an error if the `...` arguments are not recyclable.
#' @param TARG Either `NULL` or a complete positive whole-number scalar giving the target length of recycled arguments. May be greater than `length(av(...))`.
#' @return **A logical scalar**    \cr\cr   `recyclableN, recyclable`
#' \cr\cr  **The* `NULL` *object** \cr\cr   `recycle`
#' @examples
#' egN1. <- 1
#' egN2 <- 1:2
#' egN3 <- 1:3
#' egN4 <- 1:4
#' egC1 <- paste0("'", letters[egN1], "'")
#' egC2 <- paste0("'", letters[egN2], "'")
#' egC3 <- paste0("'", letters[egN3], "'")
#' egC4 <- paste0("'", letters[egN4], "'")
#'
#' egRecycle <- function(A, B, C) {
#'   asc <- function(n, v) {paste0(n, " = c(", paste0(v, collapse = ", "), ")")}
#'   recycle(A = A, B = B, C = C)
#'   cat(paste0(asc("A", A), "\n", asc("B", B), "\n", asc("C", C)))
#' }
#'
#' egRecycle(egN4, egC2, egN1)
#' egRecycle(egC1, egN2, egC4)
#' egRecycle(egN3, egC3, egC1)
#' egRecycle(egN3, egC3, egN3)
#' recyclableN(1:3)
#' recyclableN(2, 4, 8)
#' recyclableN(2, 4, 8, N = c(2, 4, 8, 16))
#' recyclableN(2, 4, 8, MIN = 4)
#' recyclableN(2, 4, 8, MAX = 4)
#' recyclableN(2, 4, 8, TARG = 7)
#' recyclableN(2, 4, 8, TARG = 8)
#' recyclableN(2, 4, 8, TARG = 16)
#' recyclable(egN1, egC2, egN3)
#' recyclable(egN4, egC2, egN1)
#' recyclable(egC1, egN2, egC4)
#' recyclable(egN3, egC3, egC1)
#' recyclable(egN3, egC3, egN3)
#' @export
recyclable_ns <- function(..., N = NULL, MIN = NULL, MAX = NULL, TARG = NULL) {
  ns <- uj::av(...)
  ok.dots <- base::length(ns) > 0
  ok.ns <- uj::f0(!ok.dots, T, uj:::.cmp_psw_vec(ns))
  ok.N <- uj::f0(base::is.null(N), T, uj:::.cmp_psw_vec(N))
  ok.MIN <- uj::f0(base::is.null(MIN), T, uj:::.cmp_psw_scl(MIN))
  ok.MAX <- uj::f0(base::is.null(MAX), T, uj:::.cmp_psw_scl(MAX))
  ok.TARG <- uj::f0(base::is.null(TARG), T, uj:::.cmp_psw_scl(TARG))
  errs <- NULL
  if (!ok.dots) {errs <- base::c(errs, "[...] is empty.")}
  if (!ok.ns) {errs <- base::c(errs, "[...] must atomize (?av) to a complete positive whole-number vec (?cmp_psw_vec).")}
  if (!ok.N) {errs <- base::c(errs, "[N] must be NULL or a complete positive whole-number vec (?cmp_psw_vec).")}
  if (!ok.MIN) {errs <- base::c(errs, "[MIN] must be NULL or a complete positive whole-number scalar (?cmp_psw_scl).")}
  if (!ok.MAX) {errs <- base::c(errs, "[MAX] must be NULL or a complete positive whole-number scalar (?cmp_psw_scl).")}
  if (!ok.TARG) {errs <- base::c(errs, "[TARG] must be NULL or a complete positive whole-number scalar (?cmp_psw_scl).")}
  if (!base::is.null(errs)) {uj::stopperr(errs, PKG = "uj")}
  if (base::is.null(TARG)) {TARG <- base::max(ns)}
  reps <- TARG / ns
  if (!base::all(reps == base::round(reps))) {return(F)}
  if (!base::is.null(N)) {if (!(TARG %in% N)) {return(F)}}
  if (!base::is.null(MIN)) {if (TARG < MIN) {return(F)}}
  if (!base::is.null(MAX)) {if (TARG > MAX) {return(F)}}
  T
}

#' @rdname recyclable_ns
#' @export
recyclable <- function(..., N = NULL, MIN = 1, MAX = NULL, TARG = NULL, ERR = FALSE) {
  x <- base::list(...)
  errs <- NULL
  if (base::length(x) == 0) {errs <- base::c(errs, "[...] is empty.")}
  if (!base::all(base::sapply(x, uj:::.atm_vec))) {errs <- base::c(errs, "Arguments in [...] must be atomic vecs (?atm_vec).")}
  if (!uj:::.cmp_chr_scl(ERR)) {errs <- base::c(errs, "[ERR] must be scalar TRUE or scalar FALSE.")}
  if (!base::is.null(errs)) {uj::stopperr(errs, PKG = "uj")}
  y <- uj::recyclable_ns(uj::ns(x), N = N, MIN = MIN, MAX = MAX, TARG = TARG)
  if (ERR & !y) {uj::stopperr("[...] arguments are not recyclable.", PKG = "uj")}
  y
}

#' @rdname recyclable_ns
#' @export
recycle <- function(..., N = NULL, MIN = 1, MAX = NULL, TARG = NULL) {
  uj::recyclable(..., N = N, MIN = MIN, MAX = MAX, TARG = TARG, ERR = T)
  ndot <- base::...length()
  lens <- base::lengths(base::list(...))
  labs <- base::...names()
  reps <- uj::f0(base::is.null(TARG), base::max(lens), TARG) / lens
  for (i in 1:ndot) {if (reps[i] > 1) {base::assign(labs[i], base::rep.int(base::...elt(i), reps[i]), envir = base::parent.frame())}}
}
