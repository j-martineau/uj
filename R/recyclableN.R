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
recyclableN <- function(..., N = NULL, MIN = NULL, MAX = NULL, TARG = NULL) {
  ns <- uj::av(...)
  ok.d <- uj::N1P(ns)
  ok.n <- uj::f0(uj::NLL(N), T, uj::cmp_psw_vec(N))
  ok.n. <- uj::f0(!ok.d, T, uj::cmp_psw_vec(ns))
  ok.m. <- uj::f0(uj::NLL(MIN), T, uj::cmp_psw_scl(MIN))
  ok.x. <- uj::f0(uj::NLL(MAX), T, uj::cmp_psw_scl(MAX))
  ok.t. <- uj::f0(uj::NLL(TARG), T, uj::cmp_psw_scl(TARG))
  uj::errs_if_nots(ok.d , "[...] is empty.",
                   ok.n , "[...] must atomize (?av) to a complete positive whole-number vec (?cmp_psw_vec).",
                   ok.n., "[N] must be NULL or a complete positive whole-number vec (?cmp_psw_vec)."        ,
                   ok.m., "[MIN] must be NULL or a complete positive whole-number scalar (?cmp_psw_scl)."   ,
                   ok.x., "[MAX] must be NULL or a complete positive whole-number scalar (?cmp_psw_scl)."   ,
                   ok.t., "[TARG] must be NULL or a complete positive whole-number scalar (?cmp_psw_scl)."  , PKG = "uj")
  if (uj::NLL(TARG)) {TARG <- base::max(ns)}
  reps <- TARG / ns
  if (!base::all(uj::rounded(reps))) {return(F)}
  if (uj::DEF(N)) {if (uj::notIN(TARG, N)) {return(F)}}
  if (uj::DEF(MIN)) {if (TARG < MIN) {return(F)}}
  if (uj::DEF(MAX)) {if (TARG > MAX) {return(F)}}
  T
}

#' @rdname recyclableN
#' @export
recyclable <- function(..., N = NULL, MIN = 1, MAX = NULL, TARG = NULL, ERR = FALSE) {
  x <- base::list(...)
  uj::errs_if_nots(uj::N1P(x)                         , "[...] is empty."                                   ,
                   base::all(base::sapply(x, atm_vec)), "Arguments in [...] must be atomic vecs (?atm_vec).",
                   uj::isTF1(ERR)                     , "[ERR] must be scalar TRUE or scalar FALSE."        ,  PKG = "uj")
  y <- uj::recyclableN(uj::NS(x), N = N, MIN = MIN, MAX = MAX, TARG = TARG)
  uj::err_if(ERR & !y, "[...] arguments are not recyclable.", PKG = "uj")
  y
}

#' @rdname recyclableN
#' @export
recycle <- function(..., N = NULL, MIN = 1, MAX = NULL, TARG = NULL) {
  uj::recyclable(..., N = N, MIN = MIN, MAX = MAX, TARG = TARG, ERR = T)
  ndot <- uj::ND()
  lens <- uj::NS(base::list(...))
  labs <- uj::DN()
  reps <- uj::f0(uj::NLL(TARG), base::max(lens), TARG) / lens
  for (i in 1:ndot) {if (reps[i] > 1) {uj::set_val(labs[i], base::rep(base::...elt(i), reps[i]))}}
}
