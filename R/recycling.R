#' @encoding UTF-8
#' @name recycling
#' @family environments
#' @family meta
#' @title Recycling and recyclability
#' @description Arguments are recyclable if all of their lengths are divisors of the argument with the greatest length subject to any settings in the optional arguments `ns.`, `min.`, `max.`, and/or `targ.`.
#' \cr\cr Functions in this family are:\tabular{rl}{
#'     `recyclable_n`   \tab Are lengths in `...` recyclable?.
#'   \cr `recyclable`   \tab Are `...` arguments are recyclable?
#'   \cr    `recycle`   \tab Recycles `...` arguments in the calling function.
#' }
#' @param ... For `recycle`, named arguments to be recycled in the environment of the calling function. For `recyclable_n`, one or more objects containing only positive whole number values (\link[=av]{atomized} before processing).
#' @param n. Either `NULL` or a complete positive whole-number vec giving the set of valid recycled argument lengths.
#' @param min. `NULL` or complete positive whole-number scalar giving the minimum valid recycled argument length.
#' @param max. `NULL` or complete positive whole-number scalar giving the maximum valid recycled argument length.
#' @param err. A non-`NA` logical scalar indicating whether to throw an error if the `...` arguments are not recyclable.
#' @param targ. A \link[=cmp_psw_scl]{complete positive whole-number scalar} giving the target length of recycled arguments. May be greater than `length(av(...))`.
#' @return *A logical scalar*
#'   \cr   `recyclable_n`
#'   \cr   `recyclable`
#'   \cr
#'   \cr `NULL`
#'   \cr   `recycle`
#' @examples
#' n1. <- 1
#' n2. <- 1:2
#' n3. <- 1:3
#' n4. <- 1:4
#' c1. <- paste0("'", letters[n1.], "'")
#' c2. <- paste0("'", letters[n2.], "'")
#' c3. <- paste0("'", letters[n3.], "'")
#' c4. <- paste0("'", letters[n4.], "'")
#'
#' rec. <- function(A, B, C) {
#'   asc <- function(n, v) {paste0(n, " = c(", paste0(v, collapse = ", "), ")")}
#'   recycle(A = A, B = B, C = C)
#'   cat(paste0(asc("A", A), "\n", asc("B", B), "\n", asc("C", C)))
#' }
#'
#' rec.(n4., c2., n1.)
#' rec.(c1., n2., c4.)
#' rec.(n3., c3., c1.)
#' rec.(n3., c3., n3.)
#' recyclable_n(1:3)
#' recyclable_n(2, 4, 8)
#' recyclable_n(2, 4, 8, n. = c(2, 4, 8, 16))
#' recyclable_n(2, 4, 8, min. = 4)
#' recyclable_n(2, 4, 8, max. = 4)
#' recyclable_n(2, 4, 8, targ. = 7)
#' recyclable_n(2, 4, 8, targ. = 8)
#' recyclable_n(2, 4, 8, targ. = 16)
#' recyclable(n1., c2., n3.)
#' recyclable(n4., c2., n1.)
#' recyclable(c1., n2., c4.)
#' recyclable(n3., c3., c1.)
#' recyclable(n3., c3., n3.)
#' @export
recyclable_n <- function(..., n. = NULL, min. = NULL, max. = NULL, targ. = NULL) {
  ns <- uj::av(...)
  ok.d <- base::length(ns) > 0
  ok.n <- uj::f0(base::is.null(n.), T, uj::cmp_psw_vec(n.))
  ok.n. <- uj::f0(!ok.d, T, uj::cmp_psw_vec(ns))
  ok.m. <- uj::f0(base::is.null(min.), T, uj::cmp_psw_scl(min.))
  ok.x. <- uj::f0(base::is.null(max.), T, uj::cmp_psw_scl(max.))
  ok.t. <- uj::f0(base::is.null(targ.), T, uj::cmp_psw_scl(targ.))
  errs <- base::c(uj::f0(ok.d , NULL, "[...] is empty."),
                  uj::f0(ok.n , NULL, "[...] must atomize (?av) to a complete positive whole-number vec (?cmp_psw_vec)."),
                  uj::f0(ok.n., NULL, "[n.] must be NULL or a complete positive whole-number vec (?cmp_psw_vec)."),
                  uj::f0(ok.m., NULL, "[min.] must be NULL or a complete positive whole-number scalar (?cmp_psw_scl)."),
                  uj::f0(ok.x., NULL, "[max.] must be NULL or a complete positive whole-number scalar (?cmp_psw_scl)."),
                  uj::f0(ok.t., NULL, "[targ.] must be NULL or a complete positive whole-number scalar (?cmp_psw_scl)."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  if (base::is.null(targ.)) {targ. <- base::max(ns)}
  reps <- targ. / ns
  if (!base::all(reps == base::round(reps))) {return(F)}
  if (!base::is.null(n.)) {if (!(targ. %in% n.)) {return(F)}}
  if (!base::is.null(min.)) {if (targ. < min.) {return(F)}}
  if (!base::is.null(max.)) {if (targ. > max.) {return(F)}}
  T
}

#' @rdname recycling
#' @export
recyclable <- function(..., n. = NULL, min. = 1, max. = NULL, targ. = NULL, err. = FALSE) {
  dots <- base::list(...)
  ok.0 <- base::length(dots) > 0
  ok.v <- base::all(base::sapply(dots, atm_vec))
  ok.e <- base::isTRUE(err.) | base::isFALSE(err.)
  errs <- base::c(uj::f0(ok.0, NULL, "[...] is empty."),
                  uj::f0(ok.v, NULL, "Arguments in [...] must be atomic vecs (?atm_vec)."),
                  uj::f0(ok.e, NULL, "[err.] must be scalar TRUE or scalar FALSE."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  out <- uj::recyclable_n(base::lengths(dots), n. = n., min. = min., max. = max., targ. = targ.)
  if (err. & !out) {stop(uj:::.errs("[...] arguments are not recyclable."))}
  out
}

#' @rdname recycling
#' @export
recycle <- function(..., n. = NULL, min. = 1, max. = NULL, targ. = NULL) {
  uj::recyclable(..., n. = n., min. = min., max. = max., targ. = targ., err. = T)
  ndot <- base::...length()
  lens <- base::lengths(base::list(...))
  labs <- base::...names()
  reps <- uj::f0(base::is.null(targ.), base::max(lens), targ.) / lens
  for (i in 1:ndot) {if (reps[i] > 1) {uj::vset(labs[i], base::rep(base::...elt(i), reps[i]))}}
}
