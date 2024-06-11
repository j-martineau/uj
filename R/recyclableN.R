#' @encoding UTF-8
#' @family environments
#' @family meta
#' @title Recycling and recyclability
#' @description Check for and conduct recycling in the environment that calls functions from this family.
#' \cr\cr Arguments are recyclable if all of their lengths are divisors of the argument with the greatest length subject to any settings in the optional arguments `.n`, `.min`, `.max`, and/or `.targ`.
#' @param ... For `recycle`, named arguments to be recycled in the environment of the calling function. For `recyclable_n`, one or more objects containing only positive whole number values (\link[=av]{atomized} before processing).
#' @param .n Either `NULL` or a \link[=cmp_psw_vec]{complete positive whole-number vec} (?cmp_psw_vec) giving the set of valid recycled argument lengths.
#' @param .min Either `NULL` or a \link[=cmp_psw_scl]{complete positive whole-number scalar} (?cmp_psw_scl) giving the minimum valid recycled argument length.
#' @param .max Either `NULL` or a complete positive whole-number scalar giving the maximum valid recycled argument length.
#' @param .err `TRUE` or `FALSE` indicating whether to throw an error if the `...` arguments are not recyclable.
#' @param .targ Either `NULL` or a complete positive whole-number scalar giving the target length of recycled arguments. May be greater than `length(av(...))`.
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
#' recyclableN(2, 4, 8, .n = c(2, 4, 8, 16))
#' recyclableN(2, 4, 8, .min = 4)
#' recyclableN(2, 4, 8, .max = 4)
#' recyclableN(2, 4, 8, .targ = 7)
#' recyclableN(2, 4, 8, .targ = 8)
#' recyclableN(2, 4, 8, .targ = 16)
#' recyclable(egN1, egC2, egN3)
#' recyclable(egN4, egC2, egN1)
#' recyclable(egC1, egN2, egC4)
#' recyclable(egN3, egC3, egC1)
#' recyclable(egN3, egC3, egN3)
#' @export
recycling_help <- function() {utils::help("recycling", package = "uj")}

#' @describeIn recycling_help Checks whether numeric scalar arguments in `...` indicate recyclable lengths, subject to restrictions in `.n`, `.min`, `.max`, and `.targ`.
#' @export
recyclable_ns <- function(..., .n = NULL, .min = NULL, .max = NULL, .targ = NULL) {
  ns     <- uj::av(...)
  okDots <- base::length(ns) > 0
  okNs   <- uj::f0(!okDots, T, uj::.cmp_psw_vec(ns))
  okN    <- uj::f0(base::is.null(.n), T, uj::.cmp_psw_vec(.n))
  okMIN  <- uj::f0(base::is.null(.min), T, uj::.cmp_psw_scl(.min))
  okMAX  <- uj::f0(base::is.null(.max), T, uj::.cmp_psw_scl(.max))
  okTARG <- uj::f0(base::is.null(.targ), T, uj::.cmp_psw_scl(.targ))
  errs   <- NULL
  if (!okDots) {errs <- base::c(errs, "[...] is empty.")}
  if (!okNs  ) {errs <- base::c(errs, "[...] must atomize (?av) to a complete positive whole-number vec (?cmp_psw_vec).")}
  if (!okN   ) {errs <- base::c(errs, "[.n] must be NULL or a complete positive whole-number vec (?cmp_psw_vec).")}
  if (!okMIN ) {errs <- base::c(errs, "[.min] must be NULL or a complete positive whole-number scalar (?cmp_psw_scl).")}
  if (!okMAX ) {errs <- base::c(errs, "[.max] must be NULL or a complete positive whole-number scalar (?cmp_psw_scl).")}
  if (!okTARG) {errs <- base::c(errs, "[.targ] must be NULL or a complete positive whole-number scalar (?cmp_psw_scl).")}
  if (!base::is.null(errs)) {uj::stopperr(errs)}
  if (base::is.null(.targ)) {.targ <- base::max(ns)}
  reps <- .targ / ns
  if (!base::all(reps == base::round(reps))) {return(F)}
  if (!base::is.null(.n  )) {if (!(.targ %in% .n)) {return(F)}}
  if (!base::is.null(.min)) {if (.targ < .min    ) {return(F)}}
  if (!base::is.null(.max)) {if (.targ > .max    ) {return(F)}}
  T
}

#' @describeIn recycling_help Checks whether arguments in `...` are recyclable, subject to restrictions in `.n`, `.min`, `.max`, and `.targ`.
#' @export
recyclable <- function(..., .n = NULL, .min = 1, .max = NULL, .targ = NULL, .err = FALSE) {
  x    <- base::list(...)
  errs <- NULL
  if (base::length(x) == 0                      ) {errs <- base::c(errs, "[...] is empty.")}
  if (!base::all(base::sapply(x, uj::.atm_vec))) {errs <- base::c(errs, "Arguments in [...] must be atomic vecs (?atm_vec).")}
  if (!uj::.cmp_lgl_scl(.err)                  ) {errs <- base::c(errs, "[.err] must be scalar TRUE or scalar FALSE.")}
  if (!base::is.null(errs)                      ) {uj::stopperr(errs)}
  y <- uj::recyclable_ns(uj::ns(x), .n = .n, .min = .min, .max = .max, .targ = .targ)
  if (.err & !y) {uj::stopperr("[...] arguments are not recyclable.")}
  y
}

#' @describeIn recycling_help Recycles named arguments in `...` in the environment of the calling function, subject to restrictions in `.n`, `.min`, `.max`, and `.targ`.
#' @export
recycle <- function(..., .n = NULL, .min = 1, .max = NULL, .targ = NULL) {
  uj::recyclable(..., .n = .n, .min = .min, .max = .max, .targ = .targ, .err = T)
  nDots   <- base::...length()
  dotLens <- base::lengths(base::list(...))
  dotLabs <- base::...names()
  reps <- uj::f0(base::is.null(.targ), base::max(dotLens), .targ) / dotLens
  for (i in 1:nDots) {if (reps[i] > 1) {base::assign(dotLabs[i], base::rep.int(base::...elt(i), reps[i]), envir = base::parent.frame())}}
}
