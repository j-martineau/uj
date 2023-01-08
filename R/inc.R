#' @encoding UTF-8
#' @family to_std_fun_form
#' @title Increment or decrement a variable
#' @description \tabular{rl}{
#'      `inc` \tab   Increments `x` by `i`.
#'  \cr `dec` \tab   Decrements `x` by `d`.
#' }
#' @param x An \link[=atm_num]{atomic numeric} object.
#' @param i,d \link[=cmp_num_scl]{Complete numeric scalars} giving the value by which to increment or decrement, respectively.
#' @param na A non-`NA` logical scalar indicating whether `NA` values are allowed.
#' @return An atomic numeric object of the same dimension as `x`.
#' @examples
#' inc(pi, pi)
#' inc(1:5, 1)
#' inc(c(1:5, NA), -1, na = T)
#' dec(pi, pi)
#' dec(1:5, 1)
#' dec(c(1:5, NA), -1, na = T)
#' @export
inc <- function(x, i = 1, na = F) {
  ok.x <- uj::f0(uj::inum(x), T, uj::f0(uj::num_dtf(x), T, uj::num_vls(x)))
  errs <- base::c(uj::f0(ok.x                                                               , NULL, "[x] must be populated numeric object (?pop_num), a numeric dtf (?num_dtf), or a numeric vlist (?num_vls)."),
                  uj::f0(uj::cmp_num_scl(i)                                                 , NULL, "[i] must be a complete numeric scalar (?cmp_num_scl)."),
                  uj::f0(uj::isTF(na)                                                       , NULL, "[na] must be TRUE or FALSE."),
                  uj::f0(uj::f0(!ok.x | !uj::isF(na), T, !base::any(base::is.na(uj::av(x)))), NULL, "[x] contains NA values but [na = FALSE]."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  x + i
}

#' @rdname inc
#' @export
dec <- function(x, d = 1, na = F) {
  ok.x <- uj::f0(uj::inum(x), T, uj::f0(uj::num_dtf(x), T, uj::num_vls(x)))
  errs <- base::c(uj::f0(ok.x                                                               , NULL, "[x] must be populated numeric object (?pop_num), a numeric dtf (?num_dtf), or a numeric vlist (?num_vls)."),
                  uj::f0(uj::cmp_num_scl(d)                                                 , NULL, "[d] must be a complete numeric scalar (?cmp_num_scl)."),
                  uj::f0(uj::isTF(na)                                                       , NULL, "[na] must be TRUE or FALSE."),
                  uj::f0(uj::f0(!ok.x | !uj::isF(na), T, !base::any(base::is.na(uj::av(x)))), NULL, "[x] contains NA values but [na = FALSE]."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  x - d
}
