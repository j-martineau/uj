#' @encoding UTF-8
#' @family to_std_fun_form
#' @title Increment or decrement a variable
#' @description \tabular{ll}{  `inc`   \tab Increments `x` by `i`. \cr
#'                             `dec`   \tab Decrements `x` by `d`.   }
#' @param x An \link[=atm_num]{atomic numeric} object.
#' @param i,d \link[=cmp_num_scl]{Complete numeric scalars} giving the value by which to increment or decrement, respectively.
#' @param na `TRUE` or `FALSE` indicating whether `NA` values are allowed.
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
  ok.x <- uj::f0(uj::NUM(x), T, uj::f0(uj::num_dtf(x), T, uj::num_vls(x)))
  uj::errs_if_nots(ok.x                                                   , "[x] must be populated numeric object (?pop_num), a numeric dtf (?num_dtf), or a numeric vlist (?num_vls).",
                   uj::num_scl(i)                                         , "[i] must be a numeric scalar (?num_scl)."                                                                 ,
                   uj::isTF1(na)                                          , "[na] must be TRUE or FALSE."                                                                              ,
                   uj::f0(!ok.x | uj::notF1(na), T, uj::noneNA(uj::av(x))), "[x] contains NA values but [na = FALSE]."                                                                 , PKG = "uj")
  x + i
}

#' @rdname inc
#' @export
dec <- function(x, d = 1, na = F) {
  ok.x <- uj::f0(uj::iNUM(x), T, uj::f0(uj::num_dtf(x), T, uj::num_vls(x)))
  uj::errs_if_nots(ok.x                                                   , "[x] must be populated numeric object (?pop_num), a numeric dtf (?num_dtf), or a numeric vlist (?num_vls).",
                   uj::num_scl(d)                                         , "[d] must be a complete numeric scalar (?num_scl)."                                                        ,
                   uj::isTF1(na)                                          , "[na] must be TRUE or FALSE."                                                                              ,
                   uj::f0(!ok.x | uj::notF1(na), T, uj::noneNA(uj::av(x))), "[x] contains NA values but [na = FALSE]."                                                                 , PKG = "uj")
  x - d
}
