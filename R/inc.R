#' @name inc
#' @family math
#' @title Increment or Decrement a Variable
#' @description \tabular{ll}{
#'   \code{inc}   \tab Increments \code{x} by the value in \code{i}.         \cr
#'   \code{dec}   \tab Decrements \code{x} by the value in \code{d}.           }
#' @param x \link[=atm_num]{Atomic numeric} object.
#' @param i,d \link[=cmp_num_scl]{Complete numeric scalar} giving the value by
#'   which to increment or decrement, respectively.
#' @param na \link[=cmp_lgl_scl]{Complete logical scalar} indicating whether to
#'   throw an error if any values of \code{x} are \code{NA}.
#' @return An atomic numeric object of the same dimension as \code{x}.
#' @export
inc <- function(x, i = 1, na = F) {
  ok.x <- f0(inum(x), T, f0(num_dtf(x), T, num_vls(x)))
  errs <- c(f0(ok.x                                       , NULL, "\n \u2022 [x] must be populated numeric object (?pop_num), a numeric dtf (?num_dtf), or a numeric vlist (?num_vls)."),
            f0(cmp_num_scl(i)                             , NULL, "\n \u2022 [i] must be a complete numeric scalar (?cmp_num_scl)."),
            f0(isTF(na)                                   , NULL, "\n \u2022 [na] must be TRUE or FALSE."),
            f0(f0(!ok.x | !isF(na), T, !any(is.na(av(x)))), NULL, "\n \u2022 [x] contains NA values but [na = FALSE]."))
  if (idef(errs)) {stop(errs)}
  x + i
}

#' @rdname inc
#' @export
dec <- function(x, d = 1, na = F) {
  ok.x <- f0(inum(x), T, f0(num_dtf(x), T, num_vls(x)))
  errs <- c(f0(ok.x                                       , NULL, "\n \u2022 [x] must be populated numeric object (?pop_num), a numeric dtf (?num_dtf), or a numeric vlist (?num_vls)."),
            f0(cmp_num_scl(d)                             , NULL, "\n \u2022 [d] must be a complete numeric scalar (?cmp_num_scl)."),
            f0(isTF(na)                                   , NULL, "\n \u2022 [na] must be TRUE or FALSE."),
            f0(f0(!ok.x | !isF(na), T, !any(is.na(av(x)))), NULL, "\n \u2022 [x] contains NA values but [na = FALSE]."))
  if (idef(errs)) {stop(errs)}
  x - d
}
