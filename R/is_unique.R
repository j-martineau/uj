#' @name is_unq.
#' @family extensions
#' @title Does an object have only unique atomic values?
#' @param x An atomic object.
#' @param a \link[cmp_lgl_scl]{Complete logical scalar} indicating whether to
#'   reduce \code{x} to an atomic vector containing all of its atomic values.
#'   When \code{FALSE} and \code{x} is not atomic, throws an error.
#' @param na \link[cmp_lgl_scl]{Complete logical scalar} indicating whether
#'   \code{NA} values are allowed.
#' @return \code{TRUE} or \code{FALSE}.
#' @export
is_unq. <- function() {help("is_unq.", package = "uj")}

#' @describeIn is_unq. Evaluates whether \code{x} contains only only unique
#'   atomic values, with options for for atomizing before evaluation and for
#'   handling \code{NA} values.
#' @export
is_unq <- function(x, a = T, na = F) {
  if (isTRUE(a)) {x <- unlist(x, T, F)}
  ok.atm <- is.atomic(x)
  errs <- c(f0(ok.atm                                     , NULL, "\n \u2022 [x] must be atomic."),
            f0(f0(ok.atm & isFALSE(na), !any(is.na(x)), T), NULL, "\n \u2022 [x] contains NA but [na = FALSE]."),
            f0(length(x) > 0                              , NULL, "\n \u2022 [x] is of length 0."),
            f0(isTF(a)                                    , NULL, "\n \u2022 [a] must be TRUE or FALSE."),
            f0(isTF(na)                                   , NULL, "\n \u2022 [na] must be TRUE or FALSE."))
  if (idef(errs)) {stop(errs)}
  length(x) == length(unique(x))
}
