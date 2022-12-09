#' @title Evaluate whether an object has only unique atomic values
#' @description Evaluates whether `x` contains only only unique atomic values, with options for for atomizing before evaluation and for handling `NA` values.
#' @param x An atomic object.
#' @param a A non-`NA` logical scalar indicating whether to reduce `x` to an atomic vector containing all of its atomic values. When `FALSE` and `x` is not atomic, throws an error.
#' @param na A non-`NA` logical scalar indicating whether `NA` values are allowed.
#' @return `TRUE` or `FALSE`.
#' @examples
#' is_unq(letters)
#' is_unq(sample(letters, 27, replace = T))
#' @export
is_unq <- function(x, a = T, na = F) {
  if (isTRUE(a)) {x <- unlist(x, T, F)}
  ok.atm <- is.atomic(x)
  errs <- c(f0(ok.atm                                     , NULL, .errx("[x] must be atomic.")),
            f0(f0(ok.atm & isFALSE(na), !any(is.na(x)), T), NULL, .errx("[x] contains NA but [na = FALSE].")),
            f0(length(x) > 0                              , NULL, .errx("[x] is of length 0.")),
            f0(isTF(a)                                    , NULL, .errx("[a] must be TRUE or FALSE.")),
            f0(isTF(na)                                   , NULL, .errx("[na] must be TRUE or FALSE.")))
  if (!is.null(errs)) {stop(errs)}
  length(x) == length(unique(x))
}
