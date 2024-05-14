#' @encoding UTF-8
#' @title Does an atomic object only contain unique values?
#' @description Evaluates whether `x` contains only only unique atomic values, with options for for atomizing before evaluation and for handling `NA` values.
#' @details
#' \tabular{ll}{  `not_unique0, not_unq0`   \tab whether `x` *is not* an atomic vector containing any duplicate value.                            \cr   \tab   \cr
#'                `not_unique, not_unq`     \tab whether `x` *is not* a unique data.frame, list, or vector depending on values of `a` and `na`. \cr   \tab   \cr
#'                `is_unique0, is_unq0`     \tab whether `x` *is* an atomic vector containing only unique values.                                 \cr   \tab   \cr
#'                `is_unique, is_unq`       \tab whether `x` *is* a unique data.frame, list, or vector depending on values of `a` and `na`.                    }
#' @param x An atomic object.
#' @param a `TRUE` or `FALSE` indicating whether to reduce `x` to an atomic vector containing all of its atomic values. When `FALSE` and `x` is not atomic, throws an error.
#' @param na `TRUE` or `FALSE` indicating whether `NA` values are allowed.
#' @return A logical scalar.
#' @examples
#' is_unique(letters)
#' is_unq(sample(letters, 27, replace = T))
#' @export
uniqueness <- function() {utils::help("uniqueness", package = "uj")}

#' @describeIn uniqueness Evaluate whether `x` *is* a unique data.frame, list, or vector depending on values of `a` and `na`.
#' @export
is_unique <- function(x, a = FALSE, na = FALSE) {
  u_dtf <- function(D) {base::nrow(base::unique(D)) == base::nrow(D)}
  u_gen <- function(G) {base::length(base::unique(G)) == base::length(G)}
  errs <- NULL
  if (!uj::.cmp_lgl_scl(a)) {errs <- base::c(errs, "[a] must be TRUE or FALSE.")}
  if (!uj::.cmp_lgl_scl(na)) {errs <- base::c(errs, "[na] must be TRUE or FALSE.")}
  if (!base::is.null(errs)) {uj::stopperr(errs)}
  atoms <- uj::av(x)
  if (a) {x <- atoms}
  if (base::length(atoms) > 0) {
    okNa <- na | !base::any(base::isna(atoms))
    if (!okNa) {uj::stopperr("[na = FALSE] but [x] contains NA values.")}
    if (base::is.data.frame(x)) {u_dtf(x)}
    else if (base::is.list(x)) {u_gen(x)}
    else {u_gen(atoms)}
  } else {F}
}

#' @describeIn uniqueness Evaluates whether `x` *is* an atomic vector containing only unique values.
#' @export
is_unique0 <- function(...) {
  x <- uj::av(...)
  N <- base::length(x)
  N > 0 & N == base::length(base::unique(x))
}

#' @describeIn uniqueness Evaluates whether `x` *is not* a unique data.frame, list, or vector depending on values of `a` and `na`.
#' @export
not_unique <- function(x, a = FALSE, na = FALSE) {!uj::is_unique(x, a = a, na = na)}

#' @describeIn uniqueness Evaluates whether `x` *is not* an atomic vector containing any duplicate value.
#' @export
not_unique0 <- function(...) {!uj::is_unique0(...)}

#' @describeIn uniqueness An alias for `is_unique`
#' @export
is_unq <- is_unique

#' @describeIn uniqueness An alias for `is_unique0`
#' @export
is_unq0 <- is_unique0

#' @describeIn uniqueness An alias for `not_unique`
#' @export
not_unq <- not_unique

#' @describeIn uniqueness An alias for `not_unique0`
#' @export
not_unq0 <- not_unique0
