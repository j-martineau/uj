#' @encoding UTF-8
#' @family extensions
#' @family properties
#' @title Does an atomic object only contain unique values?
#' @description Evaluates whether `x` contains only only unique atomic values, with options for for atomizing before evaluation and for handling `NA` values.
#' @details
#' \tabular{ll}{  `not_unique0, not_unq0`   \tab whether `x` *is not* an atomic vector containing any duplicate value.                            \cr   \tab   \cr
#'                `not_unique, not_unq`     \tab whether `x` *is not* a unique data.frame, list, or vector depending on values of `.A` and `.NA`. \cr   \tab   \cr
#'                `is_unique0, is_unq0`     \tab whether `x` *is* an atomic vector containing only unique values.                                 \cr   \tab   \cr
#'                `is_unique, is_unq`       \tab whether `x` *is* a unique data.frame, list, or vector depending on values of `.A` and `.NA`.                    }
#' @param x An atomic object.
#' @param .A `TRUE` or `FALSE` indicating whether to reduce `x` to an atomic vector containing all of its atomic values. When `FALSE` and `x` is not atomic, throws an error.
#' @param .NA `TRUE` or `FALSE` indicating whether `NA` values are allowed.
#' @return Scalar `TRUE` or scalar `FALSE`.
#' @examples
#' is_unique(letters)
#' is_unq(sample(letters, 27, replace = T))
#' @export
is_unique <- function(x, .A = FALSE, .NA = FALSE) {
  u_dtf <- function(D) {base::nrow(base::unique(D)) == base::nrow(D)}
  u_gen <- function(G) {base::length(base::unique(G)) == base::length(G)}
  Errors <- NULL
  if (!uj:::.cmp_lgl_scl(.A)) {Errors <- base::c(Errors, "[.A] must be TRUE or FALSE.")}
  if (!uj:::.cmp_lgl_scl(.NA)) {Errors <- base::c(Errors, "[.NA] must be TRUE or FALSE.")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  Atoms <- uj::av(x)
  if (.A) {x <- Atoms}
  if (base::length(Atoms) > 0) {
    OkNa <- .NA | !base::any(base::is.na(Atoms))
    if (!OkNa) {uj::stopperr("[.NA = FALSE] but [x] contains NA values.", .PKG = "uj")}
    if (base::is.data.frame(x)) {u_dtf(x)}
    else if (base::is.list(x)) {u_gen(x)}
    else {u_gen(Atoms)}
  } else {F}
}

#' @rdname is_unique
#' @export
is_unique0 <- function(...) {
  x <- uj::av(...)
  N <- base::length(x)
  N > 0 & N == base::length(base::unique(x))
}

#' @rdname is_unique
#' @export
not_unique <- function(x, .A = FALSE, .NA = FALSE) {!is_unique(x, .A = .A, .NA = .NA)}

#' @rdname is_unique
#' @export
not_unique0 <- function(...) {!uj::is_unique0(...)}

#' @rdname is_unique
#' @export
is_unq <- is_unique

#' @rdname is_unique
#' @export
is_unq0 <- is_unique0

#' @rdname is_unique
#' @export
not_unq <- not_unique

#' @rdname is_unique
#' @export
not_unq0 <- not_unique0
