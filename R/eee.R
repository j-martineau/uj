#' @name eee
#' @encoding UTF-8
#' @family properties
#' @title Effective dimensionality (effective.D) properties
#' @description An object's effective.D is defined as the number of dimensions in which it has multiple populated index positions (i.e., if a dimension has only one index position, that is not an effective dimension).
#' \tabular{ll}{  `'eud', 'EUD'`   \tab *Effectively* `NaN` *dimensional*.                                                                                                                                                                                      \cr
#'                                 \tab Length-`0` objects including `NULL` (of undefined effective.D).                                                                                                                                            \cr   \tab   \cr
#'                `'e0d', 'E0D'`   \tab *Effectively* `0` *dimensional*                                                                                                                                                                                         \cr
#'                                 \tab `1x1` data.frames and length-`1` vectors, \code{\link[=VLS]{vlists}}, and arrays.                                                                                                                          \cr   \tab   \cr
#'                `'e1d', 'E1D'`   \tab *Effectively* `1` *dimensional*                                                                                                                                                                                         \cr
#'                                 \tab \code{\link[=MVC]{multivecs}}, length-`2+` vlists, \code{\link[=ROW]{row}} data.frames, \code{\link[=COL]{column}} data.frames, and length-`2+` arrays with `2+` index positions in exactly `1` dimension. \cr   \tab   \cr
#'                `'e2d', 'E2D'`   \tab *Effectively* `2` *dimensional*                                                                                                                                                                                         \cr
#'                                 \tab `2+ x 2+ `data.frames and length-`4+` arrays with `2+` positions in exactly `2` dimensions.                                                                                                                \cr   \tab   \cr
#'                `'ehd', 'EHD'`   \tab *Effectively hyper dimensional*                                                                                                                                                                            \cr   \tab   \cr
#'                                 \tab Length-`8+` arrays with `2+` positions in `3+` dimensions.                                                                                                                                                                }
#' \cr Effective dimensionality property functions are:
#' \tabular{ll}{  `is_eee_spec`   \tab Is `spec` an effective.D specification?                                                                                                    \cr   \tab   \cr
#'                `eee_props`    \tab What effective.D properties are there?                                                                                                      \cr   \tab   \cr
#'                `eee_funs`     \tab What effective.D property functions are there?                                                                                              \cr   \tab   \cr
#'                `{EEE}`        \tab Is `x` a match to the single effective.D property `'{EEE}'` where `{EEE}` is a placeholder for any given effective dimensionality property? \cr   \tab   \cr
#'                `neee`         \tab How many effective dimensions does `x` have?                                                                                                \cr   \tab   \cr
#'                `eee`          \tab What are `x`'s effective.d properties?                                                                                                      \cr   \tab   \cr
#'                `EEE`          \tab Is `x` a match to the effective.D specification in `spec`?                                                                                                 }
#' @param x An R object.
#' @param spec `NULL` or a \link[=cmp_chr_scl]{complete character vec} containing one or more effective.D properties from `eee_props()`. Effective.D properties may be pipe-delimited. If there are multiple properties in `spec`, `x` is inspected for a match to any of the specified properties.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return **A character vector** \cr\cr `eee_props, eee_funs, eee`
#' \cr\cr  **A logical scalar**   \cr\cr `is_eee_spec, EEE, {EEE}`
#' \cr\cr  **An integer scalar**  \cr\cr `neee`
#' @examples
#' a. <- "a"
#' abc. <- c("a", "b", "c")
#' udf1 <- NULL
#' udf2 <- character(0)
#' udf3 <- matrix(nrow = 0, ncol = 0)
#' udf4 <- list()
#' zro1 <- a.
#' zro2 <- list(abc = abc.)
#' zro3 <- data.frame(a = a.)
#' zro4 <- array(a., dim = c(1, 1, 1))
#' one1 <- matrix(abc., nrow = 1)
#' one2 <- matrix(abc., ncol = 1)
#' one3 <- data.frame(a. = abc.)
#' one4 <- array(abc., dim = c(3, 1, 1))
#' two1 <- matrix(rep.int(abc., 2), nrow = 2)
#' two2 <- data.frame(a = abc., b = abc.)
#' two3 <- array(1:4, dim = c(2, 2, 1))
#' two4 <- array(1:4, dim = c(1, 2, 2, 1))
#' hyp1 <- array(1:8, dim = c(2, 2, 2))
#' hyp2 <- array(1:8, dim = c(2, 2, 2, 1))
#' hyp3 <- array(1:8, dim = c(1, 2, 2, 2, 1))
#' hyp4 <- array(1:16, dim = c(2, 2, 2, 2))
#'
#' eee_funs()
#' eee_props()
#' is_eee_spec("eUD")
#' is_eee_spec("d1D")
#' c(neee(udf1), neee(udf2), neee(udf3), neee(udf4))
#' c(neee(zro1), neee(zro2), neee(zro3), neee(zro4))
#' c(neee(one1), neee(one2), neee(one3), neee(one4))
#' c(neee(two1), neee(two2), neee(two3), neee(two4))
#' c(neee(hyp1), neee(hyp2), neee(hyp3), neee(hyp4))
#' c(EEE(one1, "eud"), EEE(one1, "e0d"), EEE(one1, "e1d"), EEE(one1, "e2d"), EEE(one1, "ehd"))
#' c(EUD(one1), E0D(one1), E1D(one1), E2D(one1), EHD(one1))
#' c(eee(udf1), eee(zro1), eee(one1), eee(two1), eee(hyp1), eee(hyp4))
#' @export
eee <- function(x) {
  y <- NULL
  for (EEE in uj::v(EEE)) {
    Match <- uj::run("uj:::.", EEE, "(x)")
    if (Match) {y <- base::c(y, base::tolower(EEE))}
  }
  y
}

#' @rdname eee
#' @export
eee_funs <- function() {uj::v(EEE)}

#' @rdname eee
#' @export
eee_props <- function() {uj::v(eee)}

#' @rdname eee
#' @export
is_eee_spec <- function(spec) {
  spec <- uj:::.spec2props(spec)
  if (base::length(spec) == 0) {F} else {base::all(spec %in% uj::v(eee))}
}

#' @rdname eee
#' @export
EEE <- function(x, spec, ...) {
  Errors <- uj:::.meets_errs(x, ...)
  if (!uj::is_eee_spec(spec)) {Errors <- base::c(Errors, "[spec] must be a complete character vec (?cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from eee_props().")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, .PKG = 'uj')}
  if (uj::meets(x, ...)) {
    Props <- uj:::.spec2props(spec)
    for (Prop in base::toupper(Props)) {
      Match <- uj::run("uj:::.", Prop, "(x)")
      if (Match) {return(T)}
    }
  }
  F
}

#' @rdname eee
#' @export
E0D <- function(x, ...) {uj::EEE(x, 'e0D', ...)}

#' @rdname eee
#' @export
E1D <- function(x, ...) {uj::EEE(x, 'e1D', ...)}

#' @rdname eee
#' @export
E2D <- function(x, ...) {uj::EEE(x, 'e2D', ...)}

#' @rdname eee
#' @export
EHD <- function(x, ...) {uj::EEE(x, 'eHD', ...)}

#' @rdname eee
#' @export
EUD <- function(x, ...) {uj::EEE(x, 'eUD', ...)}

#' @rdname eee
#' @export
neee <- function(x) {
  if (base::length(x) == 0) {NaN}
  else if (base::NCOL(x) * base::NROW(x) == 1) {0}
  else if (base::is.vector(x)) {1}
  else {base::length(base::which(base::dim(x) > 1))}
}
