#' @name eee
#' @family props
#' @title Effective dimensionality (`effective-D`) properties
#' @description An object's `effective-D` is defined as the number of dimensions in which it has multiple populated index positions (i.e., if a dimension has only one index position, that is not an effective dimension).
#' \tabular{rl}{
#'         `'eUD'`   \tab *Effectively* `NaN` *dimensional*.
#'   \cr             \tab Length-`0` objects including `NULL` (of undefined `effective-D`).
#'   \cr             \tab   
#'   \cr   `'e0D'`   \tab *Effectively* `0` *dimensional*
#'   \cr             \tab `1x1 data.frames` and length-`1 vectors`, \code{\link[=ivls]{vlists}}, and `arrays`.
#'   \cr             \tab   
#'   \cr   `'e1D'`   \tab *Effectively* `1` *dimensional*
#'   \cr             \tab \code{\link[=imvc]{multivecs}}, length-`2+ vlists`, \code{\link[=irow]{row}} `data.frames`, \code{\link[=icol]{col}} `data.frames`, and length-`2+` arrays with `2+` index positions in exactly `1` dimension.
#'   \cr             \tab   
#'   \cr   `'e2D'`   \tab *Effectively* `2` *dimensional*
#'   \cr             \tab `2+ x 2+ data.frames` and length-`4+ arrays` with `2+` positions in exactly `2` dimensions.
#'   \cr             \tab   
#'   \cr   `'eHD'`   \tab *Effectively* `hyper` *dimensional*
#'   \cr             \tab Length-`8+ arrays` with `2+` positions in `3+` dimensions.
#' }
#' \cr\cr
#' Effective dimensionality property functions are:
#' \tabular{rl}{
#'     `is_eee_spec`   \tab Is `spec` an `effective-D` specification?
#'   \cr `eee_props`   \tab What `effective-D` properties are there?
#'   \cr  `eee_funs`   \tab What `effective-D` property functions are there?
#'   \cr      `neee`   \tab How many effective dimensions does `x` have?
#'   \cr      `ieee`   \tab Is `x` a match to the `effective-D` specification`spec`?
#'   \cr      `iEEE`   \tab Is `x` a match to the single `effective-D` property `'EEE'`?
#'   \cr       `eee`   \tab What are `x`'s `effective-D` properties?
#' }
#' @param x An R object.
#' @param spec `NULL` or a \link[=cmp_chr_scl]{complete character vec} containing one or more `effective-D` properties from `eee_props()`. `Effective-D` properties may be pipe-delimited. If there are multiple properties in `spec`, `x` is inspected for a match to any of the specified properties.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return \tabular{rl}{
#'     `is_eee_spec`   \tab A logical scalar.
#'   \cr `eee_props`   \tab A character vector.
#'   \cr  `eee_funs`   \tab A logical scalar.
#'   \cr      `ieee`   \tab A logical scalar.
#'   \cr      `iEEE`   \tab A logical scalar.
#'   \cr      `neee`   \tab A numeric scalar.
#'   \cr       `eee`   \tab A character vector.
#' }
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
#'
#' c(ieee(one1, "eUD"), ieee(one1, "e0D"), ieee(one1, "e1D"), ieee(one1, "e2D"), ieee(one1, "eHD"))
#' c(ieUD(one1), ie0D(one1), ie1D(one1), ie2D(one1), ieHD(one1))
#'
#' c(eee(udf1), eee(zro1), eee(one1), eee(two1), eee(hyp1), eee(hyp4))
#' @export
eee <- function(x) {
  out <- NULL
  for (e in .eees) {out <- c(out, f0(run('.i', e, '(x)'), e, NULL))}
  out
}

#' @rdname eee
#' @export
eee_funs <- function() {paste0("i", .eees)}

#' @rdname eee
#' @export
eee_props <- function() {.eees}

#' @rdname eee
#' @export
is_eee_spec <- function(spec) {spec <- .spec_vals(spec); f0(length(spec) == 0, F, all(spec %in% .eees))}

#' @rdname eee
#' @export
ieee <- function(x, spec, ...) {
  errs <- c(.meets_errs(x, ...),
            f0(is_eee_spec(spec), NULL, '[spec] must be a complete character vec (?cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from eee_props().'))
  if (!is.null(errs)) {stop(.errs(errs))}
  if (!meets(x, ...)) {return(F)}
  for (prop in .spec_vals(spec)) {if (run('.i', prop, '(x)')) {return(T)}}
  F
}

#' @rdname eee
#' @export
ie0D <- function(x, ...) {ieee(x, 'e0D', ...)}

#' @rdname eee
#' @export
ie1D <- function(x, ...) {ieee(x, 'e1D', ...)}

#' @rdname eee
#' @export
ie2D <- function(x, ...) {ieee(x, 'e2D', ...)}

#' @rdname eee
#' @export
ieHD <- function(x, ...) {ieee(x, 'eHD', ...)}

#' @rdname eee
#' @export
ieUD <- function(x, ...) {ieee(x, 'eUD', ...)}

#' @rdname eee
#' @export
neee <- function(x) {f0(length(x) == 0, NaN, f0(NROW(x) * NCOL(x) == 1, 0, f0(is.vector(x), 1, length(which(dim(x) > 1)))))}
