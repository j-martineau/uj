#' @name ddd
#' @encoding UTF-8
#' @family properties
#' @title Defined dimensionality (defined.D) properties
#' @description An object's defined dimensionality (defined.D) is the number of dimensions on which its elements can be indexed. The associated property values are:
#' \tabular{lll}{  `'d0d', 'D0D'`   \tab `0D`   \tab The `NULL` object.                          \cr
#'                 `'d1d', 'D1D'`   \tab `1D`   \tab Vectors, \link[=VLS]{vlists}, `1D` arrays.  \cr
#'                 `'d2d', 'D2D'`   \tab `2D`   \tab Data.frames and matrices.                   \cr
#'                 `'dhd', 'DHD'`   \tab `HD`   \tab Hyper (3+) dimensional arrays                 }
#' \cr **Defined dimensionality functions**
#' \tabular{ll}{  `is_ddd_spec`   \tab Is `spec` a defined.D property specification?                                                                          \cr   \tab   \cr
#'                `ddd_props`     \tab What defined.D properties are there?                                                                                   \cr   \tab   \cr
#'                `ddd_funs`      \tab What defined.D property functions are there?                                                                           \cr   \tab   \cr
#'                `{DDD}`         \tab Does `x` match single defined.D property `'{DDD}'`? (where `{DDD}` is a placeholder for any given defined.D property). \cr   \tab   \cr
#'                `nddd`          \tab How many defined dimensions does `x` have?                                                                             \cr   \tab   \cr
#'                `ddd`           \tab What are `x`'s defined.D properties?                                                                                   \cr   \tab   \cr
#'                `DDD`           \tab Does `x` match defined.D specification `spec`?                                                                                        }
#' @param x An R object.
#' @param spec `NULL` or a \link[=cmp_chr_scl]{complete character vec} containing one or more defined.D properties from `ddd_props()`. Defined.D properties may be pipe-delimited. If there are multiple properties in `spec`, `x` is inspected for a match to any of the specified properties.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return **A character vector** \cr\cr `ddd_props, ddd_funs, ddd`
#' \cr\cr  **A logical scalar**   \cr\cr `is_ddd_spec, DDD, {DDD}`
#' \cr\cr  **A numeric scalar**   \cr\cr `nddd`
#' @examples
#' ddd_funs()
#' ddd_props()
#' is_ddd_spec("d1D|d2D")
#' nddd(matrix(1))
#' nddd(letters)
#' nddd(1)
#' DDD(data.frame(letters), "d2d|dhd")
#' D0D(NULL)
#' D1D(NULL)
#' ddd(letters)
#' @export
ddd <- function(x) {
  y <- NULL
  for (DDD in uj:::.DDD) {
    match <- uj::run("uj:::.", DDD, "(x)")
    if (match) {y <- base::c(y, base::tolower(DDD))}
  }
  y
}

#' @rdname ddd
#' @export
ddd_funs <- function() {uj:::.DDD}

#' @rdname ddd
#' @export
ddd_props <- function() {uj:::.ddd}

#' @rdname ddd
#' @export
is_ddd_spec <- function(spec) {
  spec <- uj:::.spec2props(spec)
  if (base::length(spec) == 0) {F} else {base::all(spec %in% uj:::.ddd)}
}

#' @rdname ddd
#' @export
DDD <- function(x, spec, ...) {
  errs <- uj:::.meets_errs(x, ...)
  if (!uj::is_ddd_spec(spec)) {errs <- base::c(errs, "[spec] must be a complete character vec (?cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from ddd_props().")}
  if (!base::is.null(errs)) {uj::stopperr(errs, PKG = "uj")}
  if (uj::meets(x, ...)) {
    props <- uj:::.spec2props(spec)
    for (prop in base::toupper(props)) {
      if (uj::run('uj:::.', prop, '(x)')) {return(T)}
    }
  }
  F
}

#' @rdname ddd
#' @export
D0D <- function(x, ...) {uj::DDD(x, 'd0d', ...)}

#' @rdname ddd
#' @export
D1D <- function(x, ...) {uj::DDD(x, 'd1d', ...)}

#' @rdname ddd
#' @export
D2D <- function(x, ...) {uj::DDD(x, 'd2d', ...)}

#' @rdname ddd
#' @export
DHD <- function(x, ...) {uj::DDD(x, 'dHd', ...)}

#' @rdname ddd
#' @export
nddd <- function(x) {uj::f0(base::is.null(x), 0, uj::f0(base::is.vector(x), 1, base::length(base::dim(x))))}
