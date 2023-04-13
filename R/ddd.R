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
#' \tabular{ll}{  `is_ddd_spec`   \tab Is `Spec` a defined.D property specification?                                                                          \cr   \tab   \cr
#'                `ddd_props`     \tab What defined.D properties are there?                                                                                   \cr   \tab   \cr
#'                `ddd_funs`      \tab What defined.D property functions are there?                                                                           \cr   \tab   \cr
#'                `{DDD}`         \tab Does `X` Match single defined.D property `'{DDD}'`? (where `{DDD}` is a placeholder for any given defined.D property). \cr   \tab   \cr
#'                `nddd`          \tab How many defined dimensions does `X` have?                                                                             \cr   \tab   \cr
#'                `ddd`           \tab What are `X`'s defined.D properties?                                                                                   \cr   \tab   \cr
#'                `DDD`           \tab Does `X` Match defined.D specification `Spec`?                                                                                        }
#' @param X An R object.
#' @param Spec `NULL` or a \link[=cmp_chr_scl]{complete character vec} containing one or more defined.D properties from `ddd_props()`. Defined.D properties may be pipe-delimited. If there are multiple properties in `Spec`, `X` is inspected for a Match to any of the specified properties.
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
ddd <- function(X) {
  Y <- NULL
  for (DDD in uj::v(DDD)) {
    Match <- uj::run("uj:::.", DDD, "(X)")
    if (Match) {Y <- base::c(Y, base::tolower(DDD))}
  }
  Y
}

#' @rdname ddd
#' @export
ddd_funs <- function() {uj::v(DDD)}

#' @rdname ddd
#' @export
ddd_props <- function() {uj::v(ddd)}

#' @rdname ddd
#' @export
is_ddd_spec <- function(Spec) {
  Spec <- uj:::.spec2props(Spec)
  if (base::length(Spec) == 0) {F} else {base::all(Spec %in% uj::v(ddd))}
}

#' @rdname ddd
#' @export
DDD <- function(X, Spec, ...) {
  Errors <- uj:::.meets_errs(X, ...)
  if (!uj::is_ddd_spec(Spec)) {Errors <- base::c(Errors, "[Spec] must be a complete character vec (?cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from ddd_props().")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  if (uj::meets(X, ...)) {
    Props <- uj:::.spec2props(Spec)
    for (Prop in base::toupper(Props)) {
      if (uj::run('uj:::.', Prop, '(X)')) {return(T)}
    }
  }
  F
}

#' @rdname ddd
#' @export
D0D <- function(X, ...) {uj::DDD(X, 'd0d', ...)}

#' @rdname ddd
#' @export
D1D <- function(X, ...) {uj::DDD(X, 'd1d', ...)}

#' @rdname ddd
#' @export
D2D <- function(X, ...) {uj::DDD(X, 'd2d', ...)}

#' @rdname ddd
#' @export
DHD <- function(X, ...) {uj::DDD(X, 'dHd', ...)}

#' @rdname ddd
#' @export
nddd <- function(X) {uj::f0(base::is.null(X), 0, uj::f0(base::is.vector(X), 1, base::length(base::dim(X))))}
