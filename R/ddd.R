#' @name ddd
#' @encoding UTF-8
#' @family properties
#' @title Defined dimensionality (defined.d) properties
#' @description An object's defined dimensionality (defined.d) is the number of dimensions on which its elements can be indexed. The associated property values are:
#' \tabular{rcl}{
#'       `'d0D'` \tab   `0D`   \tab The `NULL` object.
#'   \cr `'d1D'` \tab   `1D`   \tab Vectors, \link[=ivls]{vlists}, `1D` arrays.
#'   \cr `'d2D'` \tab   `2D`   \tab Data.frames and matrices.
#'   \cr `'dHD'` \tab   `HD`   \tab Hyper (3+) dimensional arrays
#' }
#' \cr
#' **Functions**
#' \tabular{rl}{
#'     `is_ddd_spec`   \tab Is `spec` a defined.d property specification?
#'   \cr `ddd_props`   \tab What defined.d properties are there?
#'   \cr  `ddd_funs`   \tab What defined.d property functions are there?
#'   \cr      `nddd`   \tab How many defined dimensions does `x` have?
#'   \cr      `iddd`   \tab Does `x` match defined.d specification `spec`?
#'   \cr      `iDDD`   \tab Does `x` match single defined.d property `'DDD'`?
#'   \cr       `ddd`   \tab What are `x`'s defined.d properties?
#' }
#' @param x An R object.
#' @param spec `NULL` or a \link[=cmp_chr_scl]{complete character vec} containing one or more defined.d properties from `ddd_props()`. defined.d properties may be pipe-delimited. If there are multiple properties in `spec`, `x` is inspected for a match to any of the specified properties.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return *A character vector*
#'  \cr   `ddd_props`
#'  \cr   `ddd_funs`
#'  \cr   `ddd`
#'  \cr
#'  \cr *A logical scalar*
#'  \cr   `is_ddd_spec`
#'  \cr   `iDDD`
#'  \cr   `iddd`
#'  \cr
#'  \cr *A numeric scalar*
#'  \cr   `nddd`
#' @examples
#' ddd_funs()
#' ddd_props()
#' is_ddd_spec("d1D|d2D")
#' nddd(matrix(1))
#' nddd(letters)
#' nddd(1)
#' iddd(data.frame(letters), "d2D|dHD")
#' id0D(NULL)
#' id1D(NULL)
#' ddd(letters)
#' @export
ddd <- function(x) {
  out <- NULL
  for (d in .ddds) {out <- base::c(out, uj::f0(run('uj:::.i', d, '(x)'), d, NULL))}
  out
}

#' @rdname ddd
#' @export
ddd_funs <- function() {base::paste0("i", uj:::.ddds)}

#' @rdname ddd
#' @export
ddd_props <- function() {uj:::.ddds}

#' @rdname ddd
#' @export
is_ddd_spec <- function(spec) {spec <- uj:::.spec_vals(spec); uj::f0(base::length(spec) == 0, F, base::all(spec %in% .ddds))}

#' @rdname ddd
#' @export
iddd <- function(x, spec, ...) {
  errs <- base::c(uj:::.meets_errs(x, ...), uj::f0(uj::is_ddd_spec(spec), NULL, '[spec] must be a complete character vec (?cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from ddd_props().'))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  if (!uj::meets(x, ...)) {return(F)}
  for (prop in uj:::.spec_vals(spec)) {if (uj::run('uj:::.i', prop, '(x)')) {return(T)}}
  F
}

#' @rdname ddd
#' @export
id0D <- function(x, ...) {uj::iddd(x, 'd0D', ...)}

#' @rdname ddd
#' @export
id1D <- function(x, ...) {uj::iddd(x, 'd1D', ...)}

#' @rdname ddd
#' @export
id2D <- function(x, ...) {uj::iddd(x, 'd2D', ...)}

#' @rdname ddd
#' @export
idHD <- function(x, ...) {uj::iddd(x, 'dHD', ...)}

#' @rdname ddd
#' @export
nddd <- function(x) {uj::f0(base::is.null(x), 0, uj::f0(base::is.vector(x), 1, base::length(base::dim(x))))}
