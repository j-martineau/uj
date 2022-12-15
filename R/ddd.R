#' @name ddd
#' @family props
#' @title Defined dimensionality (`defined-D`) properties
#' @description An object's defined dimensionality (`defined-D`) is the number of dimensions on which its elements can be indexed. The associated property values are:
#' \tabular{rcl}{
#'       `'d0D'` \tab   `0D` \tab   The `NULL` object.
#'   \cr `'d1D'` \tab   `1D` \tab   Vectors, \link[=ivls]{vlists}, `1D` arrays.
#'   \cr `'d2D'` \tab   `2D` \tab   Data.frames and matrices.
#'   \cr `'dHD'` \tab   `HD` \tab   Hyper (3+) dimensional arrays
#' }
#' **Functions**
#' \tabular{rl}{
#'     `is_ddd_spec`   \tab Is `spec` a `defined-D` property specification?
#'   \cr               \tab   
#'   \cr `ddd_props`   \tab What `defined-D` properties are there?
#'   \cr               \tab   
#'   \cr  `ddd_funs`   \tab What `defined-D` property functions are there?
#'   \cr               \tab   
#'   \cr      `nddd`   \tab How many defined dimensions does `x` have?
#'   \cr               \tab   
#'   \cr      `iddd`   \tab Is `x` a match to the `defined-D`-specific `spec`?
#'   \cr               \tab   
#'   \cr      `iDDD`   \tab is `x` a match to the single `defined-D` property `'DDD'`?
#'   \cr               \tab   
#'   \cr       `ddd`   \tab What are `x`'s `defined-D` properties?
#' }
#' @param x An R object.
#' @param spec `NULL` or a \link[=cmp_chr_scl]{complete character vec} containing one or more `defined-D` properties from `ddd_props()`. `defined-D` properties may be pipe-delimited. If there are multiple properties in `spec`, `x` is inspected for a match to any of the specified properties.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return \tabular{rl}{
#'     `is_ddd_spec`   \tab A logical scalar.
#'   \cr `ddd_props`   \tab A character vector
#'   \cr  `ddd_funs`   \tab A character vector
#'   \cr      `nddd`   \tab A numeric scalar
#'   \cr      `iddd`   \tab A logical scalar
#'   \cr      `iDDD`   \tab A logical scalar
#'   \cr       `ddd`   \tab A character vector
#' }
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
  for (d in .ddds) {out <- c(out, f0(run('.i', d, '(x)'), d, NULL))}
  out
}

#' @rdname ddd
#' @export
ddd_funs <- function() {paste0("i", .ddds)}

#' @rdname ddd
#' @export
ddd_props <- function() {.ddds}

#' @rdname ddd
#' @export
is_ddd_spec <- function(spec) {spec <- .spec_vals(spec); f0(length(spec) == 0, F, all(spec %in% .ddds))}

#' @rdname ddd
#' @export
iddd <- function(x, spec, ...) {
  errs <- c(.meets_errs(x, ...), f0(is_ddd_spec(spec), NULL, '[spec] must be a complete character vec (?cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from ddd_props().'))
  if (!is.null(errs)) {stop(.errs(errs))}
  if (!meets(x, ...)) {return(F)}
  for (prop in .spec_vals(spec)) {if (run('.i', prop, '(x)')) {return(T)}}
  F
}

#' @rdname ddd
#' @export
id0D <- function(x, ...) {iddd(x, 'd0D', ...)}

#' @rdname ddd
#' @export
id1D <- function(x, ...) {iddd(x, 'd1D', ...)}

#' @rdname ddd
#' @export
id2D <- function(x, ...) {iddd(x, 'd2D', ...)}

#' @rdname ddd
#' @export
idHD <- function(x, ...) {iddd(x, 'dHD', ...)}

#' @rdname ddd
#' @export
nddd <- function(x) {f0(is.null(x), 0, f0(is.vector(x), 1, length(dim(x))))}
