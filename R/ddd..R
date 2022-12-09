#' @name ddd
#' @family props
#' @title Defined dimensionality properties
#' @description An object's defined dimensionality is the number of dimensions on which its elements can be indexed.
#' \cr\cr **Defined dimensioanlity properties**
#' \tabular{rcl}{
#'       `'d0D'`   \tab `0`-D   \tab   The `NULL` object.
#'   \cr `'d1D'`   \tab `1`-D   \tab   Vectors, \link[=ivls]{vlists}, `1D` arrays.
#'   \cr `'d2D'`   \tab `2`-D   \tab   Data.frames and matrices.
#'   \cr `'dHD'`   \tab Hyper-D \tab   Arrays with  `3+` dimensions.
#' }
#' **Defined dimensioanlity functions**
#' \tabular{rl}{
#'     `is_ddd_spec`   \tab Evaluates whether `spec` is a valid defined dimensionality property specification.
#'   \cr               \tab  
#'   \cr `ddd_props`   \tab Gets a character vector of all possible defined dimensionality property values.
#'   \cr               \tab  
#'   \cr      `nddd`   \tab Gets the number of defined dimensions of `x`.
#'   \cr               \tab  
#'   \cr      `iddd`   \tab Evaluates whether `x` possesses one or more (possibly pipe-delimited) defined dimensionality properties in `spec` (subject to any restrictions in `...`).
#'   \cr               \tab  
#'   \cr      `ixxx`   \tab Evaluates whether `x` possesses the defined dimensionality property `xxx`\eqn{^1} (subject to any restrictions in `...`).
#'   \cr               \tab  
#'   \cr       `ddd`   \tab Gets a character vector containing all defined dimensionality properties possessed by `x`.
#' }
#' \eqn{^{1.}} A defined dimensionality property.
#' @param x An R object.
#' @param spec `NULL` or a \link[=cmp_chr_scl]{complete character vec} containing one or more defined dimensionality properties from `ddd_props()`. Defined dimensionality properties may be pipe-delimited. If there are multiple properties in `spec`, `x` is inspected for a match to any of the specified properties.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return \tabular{rl}{
#'     `ddd_vals`   \tab A character vector.
#'   \cr   `nddd`   \tab A numeric scalar.
#'   \cr   `iddd`   \tab A logical scalar.
#'   \cr   `ixxx`   \tab A logical scalar\eqn{^2}.
#'   \cr    `ddd`   \tab A character vector.
#' }
#' \eqn{^{2.}} `xxx` is a defined dimensionality property.
#' @examples
#' is_ddd_spec("d1D|d2D")
#' nddd(matrix(1))
#' nddd(letters)
#' nddd(1)
#' ddd_props()
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
ddd_props <- function() {.ddds}

#' @rdname ddd
#' @export
is_ddd_spec <- function(spec) {spec <- .spec_vals(spec); f0(length(spec) == 0, F, all(spec %in% .ddds))}

#' @rdname ddd
#' @export
iddd <- function(x, spec, ...) {
  errs <- c(.meets_errs(x, ...), f0(is_ddd_spec(spec), NULL, '\n \u2022 [spec] must be a complete character vec (?cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from ddd_props().'))
  if (!is.null(errs)) {stop(errs)}
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
