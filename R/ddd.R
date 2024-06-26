#' @encoding UTF-8
#' @family properties
#' @title Defined Dimensionality Droperties
#' @description An object's defined dimensionality is the number of dimensions on which its elements can be indexed. The associated property values are:
#' \tabular{lll}{  `'d0d', 'D0D'`   \tab `0D`   \tab The `NULL` object.                          \cr
#'                 `'d1d', 'D1D'`   \tab `1D`   \tab Vectors, \link[=VLS]{vlists}, `1D` arrays.  \cr
#'                 `'d2d', 'D2D'`   \tab `2D`   \tab Data.frames and matrices.                   \cr
#'                 `'dhd', 'DHD'`   \tab `HD`   \tab Hyper-dimensional arrays (of 3+ dimensions).  }
#' @param x An R object.
#' @param spec `NULL` or a \link[=cmp_chr_scl]{complete character vec} containing one or more defined.D properties from `ddd_props()`. Defined.D properties may be pipe-delimited. If there are multiple properties in `spec`, `x` is inspected for a Match to any of the specified properties.
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
ddd_prop_funs <- function() {utils::help("ddd_prop_funs", package = "uj")}

#' @describeIn ddd_prop_funs Lists all defined dimensionality properties possessed by `x`. Returns a lowercase character scalar.
#' @export
ddd <- function(x) {
  Y <- NULL
  for (DDD in uj::ddd_funs()) {
    if (uj::run("uj::.", DDD, "(x)")) {
      Y <- base::c(Y, base::tolower(DDD))
    }
  }
  Y
}

#' @describeIn ddd_prop_funs Lists all defined dimensionality property checking functions. Returns a sorted, uppercase, character vector.
#' @export
ddd_funs <- function() {base::c("D0D", "D1D", "D2D", "DHD")}

#' @describeIn ddd_prop_funs Lists all defined dimensionality properties. Returns a sorted, lowercase, character vector.
#' @export
ddd_props <- function() {base::c("d0d", "d1d", "d2d", "dhd")}

#' @describeIn ddd_prop_funs Checks whether `spec` is a defined-dimensionality property spec. Returns a logical scalar. See \code{\link{ppp}} for a definition of a property spec.
#' @export
is_ddd_spec <- function(spec) {
  spec <- uj::spec2props(spec)
  if (base::length(spec) == 0) {F} else {base::all(spec %in% uj::ddd_props())}
}

#' @describeIn ddd_prop_funs Checks whether `spec` is a defined-dimensionality property spec subject to any count or value restrictions in `...`. Returns a logical scalar. See \code{\link{ppp}} for a definition of a property spec.
#' @export
DDD <- function(x, spec, ...) {
  errs <- uj::meets_errs(x, ...)
  if (!uj::is_ddd_spec(spec)) {errs <- base::c(errs, "[spec] must be a complete character vec (?cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from ddd_props().")}
  if (!base::is.null(errs)) {uj::stopperr(errs)}
  if (uj::meets(x, ...)) {
    props <- uj::spec2props(spec)
    for (prop in base::toupper(props)) {if (uj::run('uj::.', prop, '(x)')) {return(T)}}
  }
  F
}

#' @describeIn ddd_prop_funs Checks `x` for zero defined dimensions subject to any count or value restrictions in `...`. Returns a logical scalar.
#' @export
D0D <- function(x, ...) {uj::DDD(x, 'd0d', ...)}

#' @describeIn ddd_prop_funs Checks `x` for one defined dimensions subject to any count or value restrictions in `...`. Returns a logical scalar.
#' @export
D1D <- function(x, ...) {uj::DDD(x, 'd1d', ...)}

#' @describeIn ddd_prop_funs Checks `x` for two defined dimensions subject to any count or value restrictions in `...`. Returns a logical scalar.
#' @export
D2D <- function(x, ...) {uj::DDD(x, 'd2d', ...)}

#' @describeIn ddd_prop_funs Checks `x` for three or more defined dimensions subject to any count or value restrictions in `...`. Returns a logical scalar.
#' @export
DHD <- function(x, ...) {uj::DDD(x, 'dHd', ...)}

#' @describeIn ddd_prop_funs Gets the defined dimensionality of `x`. Returns a non-negative integer scalar.
#' @export
nddd <- function(x) {if (base::is.null(x)) {0} else if (base::is.vector(x)) {1} else {base::length(base::dim(x))}}
