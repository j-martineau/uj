.iarr <- function(x) {is.array(x)}
.idtf <- function(x) {is.data.frame(x)}
.igen <- function(x) {is.array(x) | is.vector(x)}
.imat <- function(x) {is.matrix(x)}
.imvc <- function(x) {f0(length(x) < 2, F, f0(is.vector(x), T, is.array(x) & length(which(dim(x) > 1)) == 1))}
.iscl <- function(x) {f0(length(x) != 1, F, is.array(x) | is.vector(x))}
.ivec <- function(x) {f0(length(x) == 0, F, f0(is.vector(x), T, is.array(x) & length(which(dim(x) > 1)) < 2))}
.ivls <- function(x) {is.list(x) & !is.data.frame(x)}
.cccs <- c("arr", "dtf", "gen", "mat", "mvc", "scl", "vec", "vls")

#' @name ccc
#' @family props
#' @title Extended class properties
#' @description Extended class properties are not formally defined, but are dynamically evaluated for properties as follows:
#' \itemize{
#'   \item **`arr`** (array): arrays.
#'   \item **`dtf`** (data.frame): data frames.
#'   \item **`vls`** (vlist): vector-lists (not data.frame lists).
#'   \item **`gen`** (generic): vectors, vlists, and arrays.
#'   \item **`mat`** (matrix): matrices.
#'   \item **`scl`** (scalar): vectors, vlists, and arrays of length `1`.
#'   \item **`mvc`** (multivec): vectors and vlists of length `2+` and \link[=ie1D]{effectively 1D} arrays.
#'   \item **`vec`** (vec): scalars and multivecs
#' }
#' Functions in this family are:
#' \itemize{
#'   \item **`ccc`**: gets a character vector containing all extended class properties possessed by `x`.
#'   \item **`ixxx`**: evaluates whether `x` possesses the extended class property `xxx`** (a placeholder for any given extended class property value), subject to any restrictions in `...`.
#'   \item **`iccc`**: evaluates whether `x` possesses one or more (possibly pipe-delimited) extended class properties in `spec`, subject to any restrictions in `...`.
#'   \item **`ccc_props`**: gets a character vector of all possible extended class property values.
#'   \item **`is_ccc_spec`**: evaluates whether `spec` is a valid extended class property specification.
#' }
#' @param x An R object.
#' @param spec `NULL` or a \link[=cmp_chr_scl]{complete character vec} containing one or more extended class properties (i.e., from `ccc_props()`). Extended class properties may be pipe-delimited. If there are multiple properties in `spec`, `x` is inspected for a match to any of the specified properties.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return \itemize{
#'   \item **`ccc`**: a character scalar/vector.
#'   \item **`ccc_vals`**: a character vector.
#'   \item **`iccc, ixxx, is_ccc_spec`**: a logical scalar.
#' }
#' @export
ccc <- function(x) {
  out <- NULL
  for (C in .cccs) {out <- c(out, f0(run('.i', C, '(x)'), C, NULL))}
  out
}

#' @rdname ccc
#' @export
ccc_props <- function() {.cccs()}

#' @rdname ccc
#' @export
is_ccc_spec <- function(spec) {spec <- .spec_vals(spec); f0(length(spec) == 0, F, all(spec %in% .cccs))}

#' @rdname ccc
#' @export
iccc <- function(x, spec, ...) {
  errs <- c(.meets_errs(x, ...), f0(is_ccc_spec(spec), NULL, '\n \u2022 [spec] must be a complete character vec (?cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from ccc_props().'))
  if (!is.null(errs)) {stop(errs)}
  if (!meets(x, ...)) {return(F)}
  for (prop in .spec_vals(spec)) {if (run('.i', prop, '(x)')) {return(T)}}
  F
}

#' @rdname ccc
#' @export
iarr <- function(x, ...) {iccc(x, 'arr', ...)}

#' @rdname ccc
#' @export
idtf <- function(x, ...) {iccc(x, 'dtf', ...)}

#' @rdname ccc
#' @export
igen <- function(x, ...) {iccc(x, 'gen', ...)}

#' @rdname ccc
#' @export
imat <- function(x, ...) {iccc(x, 'mat', ...)}

#' @rdname ccc
#' @export
imvc <- function(x, ...) {iccc(x, 'mvc', ...)}

#' @rdname ccc
#' @export
iscl <- function(x, ...) {iccc(x, 'scl', ...)}

#' @rdname ccc
#' @export
ivec <- function(x, ...) {iccc(x, 'vec', ...)}

#' @rdname ccc
#' @export
ivls <- function(x, ...) {iccc(x, 'vls', ...)}
