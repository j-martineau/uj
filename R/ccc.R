#' @family props
#' @title Extended class (`exclass`) properties
#' @description NOTE: `exclass`es are dynamically evaluated rather than formally defined.
#' \cr\cr **exclass properties**
#' \tabular{rll}{
#'       `arr`   \tab array    \tab   Arrays.
#'   \cr `mat`   \tab matrix   \tab   Matrices.
#'   \cr `dtf`   \tab dtf      \tab   Data.frames.
#'   \cr `vls`   \tab vlist    \tab   Vector-lists\eqn{^1}.
#'   \cr `gen`   \tab generic  \tab   Vectors, vlists, & arrays.
#'   \cr `scl`   \tab scalar   \tab   Length-`1` generics
#'   \cr `mvc`   \tab multivec \tab   Length-`2+`, \link[=ilin]{linear} generics.
#'   \cr `vec`   \tab vec      \tab   Scalars, multivecs.
#' }
#' \eqn{^{1.}} i.e., a non-data.frame list.
#' \cr\cr **exclass functions**
#' \tabular{rl}{
#'     `is_ccc_spec`   \tab Is `spec` is an `exclass` spec?
#'   \cr `ccc_props`   \tab Gets all possible `exclass` values.
#'   \cr      `iCCC`   \tab Does `x` match `exclass` property `CCC`\eqn{^2}?
#'   \cr      `iccc`   \tab Does `x` match `exclass` spec `spec`\eqn{^2}?
#'   \cr       `ccc`   \tab Gets all `exclass` properties matching `x`.
#' }
#' \eqn{^{2.}} subject to any restrictions in `...`.
#' @param x An R object.
#' @param spec `NULL` or a \link[=cmp_chr_scl]{complete character vec} containing one or more `exclass` properties (i.e., from `ccc_props()`). `exclass` properties may be pipe-delimited. If there are multiple properties in `spec`, `x` is inspected for a match to any of the specified properties.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return \tabular{rl}{
#'     `is_ccc_spec`   \tab A logical
#'   \cr      `iCCC`   \tab scalar.
#'   \cr      `iccc`   \tab   
#'   \cr               \tab   
#'   \cr `ccc_props`   \tab A character
#'   \cr       `ccc`   \tab vector.
#' }
#' @examples
#' is_ccc_spec("scl|vls")
#' ccc_props()
#' iccc(letters, "vec|dtf")
#' ivec(letters)
#' ccc(letters)
#' @export
ccc <- function(x) {
  out <- NULL
  for (C in .cccs) {out <- c(out, f0(run('.i', C, '(x)'), C, NULL))}
  out
}

#' @rdname ccc
#' @export
ccc_props <- function() {.cccs}

#' @rdname ccc
#' @export
is_ccc_spec <- function(spec) {spec <- .spec_vals(spec); f0(length(spec) == 0, F, all(spec %in% .cccs))}

#' @rdname ccc
#' @export
iccc <- function(x, spec, ...) {
  errs <- c(.meets_errs(x, ...), f0(is_ccc_spec(spec), NULL, '[spec] must be a complete character vec (?cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from ccc_props().'))
  if (!is.null(errs)) {stop(.errs(errs))}
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
