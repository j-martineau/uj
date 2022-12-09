
#' @family props
#' @title Extended class properties
#' @description Extended class properties are not formally defined, but are dynamically evaluated.
#' \cr\cr **Extended class property values**
#' \tabular{rll}{
#'       `arr`   \tab Array      \tab   Arrays.
#'   \cr         \tab            \tab   
#'   \cr `mat`   \tab Matrix     \tab   Matrices.
#'   \cr         \tab            \tab   
#'   \cr `dtf`   \tab Data.frame \tab   Data.frames.
#'   \cr         \tab            \tab   
#'   \cr `vls`   \tab Vlist      \tab   Vector-lists (non-data.frame lists).
#'   \cr         \tab            \tab   
#'   \cr `gen`   \tab Generic    \tab   Vectors, vlists, arrays.
#'   \cr         \tab            \tab   
#'   \cr `scl`   \tab Scalar     \tab   Length-`1`  vectors, vlists, arrays.
#'   \cr         \tab            \tab   
#'   \cr `mvc`   \tab Multivec   \tab   Length-`2+` vectors, vlists; \link[=ie1D]{effectively 1D} arrays.
#'   \cr         \tab            \tab   
#'   \cr `vec`   \tab Vec        \tab   Scalars and multivecs.
#' }
#' **Extended class property functions**
#' \tabular{rl}{
#'     `is_ccc_spec`   \tab Evaluates whether `spec` is a valid extended class property specification.
#'   \cr               \tab   
#'   \cr `ccc_props`   \tab Gets a character vector of all possible extended class property values.
#'   \cr               \tab   
#'   \cr      `ixxx`   \tab Evaluates whether `x` matches property `xxx`\eqn{^1} (subject to any restrictions in `...`).
#'   \cr               \tab   
#'   \cr      `iccc`   \tab Evaluates whether `x` matches one or more (possibly pipe-delimited) extended class properties in `spec` (subject to any restrictions in `...`).
#'   \cr               \tab   
#'   \cr       `ccc`   \tab Gets a character vector of all extended class properties matching `x`.
#' }
#' \eqn{^{1.}} An extended class property.
#' @param x An R object.
#' @param spec `NULL` or a \link[=cmp_chr_scl]{complete character vec} containing one or more extended class properties (i.e., from `ccc_props()`). Extended class properties may be pipe-delimited. If there are multiple properties in `spec`, `x` is inspected for a match to any of the specified properties.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return \tabular{rl}{
#'     `is_ccc_spec`   \tab A logical scalar.
#'   \cr `ccc_props`   \tab A character vector.
#'   \cr      `iccc`   \tab A logical scalar.
#'   \cr      `ixxx`   \tab A logical scalar\eqn{^2}.
#'   \cr       `ccc`   \tab A character scalar or vector.
#' }
#' \eqn{^{1.}} `xxx` is an extended class property.
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
