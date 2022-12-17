#' @family props
#' @title Extended class (`xclass`) properties
#' @description `xclass` properties are defined as follows:
#' \tabular{rll}{
#'       `arr`   \tab `array`    \tab   `arrays`.
#'   \cr `mat`   \tab `matrix`   \tab   `matrices`.
#'   \cr `dtf`   \tab `dtf`      \tab   `data.frames`.
#'   \cr `vls`   \tab `vlist`    \tab   `vector-lists`\eqn{^a}.
#'   \cr `gen`   \tab `generic`  \tab   `vectors`, `vlists`, and `arrays.`
#'   \cr `scl`   \tab `scalar`   \tab   Length-`1` `generics`
#'   \cr `mvc`   \tab `multivec` \tab   Length-`2+` \link[=ilin]{linear} `generics.`
#'   \cr `vec`   \tab `vec`      \tab   `scalars` and `multivecs.`
#' }
#' \eqn{^{a.}} Non-`data.frame` lists.
#' \cr\cr
#' **Functions**
#' \tabular{rl}{
#'     `is_ccc_spec`   \tab Is `spec` an `xclass` specification?
#'   \cr `ccc_props`   \tab What `xclass` properties are there?
#'   \cr  `ccc_funs`   \tab What `xclass` property functions are there?
#'   \cr      `iccc`   \tab Is `x` a match to the `xclass` specification `spec`?
#'   \cr      `iCCC`   \tab Is `x` a match to the single `xclass` property `'CCC'`?
#'   \cr       `ccc`   \tab What are `x`'s `xclass` properties?
#' }
#' @param x An R object.
#' @param spec `NULL` or a \link[=cmp_chr_scl]{complete character vec} containing one or more `xclass` properties (i.e., from `ccc_props()`). `xclass` properties may be pipe-delimited. If there are multiple properties in `spec`, `x` is inspected for a match to any of the specified properties.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return \tabular{rl}{
#'     `is_ccc_spec`   \tab A logical scalar.
#'   \cr `ccc_props`   \tab A character vector.
#'   \cr  `ccc_funs`   \tab A character vector.
#'   \cr      `iccc`   \tab A logical scalar.
#'   \cr      `iCCC`   \tab A logical scalar.
#'   \cr       `ccc`   \tab A character vector.
#' }
#' @examples
#' ccc_funs()
#' ccc_props()
#' is_ccc_spec("scl|vls")
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
ccc_funs <- function() {paste0("i", .cccs)}

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
