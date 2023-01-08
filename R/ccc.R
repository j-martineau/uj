#' @encoding UTF-8
#' @family props
#' @title Extended class (xclass) properties
#' @description `xclass` properties are defined as follows:
#' \tabular{rll}{
#'       `arr`   \tab array    \tab   arrays
#'   \cr `mat`   \tab matrix   \tab   matrices
#'   \cr `dtf`   \tab dtf      \tab   data.frames
#'   \cr `vls`   \tab vlist    \tab   vector-lists`*`
#'   \cr `gen`   \tab generic  \tab   vectors, vlists, and arrays
#'   \cr `scl`   \tab scalar   \tab   Length-`1` generics
#'   \cr `mvc`   \tab multivec \tab   Length-`2+` \link[=ilin]{linear} generics
#'   \cr `vec`   \tab vec      \tab   scalars and multivecs
#' }
#' `*` Non-`data.frame` lists.
#' \cr
#' \cr
#' **Functions**
#' \tabular{rl}{
#'     `is_ccc_spec`   \tab Is `spec` an xclass specification?
#'   \cr               \tab  
#'   \cr `ccc_props`   \tab What xclass properties are there?
#'   \cr               \tab  
#'   \cr  `ccc_funs`   \tab What xclass property functions are there?
#'   \cr               \tab  
#'   \cr      `iccc`   \tab Is `x` a match to the xclass specification `spec`?
#'   \cr               \tab  
#'   \cr      `iCCC`   \tab Is `x` a match to the single xclass property `'CCC'`?
#'   \cr               \tab  
#'   \cr       `ccc`   \tab What are `x`'s xclass properties?
#' }
#' @param x An R object.
#' @param spec `NULL` or a \link[=cmp_chr_scl]{complete character vec} containing one or more xclass properties (i.e., from \code{\link{ccc_props}()}). Properties may be pipe-delimited. If there are multiple properties in `spec`, `x` is inspected for a match to any of the specified properties.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return *A character vector*
#'  \cr   `ccc_props`
#'  \cr   `ccc_funs`
#'  \cr   `ccc`
#'  \cr
#'  \cr *A logical vector*
#'  \cr   `is_ccc_spec`
#'  \cr   `iCCC`
#'  \cr   `iccc`
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
  for (C in uj:::.cccs) {out <- base::c(out, uj::f0(uj::run('uj:::.i', C, '(x)'), C, NULL))}
  out
}

#' @rdname ccc
#' @export
ccc_funs <- function() {base::paste0("i", uj:::.cccs)}

#' @rdname ccc
#' @export
ccc_props <- function() {uj:::.cccs}

#' @rdname ccc
#' @export
is_ccc_spec <- function(spec) {spec <- uj:::.spec_vals(spec); f0(base::length(spec) == 0, F, base::all(spec %in% .cccs))}

#' @rdname ccc
#' @export
iccc <- function(x, spec, ...) {
  errs <- base::c(uj:::.meets_errs(x, ...), uj::f0(uj::is_ccc_spec(spec), NULL, '[spec] must be a complete character vec (?cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from ccc_props().'))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  if (!uj::meets(x, ...)) {return(F)}
  for (prop in uj:::.spec_vals(spec)) {if (uj::run('uj:::.i', prop, '(x)')) {return(T)}}
  F
}

#' @rdname ccc
#' @export
iarr <- function(x, ...) {uj::iccc(x, 'arr', ...)}

#' @rdname ccc
#' @export
idtf <- function(x, ...) {uj::iccc(x, 'dtf', ...)}

#' @rdname ccc
#' @export
igen <- function(x, ...) {uj::iccc(x, 'gen', ...)}

#' @rdname ccc
#' @export
imat <- function(x, ...) {uj::iccc(x, 'mat', ...)}

#' @rdname ccc
#' @export
imvc <- function(x, ...) {uj::iccc(x, 'mvc', ...)}

#' @rdname ccc
#' @export
iscl <- function(x, ...) {uj::iccc(x, 'scl', ...)}

#' @rdname ccc
#' @export
ivec <- function(x, ...) {uj::iccc(x, 'vec', ...)}

#' @rdname ccc
#' @export
ivls <- function(x, ...) {uj::iccc(x, 'vls', ...)}
