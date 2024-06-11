#' @encoding UTF-8
#' @family properties
#' @title Extended class properties
#' @description Extended class properties are defined as follows:
#' \tabular{lll}{  `'arr', 'ARR'`   \tab array      \tab arrays                                   \cr
#'                 `'mat', 'MAT'`   \tab matrix     \tab matrices                                 \cr
#'                 `'dtf', 'DTF'`   \tab dtf        \tab data.frames                              \cr
#'                 `'vls', 'VLS'`   \tab vlist      \tab vector-lists  \eqn{^{(1)}}               \cr
#'                 `'gen', 'GEN'`   \tab generic    \tab vectors, vlists, and arrays              \cr
#'                 `'scl', 'SCL'`   \tab scalar     \tab length-`1` generics                      \cr
#'                 `'mvc', 'MVC'`   \tab multivec   \tab length-`2+` \link[=LIN]{linear} generics \cr
#'                 `'vec', 'VEC'`   \tab vec        \tab scalars and multivecs                      }
#'   \tabular{l}{  \eqn{^{(1)}} Non-`data.frame` lists. }
#' @param x An R object.
#' @param spec `NULL` or a \link[=cmp_chr_scl]{complete character vec} containing one or more xclass properties (i.e., from \code{\link{ccc_props}()}). Properties may be pipe-delimited. If there are multiple properties in `spec`, `x` is inspected for a match to any of the specified properties.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @examples
#' ccc_funs()
#' ccc_props()
#' is_ccc_spec("scl|vls")
#' CCC(letters, "vec|dtf")
#' VEC(letters)
#' ccc(letters)
#' @export
ccc_help <- function() {utils::help("ccc_help", package = "uj")}

#' @describeIn ccc_help Lists all extended class properties of `x`. Returns a sorted, lowercase, character vector.
#' @export
ccc <- function(x) {
  y <- NULL
  for (CCC in uj::ccc_funs()) {
    match <- uj::run("uj::.", CCC, "(x)")
    if (match) {y <- base::c(y, base::tolower(CCC))}
  }
  y
}

#' @describeIn ccc_help Lists all extended class property checking functions. Returns a sorted, uppercase, character vector.
#' @export
ccc_funs <- function() {base::c("ARR", "DTF", "GEN", "MAT", "MVC", "SCL", "VEC", "VLS")}

#' @describeIn ccc_help Lists all extended class properties. Returns a sorted, lowercase, character vector.
#' @export
ccc_props <- function() {base::c("arr", "dtf", "gen", "mat", "mvc", "scl", "vec", "vls")}

#' @describeIn ccc_help Checks whether `spec` is an extended class property spec. Returns a logical scalar. See \code{\link{ppp}} for a definition of a property spec.
#' @export
is_ccc_spec <- function(spec) {
  spec <- uj::spec2props(spec)
  if (base::length(spec) == 0) {F}
  else {base::all(spec %in% uj::ccc_props())}
}

#' @describeIn ccc_help Checks `x` against the extended class property spec `spec`. Returns a logical scalar. See \code{\link{ppp}} for a definition of a property spec.
#' @export
CCC <- function(x, spec, ...) {
  errs <- uj::meets_errs(x, ...)
  if (!uj::is_ccc_spec(spec)) {errs <- base::c(errs, '[spec] must be a complete character vec (?cmp_chr_vec) containing (possible pipe-separated) values from ccc_props().')}
  if (!base::is.null(errs)) {uj::stopperr(errs)}
  if (uj::meets(x, ...)) {
    props <- base::toupper(uj::spec2props(spec))
    for (prop in props) {if (uj::run("uj::.", prop, "(x)")) {return(T)}}
  }
  F
}

#' @describeIn ccc_help Checks `x` for array-ness. Returns a logical scalar.
#' @export
ARR <- function(x, ...) {uj::CCC(x, 'arr', ...)}

#' @describeIn ccc_help Checks `x` for dtf-ness. Returns a logical scalar.
#' @export
DTF <- function(x, ...) {uj::CCC(x, 'dtf', ...)}

#' @describeIn ccc_help Checks `x` for generic-ness. Returns a logical scalar.
#' @export
GEN <- function(x, ...) {uj::CCC(x, 'gen', ...)}

#' @describeIn ccc_help Checks `x` for matrix-ness. Returns a logical scalar.
#' @export
MAT <- function(x, ...) {uj::CCC(x, 'mat', ...)}

#' @describeIn ccc_help Checks `x` for multivec-ness. Returns a logical scalar.
#' @export
MVC <- function(x, ...) {uj::CCC(x, 'mvc', ...)}

#' @describeIn ccc_help Checks `x` for scalar-ness. Returns a logical scalar.
#' @export
SCL <- function(x, ...) {uj::CCC(x, 'scl', ...)}

#' @describeIn ccc_help Checks `x` for vec-ness. Returns a logical scalar.
#' @export
VEC <- function(x, ...) {uj::CCC(x, 'vec', ...)}

#' @describeIn ccc_help Checks `x` for vector-list-ness. Returns a logical scalar.
#' @export
VLS <- function(x, ...) {uj::CCC(x, 'vls', ...)}
