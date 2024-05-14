#' @encoding UTF-8
#' @title Basic Properties
#' @description An object's basic properties are defined by its fundamental structure as follows:
#' \tabular{lll}{  `'atm', 'ATM'`   \tab `atomic-ness`      \tab Atomic (but not `NULL`). Both a basic and an \link[=mmm]{xmode} property.                                    \cr   \tab   \cr
#'                 `'fun', 'FUN'`   \tab `function-ness`    \tab A function or a character scalar name of a function accessible from the environment of the calling function. \cr   \tab   \cr
#'                 `'rcr', 'RCR'`   \tab `recursive-ness`   \tab data.frame or list.                                                                                                       \cr
#'                 `'pop', 'POP'`   \tab `populated-ness`   \tab Length `1` or greater.                                                                                                    \cr
#'                 `'def', 'DEF'`   \tab `defined-ness`     \tab Not `NULL`.                                                                                                               \cr
#'                 `'nll', 'NLL'`   \tab `null-ness`        \tab `NULL`.                                                                                                                   \cr
#'                 `'nil', 'NIL'`   \tab `nil-ness`         \tab Length `0` but not `NULL`.                                                                                                  }
#' @param x An R object.
#' @param spec `NULL` or a \link[=cmp_chr_scl]{complete character vec} containing one or more basic properties from `bbb_props()`. basic properties may be pipe-delimited. If there are multiple properties in `spec`, `x` is inspected for a match to any of the specified properties.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @examples
#' bbb_funs()
#' bbb_props()
#' is_bbb_spec("nil|nll")
#' BBB(NULL, "nil|nll")
#' POP(NA)
#' ATM(list(letters))
#' bbb(NA)
#' @export
bbb_prop_funs <- function() {utils::help("bbb_prop_funs", package = "uj")}

#' @describeIn bbb_prop_funs Lists all basic properties of `x`. Returns a sorted, lowercase, character vector.
#' @export
bbb <- function(x) {
  y <- NULL
  for (BBB in uj::bbb_funs()) {
    Match <- uj::run("uj::.", BBB, "(x)")
    if (Match) {y <- base::c(y, base::tolower(BBB))}
  }
  y
}

#' @describeIn bbb_prop_funs Lists all basic-property-checking functions. Returns a sorted, uppercase, character vector.
#' @export
bbb_funs <- function() {base::c("ATM", "DEF", "FUN", "NIL", "NLL", "POP", "RCR")}

#' @describeIn bbb_prop_funs Lista all basic properties. Returns a sorted, lowercase, character vector.
#' @export
bbb_props <- function() {base::c("atm", "def", "fin", "nil", "nll", "pop", "rcr")}

#' @describeIn bbb_prop_funs Checks whether `spec` is a basic property spec. Returns a logical scalar. See \code{\link{ppp}} for a definition of a property spec.
#' @export
is_bbb_spec <- function(spec) {
  spec <- uj::spec2props(spec)
  if (base::length(spec) == 0) {F} else {base::all(spec %in% uj::bbb_props())}
}

#' @describeIn bbb_prop_funs Checks `x` against the basic property spec `spec` subject to any count or value restrictions in `...`. Returns a logical scalar. See \code{\link{ppp}} for a definition of a property spec.
#' @export
BBB <- function(x, spec, ...) {
  errs <- uj::meets_errs(x, ...)
  if (!uj::is_bbb_spec(spec)) {errs <- base::c(errs, '[spec] must be a complete character vec (?cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from bbb_props().')}
  if (!base::is.null(errs)) {uj::stopperr(errs, )}
  if (uj::meets(x, ...)) {
    props <- base::toupper(uj::spec2props(spec))
    for (prop in props) {if (uj::run("uj::.", prop, "(x)")) {return(T)}}
  }
  F
}

#' @describeIn bbb_prop_funs Checks `x` atomic-ness subject to any count or value restrictions in `...`. Returns a logical scalar.
#' @export
ATM <- function(x, ...) {uj::BBB(x, 'atm', ...)}

#' @describeIn bbb_prop_funs Checks `x` for defined-ness subject to any count or value restrictions in `...`. Returns a logical scalar.
#' @export
DEF <- function(x, ...) {uj::BBB(x, 'def', ...)}

#' @describeIn bbb_prop_funs Checks `x` for function-ness subject to any count or value restrictions in `...`. Returns a logical scalar.
#' @export
FUN <- function(x, ...) {uj::BBB(x, 'fun', ...)}

#' @describeIn bbb_prop_funs Checks `x` ni-lness subject to any count or value restrictions in `...`. Returns a logical scalar.
#' @export
NIL <- function(x) {uj::BBB(x, 'nil')}

#' @describeIn bbb_prop_funs Checks `x` for null-ness subject to any count or value restrictions in `...`. Returns a logical scalar.
#' @export
NLL <- function(x) {uj::BBB(x, 'nll')}

#' @describeIn bbb_prop_funs Checks `x` for populated-ness subject to any count or value restrictions in `...`. Returns a logical scalar.
#' @export
POP <- function(x, ...) {uj::BBB(x, 'pop', ...)}

#' @describeIn bbb_prop_funs Checks `x` for recursive-ness subject to any count or value restrictions in `...`. Returns a logical scalar.
#' @export
RCR <- function(x, ...) {uj::BBB(x, 'rcr', ...)}
