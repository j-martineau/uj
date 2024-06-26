#' @encoding UTF-8
#' @family properties
#' @title Integrity properties
#' @description Integrity properties are defined for \link[=POP]{populated} \link[=atm_dtf]{atomic data.frames}, populated \link[=atm_vls]{atomic vlists}, populated atomic vectors, and populated atomic arrays. For all others, all integrity properties are considered `FALSE`. The following table summarizes valid integrity properties.
#' \tabular{lll}{  `'cmp', 'CMP'`   \tab complete      \tab Populated and atomic vectors, arrays, \link[=atm_dtf]{data.frames}, and \link[=atm_vls]{vlists} containing *no* `NA` values. \cr   \tab   \cr
#'                 `'mss', 'MSS'`   \tab missing       \tab Populated and atomic vectors, arrays, data.frames, and vlists containing *only* `NA` values.                                 \cr   \tab   \cr
#'                 `'prt', 'PRT'`   \tab partial       \tab Populated and atomic vectors, arrays, data.frames, and vlists containing `NA` *and* non-`NA` values.                         \cr   \tab   \cr
#'                 `'dup', 'DUP'`   \tab duplicates    \tab Complete and containing duplicate values.                                                                                    \cr   \tab   \cr
#'                 `'unq', 'UNQ'`   \tab unique        \tab Complete and containing only unique values.                                                                                  \cr   \tab   \cr
#'                 `'nas', 'NA0'`   \tab `NA` scalar   \tab Atomic scalar `NA`.                                                                                                          \cr   \tab   \cr
#'                 `'oks', 'OK0'`   \tab `OK` scalar   \tab Non-`NA` atomic scalar.                                                                                                                     }
#' @param x An R object.
#' @param spec `NULL` or a \link[=cmp_chr_scl]{complete character vec} containing one or more integrity properties (i.e., from `iii_props()`). Integrity properties may be pipe-delimited. If there are multiple properties in `spec`, `x` is inspected for a match to any of the specified properties.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @examples
#' iii_funs()
#' iii_props()
#' is_iii_spec("nas|mss")
#' CMP(NA)
#' CMP(NULL)
#' NA0(NA)
#' OK0(letters)
#' OK0(letters[1])
#' MSS(rep(NA, 10))
#' iii(NA)
#' iii(letters)
#' iii(letters[1])
#' @export
iii_help <- function() {utils::help("iii_help", package = "uj")}

#' @describeIn iii_help Lists all integrity properties of `x`. Returns a sorted, lowercase, character vector.
#' @export
iii <- function(x) {
  Y <- NULL
  for (III in uj::iii_funs()) {if (uj::run("uj::.", III, "(x)")) {Y <- base::c(Y, base::tolower(III))}}
  Y
}

#' @describeIn iii_help Lists all integrity property checking functions. Returns a sorted, uppercase, character vector.
#' @export
iii_funs <- function() {base::c("CMP", "DUP", "MSS", "NA0", "OK0", "PRT", "UNQ")}

#' @describeIn iii_help Lists all integrity properties. Returns a sorted, lowercase, character vector.
#' @export
iii_props <- function() {base::c("cmp", "dup", "mss", "na0", "ok0", "prt", "unq")}

#' @describeIn iii_help Checks whether `spec` is an integrity property spec. Returns a logical scalar. See \code{\link{ppp}} for a definition of a property spec.
#' @export
is_iii_spec <- function(spec) {
  spec <- uj::spec2props(spec)
  if (base::length(spec) == 0) {F} else {base::all(spec %in% uj::iii_props())}
}

#' @describeIn iii_help Checks `x` against the integrity property spec `spec`. Returns a logical scalar. See \code{\link{ppp}} for a definition of a property spec.
#' @export
III <- function(x, spec, ...) {
  errs <- uj::meets_errs(x, ...)
  if (!uj::is_iii_spec(spec)) {errs <- base::c(errs, '[spec] must be a complete character vec (?cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from iii_props().')}
  if (!base::is.null(errs)) {uj::stopperr(errs)}
  for (prop in base::toupper(uj::spec2props(spec))) {if (uj::run('uj::.', prop, '(x)')) {return(T)}}
  F
}

#' @describeIn iii_help Checks `x` for completeness subject to any count or value restrictions in `...`. Returns a logical scalar.
#' @export
CMP <- function(x, ...) {uj::III(x, 'cmp', ...)}

#' @describeIn iii_help Checks `x` for missingness subject to any count or value restrictions in `...`. Returns a logical scalar.
#' @export
MSS <- function(x, ...) {uj::III(x, 'mss', ...)}

#' @describeIn iii_help Checks `x` for whether it is a missing-value scalar subject to any count or value restrictions in `...`. Returns a logical scalar.
#' @export
NA0 <- function(x, ...) {uj::III(x, 'na0', ...)}

#' @describeIn iii_help Checks `x` for whether it is a non missing-value scalar subject to any count or value restrictions in `...`. Returns a logical scalar.
#' @export
OK0 <- function(x, ...) {uj::III(x, 'ok0', ...)}

#' @describeIn iii_help Checks `x` for partial completeness subject to any count or value restrictions in `...`. Returns a logical scalar.
#' @export
PRT <- function(x, ...) {uj::III(x, 'prt', ...)}

#' @describeIn iii_help Checks `x` for duplicatedness subject to any count or value restrictions in `...`. Returns a logical scalar.
#' @export
DUP <- function(x, ...) {uj::III(x, 'dup', ...)}

#' @describeIn iii_help Checks `x` for uniqueness subject to any count or value restrictions in `...`. Returns a logical scalar.
#' @export
UNQ <- function(x, ...) {uj::III(x, 'unq', ...)}
