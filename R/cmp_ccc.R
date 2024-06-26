#' @encoding UTF-8
#' @family properties
#' @title Combo Completeness Extended Class Properties
#' @description Combinations of \link[=CMP]{completeness} and \link[=ccc]{extended class} properties.
#' @param x An R object.
#' @param ccc A character scalar single xclass property from \code{\link{ccc_props}()}.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return **A character vector** \cr\cr `cmp_ccc_funs`
#' \cr\cr  **A logical scalar**   \cr\cr `cmp_{ccc}, cmp_ccc`
#' @examples
#' cmp_ccc_funs()
#' cmp_ccc(letters, "mvc")
#' cmp_ccc(1, "scl")
#' cmp_ccc(NA, "gen")
#' cmp_mvc(letters)
#' cmp_scl(1)
#'
#' @export
cmp_ccc_help <- function() {utils::help("cmp_ccc_help", package = "uj")}

#' @describeIn cmp_ccc_help Checks `x` for completeness plus the extended class in `ccc` subject to count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ccc <- function(x, ccc, ...) {
  cfun <- function(cx) {uj::run("uj::.", base::toupper(ccc), "(cx)")}
  afun <- function(ax) {if (!base::is.atomic(ax)) {F} else if (base::length(ax) == 0) {F} else {!base::any(base::is.na(ax))}}
  dfun <- function(dx) {
    if (base::NROW(x) * base::NCOL(x) > 0) {
      for (i in 1:base::NCOL(x)) {if (!afun(dx[[i]])) {return(F)}}
      T
    } else {F}
  }
  vfun <- function(vx) {base::all(base::sapply(vx, afun))}
  if (base::is.character(ccc)) {ccc <- base::tolower(ccc)}
  errs <- uj::meets_errs(x, ...)
  if (!base::is.null(errs)) {uj::stopperr(errs)}
  if (!(ccc %in% uj::ccc_props())) {F}
  else if (!cfun(x)) {F}
  else if (ccc == "dtf") {dfun(x)}
  else if (ccc == "vls") {vfun(x)}
  else {afun(x)}
}

#' @describeIn cmp_ccc_help Lists all completeness plus extended class property checking functions. Returns a character vector.
#' @export
cmp_ccc_funs <- function() {base::paste0('cmp_', uj::ccc_props())}

#' @describeIn cmp_ccc_help Lists all completeness plus extended class properties.
#' @export
cmp_ccc_props <- function() {base::paste0('cmp_', uj::ccc_props())}

#' @describeIn cmp_ccc_help Checks `x` for completeness and array-ness subject to count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_arr <- function(x, ...) {uj::cmp_ccc(x, 'arr', ...)}

#' @describeIn cmp_ccc_help Checks `x` for completeness and data.frame-ness subject to count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_dtf <- function(x, ...) {uj::cmp_ccc(x, 'dtf', ...)}

#' @describeIn cmp_ccc_help Checks `x` for completeness and generic-ness subject to count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_gen <- function(x, ...) {uj::cmp_ccc(x, 'gen', ...)}

#' @describeIn cmp_ccc_help Checks `x` for completeness and matrix-ness subject to count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_mat <- function(x, ...) {uj::cmp_ccc(x, 'mat', ...)}

#' @describeIn cmp_ccc_help Checks `x` for completeness and multivec-ness subject to count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_mvc <- function(x, ...) {uj::cmp_ccc(x, 'mvc', ...)}

#' @describeIn cmp_ccc_help Checks `x` for completeness and scalar-ness subject to count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_scl <- function(x, ...) {uj::cmp_ccc(x, 'scl', ...)}

#' @describeIn cmp_ccc_help Checks `x` for completeness and vec-ness subject to count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_vec <- function(x, ...) {uj::cmp_ccc(x, 'vec', ...)}

#' @describeIn cmp_ccc_help Checks `x` for completeness and vector-list-ness subject to count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_vls <- function(x, ...) {uj::cmp_ccc(x, 'vls', ...)}
