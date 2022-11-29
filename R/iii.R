.icmp <- function(x) {f0(length(x) == 0 | !is.atomic(x), F, !any(is.na(x)))}
.imss <- function(x) {f0(length(x) == 0 | !is.atomic(x), F, all(is.na(x)))}
.inas <- function(x) {f0(length(x) != 1 | !is.atomic(x), F, is.na(x))}
.ioks <- function(x) {f0(length(x) != 1 | !is.atomic(x), F, !is.na(x))}
.iprt <- function(x) {f0(length(x) < 2 | !is.atomic(x), F, {x <- is.na(x); any(x) & !all(x)})}
.iiis <- c("cmp", "mss", "nas", "oks", "prt")

#' @name iii
#' @family props
#' @title Integrity (completeness) properties
#' @description Integrity properties are defined for \link[=ipop]{populated} \link[=atm_dtf]{atomic data.frame}, populated \link[=atm_vls]{atomic vlists}, populated atomic vectors, and populated atomic arrays. For all others, all integrity properties are considered \code{FALSE}. The following table summarizes valid integrity properties.
#' \itemize{
#'   \item **`'cmp'`** (complete): populated and atomic vectors, arrays, \link[=atm_dtf]{data.frames}, and \link[=atm_vls]{vlists} containing **no** `NA` values.
#'   \item **`'mss'`** (missing): populated and atomic vectors, arrays, \link[=atm_dtf]{data.frames}, and \link[=atm_vls]{vlists} containing **only** `NA` values.
#'   \item **`'prt'`** (partial): populated and atomic vectors, arrays, \link[=atm_dtf]{data.frames}, and \link[=atm_vls]{vlists} containing **both** `NA` and non-`NA` values.
#'   \item **`'nas'`** (`NA` scalar): atomic scalar `NA`.
#'   \item **`'oks'`** (OK scalar): non-`NA` atomic scalar.
#' }
#' **Functions**
#' \itemize{
#'   \item **`iii`**: gets a character vector containing all integrity properties possessed by `x`.
#'   \item **`ixxx`**: evaluates whether `x` possesses the integrity property `xxx` (a placeholder for any given integrity property value) subject to any restrictions in `...`.
#'   \item **`iiii`**: evaluates whether `x` possesses one or more (possibly pipe-delimited) integrity properties in `spec` subject to any restrictions in `...`.
#'   \item **`iii_props`**: gets a character vector of all possible integrity property values.
#'   \item **`is_iii_spec`**: evaluates whether `spec` is a valid integrity property specification.
#' }
#' @param x An R object.
#' @param spec `NULL` or a \link[=cmp_chr_scl]{complete character vec} containing one or more integrity properties (i.e., from `iii_props()`). Integrity properties may be pipe-delimited. If there are multiple properties in `spec`, `x` is inspected for a match to any of the specified properties.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return \itemize{
#'   \item **`iii`**: a character scalar/vector.
#'   \item **`iii_vals`**: a character vector.
#'   \item **`iiii, ixxx, is_iii_spec`**: a logical scalar.
#' }
#' @export
iii <- function(x) {
  out <- NULL
  for (i in .iiis) {out <- c(out, f0(run('.i', i, '(x)'), i, NULL))}
  out
}

#' @rdname iii
#' @export
iii_props <- function() {.iiis}

#' @rdname iii
#' @export
is_iii_spec <- function(spec) {spec <- .spec_vals(spec); f0(length(spec) == 0, F, all(spec %in% .iiis))}

#' @rdname iii
#' @export
iiii <- function(x, spec, ...) {
  errs <- c(.meets_errs(x, ...), f0(is_iii_spec(spec), NULL, '\n \u2022 [spec] must be a complete character vec (?cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from iii_props().'))
  if (!is.null(errs)) {stop(errs)}
  if (!meets(x, ...)) {return(F)}
  for (prop in .spec_vals(spec)) {if (run('.i', prop, '(x)')) {return(T)}}
  F
}

#' @rdname iii
#' @export
icmp <- function(x, ...) {iiii(x, 'cmp', ...)}

#' @rdname iii
#' @export
imss <- function(x, ...) {iiii(x, 'mss', ...)}

#' @rdname iii
#' @export
inas <- function(x, ...) {iiii(x, 'nas', ...)}

#' @rdname iii
#' @export
ioks <- function(x, ...) {iiii(x, 'oks', ...)}

#' @rdname iii
#' @export
iprt <- function(x, ...) {iiii(x, 'prt', ...)}
