#' @encoding UTF-8
#' @family properties
#' @title Integrity properties
#' @description Integrity properties are defined for \link[=ipop]{populated} \link[=atm_dtf]{atomic data.frameS}, populated \link[=atm_vls]{atomic vlists}, populated atomic vectors, and populated atomic arrays. For all others, all integrity properties are considered `FALSE`. The following table summarizes valid integrity properties.
#' \tabular{rll}{
#'       `'cmp'`   \tab complete      \tab Populated and atomic vectors, arrays, \link[=atm_dtf]{data.frames}, and \link[=atm_vls]{vlists} containing *no* `NA` values.
#'   \cr `'mss'`   \tab missing       \tab Populated and atomic vectors, arrays, data.frames, and vlists containing *only* `NA` values.
#'   \cr `'prt'`   \tab partial       \tab Populated and atomic vectors, arrays, data.frames, and vlists containing `NA` *and* non-`NA` values.
#'   \cr `'dup'`   \tab duplicates    \tab Complete and containing duplicate values.
#'   \cr `'unq'`   \tab unique        \tab Complete and containing only unique values.
#'   \cr `'nas'`   \tab `NA` scalar   \tab Atomic scalar `NA`.
#'   \cr `'oks'`   \tab `OK` scalar   \tab Non-`NA` atomic scalar.
#' }
#' *Integrity property functions*
#' \tabular{rl}{
#'     `is_iii_spec`   \tab Is `spec` an integrity specification?
#'   \cr `iii_props`   \tab What integrity properties are there?
#'   \cr  `iii_funs`   \tab What integrity property functions are there?
#'   \cr      `iiii`   \tab Does `x` match integrity specification `spec`?
#'   \cr      `iIII`   \tab Does `x` match single integrity property `'III'`?
#'   \cr       `iii`   \tab Gets all of `x`'s integrity properties.
#' }
#' @param x An R object.
#' @param spec `NULL` or a \link[=cmp_chr_scl]{complete character vec} containing one or more integrity properties (i.e., from `iii_props()`). Integrity properties may be pipe-delimited. If there are multiple properties in `spec`, `x` is inspected for a match to any of the specified properties.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return *A character vector* \cr   `iii_props, iii_funs, iii`
#'  \cr\cr *A logical scalar* \cr   `is_iii, iIII, iiii`
#' @examples
#' iii_funs()
#' iii_props()
#' is_iii_spec("nas|mss")
#' icmp(NA)
#' icmp(NULL)
#' inas(NA)
#' ioks(letters)
#' ioks(letters[1])
#' imss(rep(NA, 10))
#' iii(NA)
#' iii(letters)
#' iii(letters[1])
#' @export
iii <- function(x) {
  out <- NULL
  for (i in uj:::.iiis) {out <- base::c(out, uj::f0(uj::run('.i', i, '(x)'), i, NULL))}
  out
}

#' @rdname iii
#' @export
iii_funs <- function() {base::paste0("i", uj:::.iiis)}

#' @rdname iii
#' @export
iii_props <- function() {uj:::.iiis}

#' @rdname iii
#' @export
is_iii_spec <- function(spec) {spec <- uj:::.spec_vals(spec); f0(base::length(spec) == 0, F, base::all(spec %in% uj:::.iiis))}

#' @rdname iii
#' @export
iiii <- function(x, spec, ...) {
  errs <- base::c(uj:::.meets_errs(x, ...), uj::f0(uj::is_iii_spec(spec), NULL, '[spec] must be a complete character vec (?cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from iii_props().'))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  if (!uj::meets(x, ...)) {return(F)}
  for (prop in uj:::.spec_vals(spec)) {if (uj::run('uj:::.i', prop, '(x)')) {return(T)}}
  F
}

#' @rdname iii
#' @export
icmp <- function(x, ...) {uj::iiii(x, 'cmp', ...)}

#' @rdname iii
#' @export
imss <- function(x, ...) {uj::iiii(x, 'mss', ...)}

#' @rdname iii
#' @export
inas <- function(x, ...) {uj::iiii(x, 'nas', ...)}

#' @rdname iii
#' @export
ioks <- function(x, ...) {uj::iiii(x, 'oks', ...)}

#' @rdname iii
#' @export
iprt <- function(x, ...) {uj::iiii(x, 'prt', ...)}

#' @rdname iii
#' @export
idup <- function(x, ...) {uj::iiii(x, 'dup', ...)}

#' @rdname iii
#' @export
iunq <- function(x, ...) {uj::iiii(x, 'unq', ...)}
