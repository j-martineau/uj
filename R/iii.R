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
#' \cr\cr **Integrity property functions**
#' \tabular{ll}{  `is_iii_spec`   \tab Is `spec` an integrity specification?                                                                \cr   \tab   \cr
#'                `iii_props`     \tab What integrity properties are there?                                                                 \cr   \tab   \cr
#'                `iii_funs`      \tab What integrity property functions are there?                                                         \cr   \tab   \cr
#'                `iii`           \tab Gets all of `x`'s integrity properties.                                                              \cr   \tab   \cr
#'                `III`           \tab Does `x` match integrity specification `spec`?                                                       \cr   \tab   \cr
#'                `{II}`          \tab Does `x` match single integrity property `'{III}'` where `{III}` is a placeholder for any given integrity property? }
#' @param x An R object.
#' @param spec `NULL` or a \link[=cmp_chr_scl]{complete character vec} containing one or more integrity properties (i.e., from `iii_props()`). Integrity properties may be pipe-delimited. If there are multiple properties in `spec`, `x` is inspected for a match to any of the specified properties.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return **A character vector** \cr\cr   `iii_props, iii_funs, iii`
#' \cr\cr  **A logical scalar**   \cr\cr   `is_iii_spec, III, {III}`
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
iii <- function(x) {
  y <- NULL
  for (III in uj:::.III) {y <- base::c(y, uj::f0(uj::run("uj:::.", III, '(x)'), III, NULL))}
  y
}

#' @rdname iii
#' @export
iii_funs <- function() {uj:::.III}

#' @rdname iii
#' @export
iii_props <- function() {uj:::.iii}

#' @rdname iii
#' @export
is_iii_spec <- function(spec) {
  spec <- uj:::.spec2props(spec)
  f0(base::length(spec) == 0, F, base::all(spec %in% uj:::.iii))
}

#' @rdname iii
#' @export
III <- function(x, spec, ...) {
  errs <- uj:::.meets_errs(x, ...)
  if (!uj::is_iii_spec(spec)) {errs <- base::c(errs, '[spec] must be a complete character vec (?cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from iii_props().')}
  if (!base::is.null(errs)) {uj::stopperr(errs, PKG = "uj")}
  for (prop in base::toupper(uj:::.spec2props(spec))) {if (uj::run('uj:::.', prop, '(x)')) {return(T)}}
  F
}

#' @rdname iii
#' @export
CMP <- function(x, ...) {uj::III(x, 'x', ...)}

#' @rdname iii
#' @export
MSS <- function(x, ...) {uj::III(x, 'mss', ...)}

#' @rdname iii
#' @export
NA0 <- function(x, ...) {uj::III(x, 'nas', ...)}

#' @rdname iii
#' @export
OK0 <- function(x, ...) {uj::III(x, 'oks', ...)}

#' @rdname iii
#' @export
PRT <- function(x, ...) {uj::III(x, 'prt', ...)}

#' @rdname iii
#' @export
DUP <- function(x, ...) {uj::III(x, 'dup', ...)}

#' @rdname iii
#' @export
UNQ <- function(x, ...) {uj::III(x, 'unq', ...)}
