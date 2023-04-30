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
#' \tabular{ll}{  `is_iii_spec`   \tab Is `Spec` an integrity specification?                                                                \cr   \tab   \cr
#'                `iii_props`     \tab What integrity properties are there?                                                                 \cr   \tab   \cr
#'                `iii_funs`      \tab What integrity property functions are there?                                                         \cr   \tab   \cr
#'                `iii`           \tab Gets all of `X`'s integrity properties.                                                              \cr   \tab   \cr
#'                `III`           \tab Does `X` match integrity specification `Spec`?                                                       \cr   \tab   \cr
#'                `{II}`          \tab Does `X` match single integrity property `'{III}'` where `{III}` is a placeholder for any given integrity property? }
#' @param X An R object.
#' @param Spec `NULL` or a \link[=cmp_chr_scl]{complete character vec} containing one or more integrity properties (i.e., from `iii_props()`). Integrity properties may be pipe-delimited. If there are multiple properties in `Spec`, `X` is inspected for a match to any of the specified properties.
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
iii <- function(X) {
  Y <- NULL
  for (III in uj::v(III)) {Y <- base::c(Y, uj::f0(uj::run("uj:::.", III, '(X)'), III, NULL))}
  Y
}

#' @rdname iii
#' @export
iii_funs <- function() {uj::v(III)}

#' @rdname iii
#' @export
iii_props <- function() {uj::v(iii)}

#' @rdname iii
#' @export
is_iii_spec <- function(Spec) {
  Spec <- uj:::.spec2props(Spec)
  f0(base::length(Spec) == 0, F, base::all(Spec %in% uj::v(iii)))
}

#' @rdname iii
#' @export
III <- function(X, Spec, ...) {
  Errors <- uj:::.meets_errs(X, ...)
  if (!uj::is_iii_spec(Spec)) {Errors <- base::c(Errors, '[Spec] must be a complete character vec (?cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from iii_props().')}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  for (Prop in base::toupper(uj:::.spec2props(Spec))) {if (uj::run('uj:::.', Prop, '(X)')) {return(T)}}
  F
}

#' @rdname iii
#' @export
CMP <- function(X, ...) {uj::III(X, 'X', ...)}

#' @rdname iii
#' @export
MSS <- function(X, ...) {uj::III(X, 'mss', ...)}

#' @rdname iii
#' @export
NA0 <- function(X, ...) {uj::III(X, 'na0', ...)}

#' @rdname iii
#' @export
OK0 <- function(X, ...) {uj::III(X, 'ok0', ...)}

#' @rdname iii
#' @export
PRT <- function(X, ...) {uj::III(X, 'prt', ...)}

#' @rdname iii
#' @export
DUP <- function(X, ...) {uj::III(X, 'dup', ...)}

#' @rdname iii
#' @export
UNQ <- function(X, ...) {uj::III(X, 'unq', ...)}
