#' @encoding UTF-8
#' @family properties
#' @title Integrity properties
#' @description Integrity properties are defined for \link[=POP]{populated} \link[=atm_dtf]{atomic data.frames}, populated \link[=atm_vls]{atomic vlists}, populated atomic vectors, and populated atomic arrays. For all others, all integrity properties are considered `FALSE`. The following table summarizes valid integrity properties.
#' \tabular{lll}{  `'cmp', 'CMP'`   \tab complete      \tab Populated and atomic vectors, arrays, \link[=atm_dtf]{data.frames}, and \link[=atm_vls]{vlists} containing *no* `NA` values. \cr   \tab   \cr
#'                 `'mss', 'MSS'`   \tab missing       \tab Populated and atomic vectors, arrays, data.frames, and vlists containing *only* `NA` values.                               \cr   \tab   \cr
#'                 `'prt', 'PRt'`   \tab partial       \tab Populated and atomic vectors, arrays, data.frames, and vlists containing `NA` *and* non-`NA` values.                       \cr   \tab   \cr
#'                 `'dup', 'DUP'`   \tab duplicates    \tab Complete and containing duplicate values.                                                                                  \cr   \tab   \cr
#'                 `'unq', 'UNQ'`   \tab unique        \tab Complete and containing only unique values.                                                                                \cr   \tab   \cr
#'                 `'nas', 'NAS'`   \tab `NA` scalar   \tab Atomic scalar `NA`.                                                                                                        \cr   \tab   \cr
#'                 `'oks', 'OKS'`   \tab `OK` scalar   \tab Non-`NA` atomic scalar.                                                                                                                   }
#' \cr\cr **Integrity property functions**
#' \tabular{ll}{  `is_iii_spec`   \tab Is `spec` an integrity specification?                                                                           \cr   \tab     }
#' \tabular{ll}{  `iii_props`     \tab What integrity properties are there?                                                                            \cr   \tab     }
#' \tabular{ll}{  `iii_funs`      \tab What integrity property functions are there?                                                                    \cr   \tab     }
#' \tabular{ll}{  `iii`           \tab Gets all of `x`'s integrity properties.                                                                         \cr   \tab   \cr
#'                `III`           \tab Does `x` match integrity specification `spec`?                                                                  \cr   \tab   \cr
#'                `{iii}`         \tab Does `x` match single integrity property `'{iii}'` where `{iii}` is a placeholder for any given integrity property?                }
#' @param x An R object.
#' @param spec `NULL` or a \link[=cmp_chr_scl]{complete character vec} containing one or more integrity properties (i.e., from `iii_props()`). Integrity properties may be pipe-delimited. If there are multiple properties in `spec`, `x` is inspected for a match to any of the specified properties.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return **A character vector** \cr   `iii_props, iii_funs, iii`
#' \cr\cr  **A logical scalar**   \cr   `is_iii_spec, III, XXX`
#' @examples
#' iii_funs()
#' iii_props()
#' is_iii_spec("nas|mss")
#' CMP(NA)
#' CMP(NULL)
#' NAS(NA)
#' OKS(letters)
#' OKS(letters[1])
#' MSS(rep(NA, 10))
#' iii(NA)
#' iii(letters)
#' iii(letters[1])
#' @export
iii <- function(x) {
  y <- NULL
  for (iii in uj:::.III) {y <- base::c(y, uj::f0(uj::run(iii, '(x)'), iii, NULL))}
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
  spec <- uj:::.props_from_spec(spec)
  f0(uj::N0(spec), F, uj::allIN(spec, base::c(uj:::.iii, uj:::.III)))
}

#' @rdname iii
#' @export
III <- function(x, spec, ...) {
  uj::errs_if_pop(base::c(uj:::.meets_errs(x, ...), uj::f0(uj::is_iii_spec(spec), NULL, '[spec] must be a complete character vec (?cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from iii_props().')), PKG = "uj")
  for (prop in base::toupper(uj:::.props_from_spec(spec))) {if (uj::run('uj::', prop, '(x)')) {return(T)}}
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
NAS <- function(x, ...) {uj::III(x, 'nas', ...)}

#' @rdname iii
#' @export
OKS <- function(x, ...) {uj::III(x, 'oks', ...)}

#' @rdname iii
#' @export
PRT <- function(x, ...) {uj::III(x, 'prt', ...)}

#' @rdname iii
#' @export
DUP <- function(x, ...) {uj::III(x, 'dup', ...)}

#' @rdname iii
#' @export
UNQ <- function(x, ...) {uj::III(x, 'unq', ...)}
