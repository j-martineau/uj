#' @encoding UTF-8
#' @family props
#' @title Extended class (xclass) properties
#' @description `xclass` properties are defined as follows:
#' \tabular{lll}{  `'arr', 'ARR'`   \tab array      \tab arrays                                   \cr
#'                 `'mat', 'MAT'`   \tab matrix     \tab matrices                                 \cr
#'                 `'dtf', 'DTF'`   \tab dtf        \tab data.frames                              \cr
#'                 `'vls', 'VLS'`   \tab vlist      \tab vector-lists  \eqn{^{(1)}}               \cr
#'                 `'gen', 'GEN'`   \tab generic    \tab vectors, vlists, and arrays              \cr
#'                 `'scl', 'SCL'`   \tab scalar     \tab Length-`1` generics                      \cr
#'                 `'mvc', 'MVC'`   \tab multivec   \tab Length-`2+` \link[=LIN]{linear} generics \cr
#'                 `'vec', 'VEC'`   \tab vec        \tab scalars and multivecs                      }
#'   \tabular{l}{  \eqn{^{(1)}} Non-`data.frame` lists. }
#' @details
#' \tabular{ll}{  `is_ccc_spec`   \tab Is `spec` an xclass specification?                                                                                   \cr   \tab   \cr
#'                `ccc_props`     \tab What xclass properties are there?                                                                                    \cr   \tab   \cr
#'                `ccc_funs`      \tab What xclass property functions are there?                                                                            \cr   \tab   \cr
#'                `{CCC}`         \tab Is `x` a match to the single xclass property `'{CCC}'` where `{CCC}` is a placeholder for any given xclass property? \cr   \tab   \cr
#'                `CCC`           \tab Is `x` a match to the xclass specification `spec`?                                                                   \cr   \tab   \cr
#'                `ccc`           \tab What are `x`'s xclass properties?                                                                                                   }
#' @param x An R object.
#' @param spec `NULL` or a \link[=cmp_chr_scl]{complete character vec} containing one or more xclass properties (i.e., from \code{\link{ccc_props}()}). Properties may be pipe-delimited. If there are multiple properties in `spec`, `x` is inspected for a match to any of the specified properties.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return **A character vector** \cr\cr `ccc_props, ccc_funs, ccc`
#' \cr\cr  **A logical vector**   \cr\cr `is_ccc_spec, {CCC}, CCC`
#' @examples
#' ccc_funs()
#' ccc_props()
#' is_ccc_spec("scl|vls")
#' CCC(letters, "vec|dtf")
#' VEC(letters)
#' ccc(letters)
#' @export
ccc <- function(x) {
  y <- NULL
  for (CCC in uj:::.CCC) {
    match <- uj::run("uj:::.", CCC, "(x)")
    if (match) {y <- base::c(y, base::tolower(CCC))}
  }
  y
}

#' @rdname ccc
#' @export
ccc_funs <- function() {uj:::.CCC}

#' @rdname ccc
#' @export
ccc_props <- function() {uj:::.ccc}

#' @rdname ccc
#' @export
is_ccc_spec <- function(spec) {
  spec <- uj:::.spec2props(spec)
  if (base::length(spec) == 0) {F}
  else {base::all(spec %in% uj:::.ccc)}
}

#' @rdname ccc
#' @export
CCC <- function(x, spec, ...) {
  errs <- uj:::.meets_errs(x, ...)
  if (!uj::is_ccc_spec(spec)) {errs <- base::c(errs, '[spec] must be a complete character vec (?cmp_chr_vec) containing (possible pipe-separated) values from ccc_props().')}
  if (!base::is.null(errs)) {uj::stopperr(errs, PKG = "uj")}
  if (uj::meets(x, ...)) {
    props <- base::toupper(uj:::.spec2props(spec))
    for (prop in props) {
      match <- uj::run("uj:::.", prop ,"(x)")
      if (match) {return(T)}
    }
  }
  F
}

#' @rdname ccc
#' @export
ARR <- function(x, ...) {uj::CCC(x, 'arr', ...)}

#' @rdname ccc
#' @export
DTF <- function(x, ...) {uj::CCC(x, 'dtf', ...)}

#' @rdname ccc
#' @export
GEN <- function(x, ...) {uj::CCC(x, 'gen', ...)}

#' @rdname ccc
#' @export
MAT <- function(x, ...) {uj::CCC(x, 'mat', ...)}

#' @rdname ccc
#' @export
MVC <- function(x, ...) {uj::CCC(x, 'mvc', ...)}

#' @rdname ccc
#' @export
SCL <- function(x, ...) {uj::CCC(x, 'scl', ...)}

#' @rdname ccc
#' @export
VEC <- function(x, ...) {uj::CCC(x, 'vec', ...)}

#' @rdname ccc
#' @export
VLS <- function(x, ...) {uj::CCC(x, 'vls', ...)}
