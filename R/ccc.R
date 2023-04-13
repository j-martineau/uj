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
#' \tabular{ll}{  `is_ccc_spec`   \tab Is `Spec` an xclass specification?                                                                                   \cr   \tab   \cr
#'                `ccc_props`     \tab What xclass properties are there?                                                                                    \cr   \tab   \cr
#'                `ccc_funs`      \tab What xclass property functions are there?                                                                            \cr   \tab   \cr
#'                `{CCC}`         \tab Is `X` a match to the single xclass property `'{CCC}'` where `{CCC}` is a placeholder for any given xclass property? \cr   \tab   \cr
#'                `CCC`           \tab Is `X` a match to the xclass specification `Spec`?                                                                   \cr   \tab   \cr
#'                `ccc`           \tab What are `X`'s xclass properties?                                                                                                   }
#' @param X An R object.
#' @param Spec `NULL` or a \link[=cmp_chr_scl]{complete character vec} containing one or more xclass properties (i.e., from \code{\link{ccc_props}()}). Properties may be pipe-delimited. If there are multiple properties in `Spec`, `X` is inspected for a match to any of the specified properties.
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
ccc <- function(X) {
  y <- NULL
  for (CCC in uj::v(CCC)) {
    match <- uj::run("uj:::.", CCC, "(X)")
    if (match) {y <- base::c(y, base::tolower(CCC))}
  }
  y
}

#' @rdname ccc
#' @export
ccc_funs <- function() {uj::v(CCC)}

#' @rdname ccc
#' @export
ccc_props <- function() {uj::v(ccc)}

#' @rdname ccc
#' @export
is_ccc_spec <- function(Spec) {
  Spec <- uj:::.spec2props(Spec)
  if (base::length(Spec) == 0) {F}
  else {base::all(Spec %in% uj::v(ccc))}
}

#' @rdname ccc
#' @export
CCC <- function(X, Spec, ...) {
  errs <- uj:::.meets_errs(X, ...)
  if (!uj::is_ccc_spec(Spec)) {errs <- base::c(errs, '[Spec] must be a complete character vec (?cmp_chr_vec) containing (possible pipe-separated) values from ccc_props().')}
  if (!base::is.null(errs)) {uj::stopperr(errs, PKG = "uj")}
  if (uj::meets(X, ...)) {
    Props <- base::toupper(uj:::.spec2props(Spec))
    for (Prop in Props) {
      Match <- uj::run("uj:::.", Prop ,"(X)")
      if (Match) {return(T)}
    }
  }
  F
}

#' @rdname ccc
#' @export
ARR <- function(X, ...) {uj::CCC(X, 'arr', ...)}

#' @rdname ccc
#' @export
DTF <- function(X, ...) {uj::CCC(X, 'dtf', ...)}

#' @rdname ccc
#' @export
GEN <- function(X, ...) {uj::CCC(X, 'gen', ...)}

#' @rdname ccc
#' @export
MAT <- function(X, ...) {uj::CCC(X, 'mat', ...)}

#' @rdname ccc
#' @export
MVC <- function(X, ...) {uj::CCC(X, 'mvc', ...)}

#' @rdname ccc
#' @export
SCL <- function(X, ...) {uj::CCC(X, 'scl', ...)}

#' @rdname ccc
#' @export
VEC <- function(X, ...) {uj::CCC(X, 'vec', ...)}

#' @rdname ccc
#' @export
VLS <- function(X, ...) {uj::CCC(X, 'vls', ...)}
