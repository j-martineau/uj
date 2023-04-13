#' @encoding UTF-8
#' @family properties
#' @title Shape properties
#' @description Shape properties are defined as follows:
#' \tabular{lll}{  `'emp', 'EMP'`   \tab empty    \tab of length `0` (but not `NULL`)              \cr
#'                 `'pnt', 'PNT'`   \tab point    \tab of length `1`\eqn{^{(1)}}                   \cr
#'                 `'col', 'COL'`   \tab column   \tab `1 × 2+` matrix/data.frame                  \cr
#'                 `'row', 'ROW'`   \tab row      \tab `2+ × 1` matrix/data.frame                  \cr
#'                 `'lin', 'LIN'`   \tab linear   \tab `2+` positions in `1` dimension\eqn{^{(2)}} \cr
#'                 `'rct', 'RCT'`   \tab rect     \tab `2+` positions in `2` dimensions            \cr
#'                 `'sqr', 'SQR'`   \tab square   \tab `N × N` matrix where `N ≥ 2`                \cr
#'                 `'sld', 'SLD'`   \tab solid    \tab `2+` positions in `3+` dimensions             }
#'   \tabular{l}{  \eqn{^{(1)}} Includes `1x1` data.frames.                                                            \cr
#'                 \eqn{^{(2)}} Length-`2+` vectors, arrays of `2+` positions in `1` dimension, row/column data.frames.  }
#' @details
#' \tabular{ll}{  `is_sss_spec`   \tab Is `spec` a shape specification?                                                                              \cr
#'                `sss_props`     \tab What shape properties are there?                                                                              \cr
#'                `sss_funs`      \tab What shape property functions are there?                                                                      \cr   \tab  }
#' \tabular{ll}{  `{SSS}`         \tab Does `X` match single shape property `'{SSS}'`? (where `{SSS}` is a placeholder for any given shape property. \cr   \tab   \cr
#'                `SSS`           \tab Does `X` match the shape spec in argument `spec`?                                                             \cr
#'                `sss`           \tab What are `X`'s shape properties?                                                                                }
#' @param X An R object.
#' @param Spec Either `NULL` or a \link[=cmp_chr_vec]{complete character vec} containing one or more shape properties (i.e., from `sss_props()`). Shape properties in `spec` may be pipe-delimited. If there are multiple shape properties in `spec`, `X` is inspected for any match to any shape property in `spec`.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return **A character vector** \cr\cr `sss_props, sss_funs`
#' \cr\cr  **A logical scalar**   \cr\cr `is_sss_spec, {SSS}, SSS, sss`
#' @examples
#' Cl1 <- data.frame(letters = letters)
#' Cl2 <- matrix(1:10)
#' Emp <- vector(0)
#' Ln1 <- letters
#' Ln2 <- Cl1
#' Pt1 <- 1
#' Pt2 <- data.frame(letters = "a")
#' Rc1 <- data.frame(letters = letters, numbers = 1:26)
#' Rc2 <- matrix(1:10, nrow = 2)
#' Rw1 <- data.frame(a = "a", b = 1)
#' Rw2 <- matrix(1:10, nrow = 1)
#' Sld <- array(1:27, dim = c(3, 3, 3))
#' Sqr <- matrix(1:9, nrow = 3)
#' sss_props()
#' sss_funs()
#' is_sss_sped("emp|pnt")
#' sss(Cl1)
#' sss(Sqr)
#' c(SSS(Cl1, "col"), SSS(Rw1, "col"))
#' c(COL(Cl1), COL(Rw1))
#' c(COL(Cl2), COL(Rw2))
#' c(EMP(Emp), EMP(Ln1))
#' c(LIN(Ln1), LIN(Pt1))
#' c(LIN(Ln2), LIN(Rc1))
#' c(PNT(Pt1), PNT(Rc1))
#' c(PNT(Pt2), PNT(Rw1))
#' c(RCT(Rc1), RCT(Pt1))
#' c(RCT(Rc2), RCT(Ln1))
#' c(ROW(Rw1), ROW(Cl1))
#' c(ROW(Rw2), ROW(Rc1))
#' c(SLD(Sld), SLD(Rc1))
#' c(SQR(Sqr), SQR(Pt1))
#' @export
sss <- function(X) {
  Y <- NULL
  for (SSS in uj::v(SSS)) {
    match <- uj::run("uj:::.", SSS, "(X)")
    if (match) {Y <- base::c(Y, base::tolower(SSS))}
  }
  Y
}

#' @rdname sss
#' @export
sss_props <- function() {uj::v(sss)}

#' @rdname sss
#' @export
sss_funs <- function() {uj::v(SSS)}

#' @rdname sss
#' @export
is_sss_spec <- function(Spec) {
  Spec <- uj:::.spec2props(Spec)
  if (base::length(Spec) == 0) {F} else {base::all(Spec %in% uj::v(sss))}
}

#' @rdname sss
#' @export
SSS <- function(X, Spec, ...) {
  Errors <- uj:::.meets_errs(X, ...)
  if (!uj::is_sss_spec(Spec)) {Errors <- base::c(Errors, "[Spec] must be a complete character vec (?cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from sss_props().")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  if (uj::meets(X, ...)) {
    Props <- base::toupper(uj:::.spec2props(Spec))
    for (PPP in Props) {if (uj::run('uj:::.', PPP, '(X)')) {return(T)}}
  }
  F
}

#' @rdname sss
#' @export
COL <- function(X, ...) {uj::SSS(X, 'col', ...)}

#' @rdname sss
#' @export
EMP <- function(X, ...) {uj::SSS(X, 'emp', ...)}

#' @rdname sss
#' @export
LIN <- function(X, ...) {uj::SSS(X, 'lin', ...)}

#' @rdname sss
#' @export
PNT <- function(X, ...) {uj::SSS(X, 'pnt', ...)}

#' @rdname sss
#' @export
RCT <- function(X, ...) {uj::SSS(X, 'rct', ...)}

#' @rdname sss
#' @export
ROW <- function(X, ...) {uj::SSS(X, 'row', ...)}

#' @rdname sss
#' @export
SLD <- function(X, ...) {uj::SSS(X, 'sld', ...)}

#' @rdname sss
#' @export
SQR <- function(X, ...) {uj::SSS(X, 'sqr', ...)}
