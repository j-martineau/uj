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
#' @param x An R object.
#' @param spec Either `NULL` or a \link[=cmp_chr_vec]{complete character vec} containing one or more shape properties (i.e., from `sss_funs()`). Shape properties in `spec` may be pipe-delimited. If there are multiple shape properties in `spec`, `x` is inspected for any match to any shape property in `spec`.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @examples
#' Cl1 <- data.frame(letters = letters)
#' Cl2 <- matrix(1:10)
#' Emp <- vector()
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
#' sss_funs()
#' sss_funs()
#' is_sss_spec("emp|pnt")
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
sss_prop_funs <- function() {utils::help("sss_prop_funs", package = "uj")}

#' @describeIn sss_prop_funs Lists all shape properties possessed by `x`. Returns a sorted, lowercase, character vector.
#' @export
sss <- function(x) {
  Y <- NULL
  for (SSS in uj::sss_funs()) {if (uj::run("uj::.", SSS, "(x)")) {Y <- base::c(Y, base::tolower(SSS))}}
  Y
}

#' @describeIn sss_prop_funs Lists all possible shape properties. Returns a sorted, lowercase, character vector
#' @export
sss_props <- function() {base::c("col", "emp", "lin", "pnt", "rct", "row", "sld", "sqr")}

#' @describeIn sss_prop_funs Lists all possible shape-property-checking functions. Returns a sorted, uppercase, character vector.
#' @export
sss_funs <- function() {base::c("COL", "EMP", "LIN", "PNT", "RCT", "ROW", "SLD", "SQR")}

#' @describeIn sss_prop_funs Checks whether `spec` is a shape property spec. Returns a logical scalar. See \code{\link{ppp}} for the definition of a property spec.
#' @export
is_sss_spec <- function(spec) {
  spec <- uj::spec2props(spec)
  if (base::length(spec) == 0) {F} else {base::all(spec %in% uj::sss_funs())}
}

#' @describeIn sss_prop_funs Checks `x` against the shape property spec `spec`. Returns a logical scalar. See \code{\link{ppp}} for the definition of a property spec.
#' @export
SSS <- function(x, spec, ...) {
  errs <- uj::meets_errs(x, ...)
  if (!uj::is_sss_spec(spec)) {errs <- base::c(errs, "[spec] must be a complete character vec (?uj::cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from sss_funs().")}
  if (!base::is.null(errs)) {uj::stopperr(errs)}
  if (uj::meets(x, ...)) {for (PPP in base::toupper(uj::spec2props(spec))) {if (uj::run('uj::.', PPP, '(x)')) {return(T)}}}
  F
}

#' @describeIn sss_prop_funs Checks `x` against the column shape property subject to any count or value restrictions in `...`. Returns a logical scalar.
#' @export
COL <- function(x, ...) {uj::SSS(x, 'col', ...)}

#' @describeIn sss_prop_funs Checks `x` against the empty shape property subject to any count or value restrictions in `...`. Returns a logical scalar.
#' @export
EMP <- function(x, ...) {uj::SSS(x, 'emp', ...)}

#' @describeIn sss_prop_funs Checks `x` against the linear shape property subject to any count or value restrictions in `...`. Returns a logical scalar.
#' @export
LIN <- function(x, ...) {uj::SSS(x, 'lin', ...)}

#' @describeIn sss_prop_funs Checks `x` against the point shape property subject to any count or value restrictions in `...`. Returns a logical scalar.
#' @export
PNT <- function(x, ...) {uj::SSS(x, 'pnt', ...)}

#' @describeIn sss_prop_funs Checks `x` against the rectangle shape property subject to any count or value restrictions in `...`. Returns a logical scalar.
#' @export
RCT <- function(x, ...) {uj::SSS(x, 'rct', ...)}

#' @describeIn sss_prop_funs Checks `x` against the row shape property subject to any count or value restrictions in `...`. Returns a logical scalar.
#' @export
ROW <- function(x, ...) {uj::SSS(x, 'row', ...)}

#' @describeIn sss_prop_funs Checks `x` against the solid shape property subject to any count or value restrictions in `...`. Returns a logical scalar.
#' @export
SLD <- function(x, ...) {uj::SSS(x, 'sld', ...)}

#' @describeIn sss_prop_funs Checks `x` against the square shape property subject to any count or value restrictions in `...`. Returns a logical scalar.
#' @export
SQR <- function(x, ...) {uj::SSS(x, 'sqr', ...)}
