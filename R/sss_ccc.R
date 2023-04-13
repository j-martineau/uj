#' @encoding UTF-8
#' @family props
#' @title Shape + xclass combination properties
#' @description These functions check for the combination of \link[=sss]{shape} and \link[=ccc]{xlass} properties.
#' @details
#' \tabular{ll}{  `sss_ccc_funs`   \tab What \link[=sss]{shape} + \link[=ccc]{xclass} combination \link[=prop_funs]{property functions} are there?                                                                                                   \cr   \tab   \cr
#'                `{sss}_{ccc}`    \tab Is `X` a match to single shape property `'{sss}'` and single xclass property `'{ccc}'`, where `{sss}` and `{ccc}` are placeholders for any given shape property and any given xclass property, respectively? \cr   \tab   \cr
#'                `sss_ccc`        \tab Is `X` a match to the single shape and xclass properties in `SSS` and `CCCc`, respectively?                                                                                                                                  }
#' \cr Some combinations of shape and xclass are nonsensical. That includes:
#' \itemize{\item Shapes `emp` (empty) and `pnt` (point) combined with xclasses `mvc` (multivec) or `scl` (scalar).
#'          \item Shapes `row`, `col` (column), or `rct` (rectangular) combined with xclasses other than `dtf` (data.frame) or `mat` (matrix),
#'          \item Shape `sqr` (square) combined with any xclass other than `mat` (matrix).}
#' \cr Nonsensical combinations do not have corresponding `sss_ccc` property functions.
#' @param X An R object.
#' @param SSS A character scalar single basic property from \code{link{sss_props}()}.
#' @param CCC A character scalar single xclass property from \code{link{ccc_props}()}
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return **A character vector** \cr\cr `sss_ccc_funs`
#' \cr\cr  **A logical scalar**   \cr\cr `{sss}_{ccc}, sss_ccc`
#' @examples
#' sss_cccfuns()
#' sss_cccprops()
#' sss_ccc(letters, "lin", "vec")
#' sss_ccc(1, "emp", "vec")
#' lin_gen(letters)
#' pnt_vec(1)
#' @export
sss_ccc <- function(X, SSS, CCC, ...) {
  if (!uj:::.cmp_chr_scl(SSS)) {OkSSS <- F} else {OkSSS <- base::tolower(SSS) %in% uj::v(sss)}
  if (!uj:::.cmp_chr_scl(CCC)) {OkCCC <- F} else {OkCCC <- base::tolower(CCC) %in% uj::v(ccc)}
  Errors <- uj:::.meets_errs(X, ...)
  if (!OkSSS) {Errors <- base::c(Errors, "[SSS] is not a scalar value from sss_props().")}
  if (!OkCCC) {Errors <- base::c(Errors, "[CCC] is not a scalar value from ccc_props().")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  if (!uj::meets(X, ...)) {return(F)}
  SSS <- base::toupper(SSS)
  CCC <- base::toupper(CCC)
  if (SSS == "emp") {
    if (base::length(base::dim(X)) != 2) {
      if (base::NROW(X) * base::NCOL(X) != 0) {F}
      else if (CCC == "ARR") {uj:::.ARR(X)}
      else if (CCC == "DTF") {uj:::.DTF(X)}
      else if (CCC == "GEN") {uj:::.GEN(X)}
      else if (CCC == "MAT") {uj:::.MAT(X)}
      else if (CCC == "VEC") {uj:::.VEC(X)}
      else if (CCC == "VLS") {uj:::.VLS(X)}
      else {F}
    } else if (base::NROW(X) * base::NCOL(X) == 0) {CCC %in% base::c("ARR", "VEC", "VLS")}
  } else {uj::run("uj:::.", SSS, "(X) & uj:::.", CCC, "(X)")}
}

#' @rdname sss_ccc
#' @export
sss_ccc_funs <- function() {base::sort(base::c("emp_arr", "emp_dtf", "emp_gen", "emp_mat", "emp_vec", "emp_vls", "pnt_arr", "pnt_dtf", "pnt_gen", "pnt_mat", "pnt_vec", "pnt_vls", "lin_arr", "lin_dtf", "lin_gen", "lin_mat", "lin_mvc", "lin_vec", "lin_vls", "col_dtf", "col_mat", "row_dtf", "row_mat", "rct_dtf", "rct_mat", "sld_arr", "sqr_mat"))}

#' @rdname sss_ccc
#' @export
emp_arr <- function(X, ...) {uj::sss_ccc(X, "emp", "arr", ...)}

#' @rdname sss_ccc
#' @export
emp_dtf <- function(X, ...) {uj::sss_ccc(X, "emp", "dtf", ...)}

#' @rdname sss_ccc
#' @export
emp_gen <- function(X, ...) {uj::sss_ccc(X, "emp", "gen", ...)}

#' @rdname sss_ccc
#' @export
emp_mat <- function(X, ...) {uj::sss_ccc(X, "emp", "mat", ...)}

#' @rdname sss_ccc
#' @export
emp_vec <- function(X, ...) {uj::sss_ccc(X, "emp", "vec", ...)}

#' @rdname sss_ccc
#' @export
emp_vls <- function(X, ...) {uj::sss_ccc(X, "emp", "vls", ...)}

#' @rdname sss_ccc
#' @export
pnt_arr <- function(X, ...) {uj::sss_ccc(X, "pnt", "arr", ...)}

#' @rdname sss_ccc
#' @export
pnt_dtf <- function(X, ...) {uj::sss_ccc(X, "pnt", "dtf", ...)}

#' @rdname sss_ccc
#' @export
pnt_gen <- function(X, ...) {uj::sss_ccc(X, "pnt", "gen", ...)}

#' @rdname sss_ccc
#' @export
pnt_mat <- function(X, ...) {uj::sss_ccc(X, "pnt", "mat", ...)}

#' @rdname sss_ccc
#' @export
pnt_scl <- function(X, ...) {uj::sss_ccc(X, "pnt", "scl", ...)}

#' @rdname sss_ccc
#' @export
pnt_vec <- function(X, ...) {uj::sss_ccc(X, "pnt", "vec", ...)}

#' @rdname sss_ccc
#' @export
pnt_vls <- function(X, ...) {uj::sss_ccc(X, "pnt", "vls", ...)}

#' @rdname sss_ccc
#' @export
lin_arr <- function(X, ...) {uj::sss_ccc(X, "lin", "arr", ...)}

#' @rdname sss_ccc
#' @export
lin_dtf <- function(X, ...) {uj::sss_ccc(X, "lin", "dtf", ...)}

#' @rdname sss_ccc
#' @export
lin_gen <- function(X, ...) {uj::sss_ccc(X, "lin", "gen", ...)}

#' @rdname sss_ccc
#' @export
lin_mat <- function(X, ...) {uj::sss_ccc(X, "lin", "mat", ...)}

#' @rdname sss_ccc
#' @export
linMVC <- function(X, ...) {uj::sss_ccc(X, "lin", "mvc", ...)}

#' @rdname sss_ccc
#' @export
lin_vec <- function(X, ...) {uj::sss_ccc(X, "lin", "vec", ...)}

#' @rdname sss_ccc
#' @export
lin_vls <- function(X, ...) {uj::sss_ccc(X, "lin", "vls", ...)}

#' @rdname sss_ccc
#' @export
col_dtf <- function(X, ...) {uj::sss_ccc(X, "col", "dtf", ...)}

#' @rdname sss_ccc
#' @export
col_mat <- function(X, ...) {uj::sss_ccc(X, "col", "mat", ...)}

#' @rdname sss_ccc
#' @export
row_dtf <- function(X, ...) {uj::sss_ccc(X, "row", "dtf", ...)}

#' @rdname sss_ccc
#' @export
row_mat <- function(X, ...) {uj::sss_ccc(X, "row", "mat", ...)}

#' @rdname sss_ccc
#' @export
rct_dtf <- function(X, ...) {uj::sss_ccc(X, "rct", "dtf", ...)}

#' @rdname sss_ccc
#' @export
rct_mat <- function(X, ...) {uj::sss_ccc(X, "rct", "mat", ...)}

#' @rdname sss_ccc
#' @export
sqr_mat <- function(X, ...) {uj::sss_ccc(X, "sqr", "mat", ...)}

#' @rdname sss_ccc
#' @export
sld_arr <- function(X, ...) {uj::sss_ccc(X, "sld", "arr", ...)}
