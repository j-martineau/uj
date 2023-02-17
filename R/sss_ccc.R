#' @encoding UTF-8
#' @family props
#' @title Shape + xclass combination properties
#' @description These functions check for the combination of \link[=sss]{shape} and \link[=ccc]{xlass} properties.
#' @details
#' \tabular{ll}{  `sss_cccfuns`   \tab What \link[=sss]{shape} + \link[=ccc]{xclass} combination \link[=prop_funs]{property functions} are there?                                                                                    \cr   \tab     }
#' \tabular{ll}{  `sss_ccc`       \tab Is `x` a match to the single shape and xclass properties in `bbb` and `ccc`, respectively?                                                                                                    \cr   \tab   \cr
#'                `{sss}{ccc}`    \tab Is `x` a match to single shape property `'{sss}'` and single xclass property `'{ccc}'`, where `{sss}` and `{ccc}` are placeholders for any given shape property and any given xclass property, respectively? }
#' \cr Some combinations of shape and xclass are nonsensical. That includes:
#' \itemize{\item Shapes `emp` (empty) and `pnt` (point) combined with xclasses `mvc` (multivec) or `scl` (scalar).
#'          \item Shapes `row`, `col` (column), or `rct` (rectangular) combined with xclasses other than `dtf` (data.frame) or `mat` (matrix),
#'          \item Shape `sqr` (square) combined with any xclass other than `mat` (matrix).}
#' \cr Nonsensical combinations do not have corresponding `sss_ccc` property functions.
#' @param x An R object.
#' @param sss A character scalar single basic property from \code{link{sss_props}()}.
#' @param ccc A character scalar single xclass property from \code{link{ccc_props}()}
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return **A character vector** \cr `sss_ccc_funs`
#' \cr\cr  **A logical scalar**   \cr `sss_ccc, {sss}{ccc}`
#' @examples
#' sss_cccfuns()
#' sss_cccprops()
#' sss_ccc(letters, "lin", "vec")
#' sss_ccc(1, "emp", "vec")
#' lin_gen(letters)
#' pnt_vec(1)
#' @export
sss_ccc <- function(x, sss, ccc, ...) {
  uj::errs_if_pop(base::c(uj:::.meets_errs(x, ...),
                        uj::f0(uj::isIN1(base::toupper(sss), uj::.sss), NULL, "[sss] is not a scalar value from sss_props()."),
                        uj::f0(uj::isIN1(base::toupper(ccc), uj::.ccc), NULL, "[ccc] is not a scalar value from ccc_props().")), PKG = "uj" )
  sss <- base::tolower(sss); SSS <- base::toupper(sss)
  ccc <- base::tolower(ccc); ccc <- base::toupper(ccc)
  if (uj::DEF(errs)) {uj::stopper(errs, PKG = "uj")}
  else if (!uj::meets(x, ...)) {F}
  else if (sss == "emp") {
    if (uj::notNDIM2P(x)) {
      if (uj::notNRC0(x)) {F}
      else if (ccc == "arr") {uj::ARR(x)}
      else if (ccc == "dtf") {uj::DTF(x)}
      else if (ccc == "gen") {uj::GEN(x)}
      else if (ccc == "mat") {uj::MAT(x)}
      else if (ccc == "vec") {uj::VEC(x)}
      else if (ccc == "vls") {uj::VLS(x)}
      else {F}
    } else if (uj::nRC0(x)) {uj::isIN1(ccc, "arr", "arr", "vec", "vls")}
  } else {uj::run("uj::", SSS, "(x) & uj::", ccc, "(x)")}
}

#' @rdname sss_ccc
#' @export
sss_ccc_funs <- function() {base::sort(base::c("emp_arr", "emp_dtf", "emp_gen", "emp_mat", "emp_vec", "emp_vls", "pnt_arr", "pnt_dtf", "pnt_gen", "pnt_mat", "pnt_vec", "pnt_vls", "lin_arr", "lin_dtf", "lin_gen", "lin_mat", "linMVC", "lin_vec", "lin_vls", "col_dtf", "col_mat", "row_dtf", "row_mat", "rct_dtf", "rct_mat", "sld_arr", "sqr_mat"))}

#' @rdname sss_ccc
#' @export
emp_arr <- function(x, ...) {uj::sss_ccc(x, "emp", "arr", ...)}

#' @rdname sss_ccc
#' @export
emp_dtf <- function(x, ...) {uj::sss_ccc(x, "emp", "dtf", ...)}

#' @rdname sss_ccc
#' @export
emp_gen <- function(x, ...) {uj::sss_ccc(x, "emp", "gen", ...)}

#' @rdname sss_ccc
#' @export
emp_mat <- function(x, ...) {uj::sss_ccc(x, "emp", "mat", ...)}

#' @rdname sss_ccc
#' @export
emp_vec <- function(x, ...) {uj::sss_ccc(x, "emp", "vec", ...)}

#' @rdname sss_ccc
#' @export
emp_vls <- function(x, ...) {uj::sss_ccc(x, "emp", "vls", ...)}

#' @rdname sss_ccc
#' @export
pnt_arr <- function(x, ...) {uj::sss_ccc(x, "pnt", "arr", ...)}

#' @rdname sss_ccc
#' @export
pnt_dtf <- function(x, ...) {uj::sss_ccc(x, "pnt", "dtf", ...)}

#' @rdname sss_ccc
#' @export
pnt_gen <- function(x, ...) {uj::sss_ccc(x, "pnt", "gen", ...)}

#' @rdname sss_ccc
#' @export
pnt_mat <- function(x, ...) {uj::sss_ccc(x, "pnt", "mat", ...)}

#' @rdname sss_ccc
#' @export
pnt_scl <- function(x, ...) {uj::sss_ccc(x, "pnt", "scl", ...)}

#' @rdname sss_ccc
#' @export
pnt_vec <- function(x, ...) {uj::sss_ccc(x, "pnt", "vec", ...)}

#' @rdname sss_ccc
#' @export
pnt_vls <- function(x, ...) {uj::sss_ccc(x, "pnt", "vls", ...)}

#' @rdname sss_ccc
#' @export
lin_arr <- function(x, ...) {uj::sss_ccc(x, "lin", "arr", ...)}

#' @rdname sss_ccc
#' @export
lin_dtf <- function(x, ...) {uj::sss_ccc(x, "lin", "dtf", ...)}

#' @rdname sss_ccc
#' @export
lin_gen <- function(x, ...) {uj::sss_ccc(x, "lin", "gen", ...)}

#' @rdname sss_ccc
#' @export
lin_mat <- function(x, ...) {uj::sss_ccc(x, "lin", "mat", ...)}

#' @rdname sss_ccc
#' @export
linMVC <- function(x, ...) {uj::sss_ccc(x, "lin", "mvc", ...)}

#' @rdname sss_ccc
#' @export
lin_vec <- function(x, ...) {uj::sss_ccc(x, "lin", "vec", ...)}

#' @rdname sss_ccc
#' @export
lin_vls <- function(x, ...) {uj::sss_ccc(x, "lin", "vls", ...)}

#' @rdname sss_ccc
#' @export
col_dtf <- function(x, ...) {uj::sss_ccc(x, "col", "dtf", ...)}

#' @rdname sss_ccc
#' @export
col_mat <- function(x, ...) {uj::sss_ccc(x, "col", "mat", ...)}

#' @rdname sss_ccc
#' @export
row_dtf <- function(x, ...) {uj::sss_ccc(x, "row", "dtf", ...)}

#' @rdname sss_ccc
#' @export
row_mat <- function(x, ...) {uj::sss_ccc(x, "row", "mat", ...)}

#' @rdname sss_ccc
#' @export
rct_dtf <- function(x, ...) {uj::sss_ccc(x, "rct", "dtf", ...)}

#' @rdname sss_ccc
#' @export
rct_mat <- function(x, ...) {uj::sss_ccc(x, "rct", "mat", ...)}

#' @rdname sss_ccc
#' @export
sqr_mat <- function(x, ...) {uj::sss_ccc(x, "sqr", "mat", ...)}

#' @rdname sss_ccc
#' @export
sld_arr <- function(x, ...) {uj::sss_ccc(x, "sld", "arr", ...)}
