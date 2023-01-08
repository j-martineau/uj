#' @encoding UTF-8
#' @family props
#' @title Shape + xclass combination properties
#' @description \tabular{rl}{
#'     `sss_ccc_funs`   \tab What \link[=sss]{shape} + \link[=ccc]{xclass} combination \link[=prop_funs]{property functions} are there?
#'   \cr                \tab  
#'   \cr    `sss_ccc`   \tab Is `x` a match to the single shape and xclass properties in `bbb` and `ccc`, respectively?
#'   \cr                \tab  
#'   \cr    `SSS_CCC`   \tab Is `x` a match to single shape and xclass properties `'SSS'` and `'CCC'`, respectively?
#' }
#' Some combinations of shape and xclass are nonsensical. That includes:\itemize{
#'   \item Shapes `emp` and `pnt` combined with xclasses `mvc` or `scl`.
#'   \item Shapes `row`, `col`, or `rct` combined with xclasses other than `dtf` or `mat`
#'   \item Shape `sqr` combined with any xclass other than `mat`.
#' }
#' Those nonsensical combinations do not have corresponding `SSS_CCC` property functions.
#' @param x An R object.
#' @param sss A character scalar single basic property from \code{link{sss_props}()}.
#' @param ccc A character scalar single xclass property from \code{link{ccc_props}()}
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return *A character vector*
#'  \cr   `sss_ccc_funs`
#'  \cr\cr *A logical scalar*
#'  \cr   `SSS_CCC`
#'  \cr   `sss_ccc`
#' @examples
#' sss_ccc_funs()
#' sss_ccc_props()
#' sss_ccc(letters, "lin", "vec")
#' sss_ccc(1, "emp", "vec")
#' lin_gen(letters)
#' pnt_vec(1)
#' @export
sss_ccc <- function(x, sss, ccc, ...) {
  errs <- base::c(uj:::.meets_errs(x, ...),
                  uj::f0(isIN(sss, uj:::.ssss), NULL, "[sss] is not a scalar value from sss_props()."),
                  uj::f0(isIN(ccc, uj:::.cccs), NULL, "[ccc] is not a scalar value from ccc_props()."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  else if (!uj::meets(x, ...)) {F}
  else if (sss == "emp") {
    if (base::length(base::dim(x)) <= 2) {
      if (!(base::NROW(x) * base::NCOL(x) == 0)) {F}
      else if (ccc == "arr") {base::is.array(x) | base::is.vector(x)}
      else if (ccc == "dtf") {base::is.data.frame(x)}
      else if (ccc == "gen") {base::is.array(x) | base::is.vector(x) | uj::ivls(x)}
      else if (ccc == "mat") {base::is.matrix(x)}
      else if (ccc == "vec") {base::is.vector(x)}
      else if (ccc == "vls") {uj::ivls(x)}
      else {F}
    } else if (base::NROW(x) * base::NCOL(x) == 0) {ccc %in% c("arr", "gen", "vec", "vls")}
  } else {uj::run("uj:::i", sss, "(x) & i", ccc, "(x)")}
}

#' @rdname bbb_ccc
#' @export
sss_ccc_funs <- function() {base::sort(base::c("emp_arr", "emp_dtf", "emp_gen", "emp_mat", "emp_vec", "emp_vls", "pnt_arr", "pnt_dtf", "pnt_gen", "pnt_mat", "pnt_vec", "pnt_vls", "lin_arr", "lin_dtf", "lin_gen", "lin_mat", "lin_mvc", "lin_vec", "lin_vls", "col_dtf", "col_mat", "row_dtf", "row_mat", "rct_dtf", "rct_mat", "sld_arr", "sqr_mat"))}

#' @rdname bbb_ccc
#' @export
emp_arr <- function(x, ...) {uj::sss_ccc(x, "emp", "arr", ...)}

#' @rdname bbb_ccc
#' @export
emp_dtf <- function(x, ...) {uj::sss_ccc(x, "emp", "dtf", ...)}

#' @rdname bbb_ccc
#' @export
emp_gen <- function(x, ...) {uj::sss_ccc(x, "emp", "gen", ...)}

#' @rdname bbb_ccc
#' @export
emp_mat <- function(x, ...) {uj::sss_ccc(x, "emp", "mat", ...)}

#' @rdname bbb_ccc
#' @export
emp_vec <- function(x, ...) {uj::sss_ccc(x, "emp", "vec", ...)}

#' @rdname bbb_ccc
#' @export
emp_vls <- function(x, ...) {uj::sss_ccc(x, "emp", "vls", ...)}

#' @rdname bbb_ccc
#' @export
pnt_arr <- function(x, ...) {uj::sss_ccc(x, "pnt", "arr", ...)}

#' @rdname bbb_ccc
#' @export
pnt_dtf <- function(x, ...) {uj::sss_ccc(x, "pnt", "dtf", ...)}

#' @rdname bbb_ccc
#' @export
pnt_gen <- function(x, ...) {uj::sss_ccc(x, "pnt", "gen", ...)}

#' @rdname bbb_ccc
#' @export
pnt_mat <- function(x, ...) {uj::sss_ccc(x, "pnt", "mat", ...)}

#' @rdname bbb_ccc
#' @export
pnt_scl <- function(x, ...) {uj::sss_ccc(x, "pnt", "scl", ...)}

#' @rdname bbb_ccc
#' @export
pnt_vec <- function(x, ...) {uj::sss_ccc(x, "pnt", "vec", ...)}

#' @rdname bbb_ccc
#' @export
pnt_vls <- function(x, ...) {uj::sss_ccc(x, "pnt", "vls", ...)}

#' @rdname bbb_ccc
#' @export
lin_arr <- function(x, ...) {uj::sss_ccc(x, "lin", "arr", ...)}

#' @rdname bbb_ccc
#' @export
lin_dtf <- function(x, ...) {uj::sss_ccc(x, "lin", "dtf", ...)}

#' @rdname bbb_ccc
#' @export
lin_gen <- function(x, ...) {uj::sss_ccc(x, "lin", "gen", ...)}

#' @rdname bbb_ccc
#' @export
lin_mat <- function(x, ...) {uj::sss_ccc(x, "lin", "mat", ...)}

#' @rdname bbb_ccc
#' @export
lin_mvc <- function(x, ...) {uj::sss_ccc(x, "lin", "mvc", ...)}

#' @rdname bbb_ccc
#' @export
lin_vec <- function(x, ...) {uj::sss_ccc(x, "lin", "vec", ...)}

#' @rdname bbb_ccc
#' @export
lin_vls <- function(x, ...) {uj::sss_ccc(x, "lin", "vls", ...)}

#' @rdname bbb_ccc
#' @export
col_dtf <- function(x, ...) {uj::sss_ccc(x, "col", "dtf", ...)}

#' @rdname bbb_ccc
#' @export
col_mat <- function(x, ...) {uj::sss_ccc(x, "col", "mat", ...)}

#' @rdname bbb_ccc
#' @export
row_dtf <- function(x, ...) {uj::sss_ccc(x, "row", "dtf", ...)}

#' @rdname bbb_ccc
#' @export
row_mat <- function(x, ...) {uj::sss_ccc(x, "row", "mat", ...)}

#' @rdname bbb_ccc
#' @export
rct_dtf <- function(x, ...) {uj::sss_ccc(x, "rct", "dtf", ...)}

#' @rdname bbb_ccc
#' @export
rct_mat <- function(x, ...) {uj::sss_ccc(x, "rct", "mat", ...)}

#' @rdname bbb_ccc
#' @export
sqr_mat <- function(x, ...) {uj::sss_ccc(x, "sqr", "mat", ...)}

#' @rdname bbb_ccc
#' @export
sld_arr <- function(x, ...) {uj::sss_ccc(x, "sld", "arr", ...)}
