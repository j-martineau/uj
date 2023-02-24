#' @encoding UTF-8
#' @family props
#' @title basic + xclass combination properties
#' @description Check for combinations of \link[=bbb]{basic} and \link[=ccc]{xclass} properties.
#' @details
#' \tabular{ll}{  `bbb_cccfuns`   \tab What \link[=bbb]{basic} + \link[=ccc]{xclass} combination \link[=prop_funs]{property functions} are there?                                                                                                  \cr   \tab   \cr
#'                `{bbb}_{ccc}`   \tab Is `x` a match to single basic property `'{bbb}'` and single xmode property `'{mmm}'`, where `{bbb}` and `{mmm}` are placeholders for any given basic property and any given xclass property, respectively? \cr   \tab   \cr
#'                `bbb_ccc`       \tab Is `x` a match to the single basic and xclass properties in `bbb` and `ccc`, respectively?                                                                                                                                 }
#' \cr\cr Some combinations of basic + xclass properties are nonsensical. For this reason, the basic properties represented in this family of functions are `c('atm', 'nil',  'pop')`, or atomic, nil (non-`NULL` and of length 0), and populated (of length `1+`).
#' \cr\cr In addition, the base property `'nil'` is nonsensical in combination with xclasses `'mvc'` (multivec) and `'scl'` (scalar), which thus do not have combined `bbb_ccc` property functions.
#' @param x An R object.
#' @param bbb A character scalar single basic property from \code{\link{bbb_props}()}.
#' @param ccc A character scalar single xclass property from \code{\link{ccc_props}()}.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return **A character vector** \cr\cr `bbb_ccc_funs`
#' \cr\cr  **A logical scalar**   \cr\cr `{bbb}_{ccc}, bbb_ccc`
#' @examples
#' bbb_ccc_funs()
#' bbb_ccc(letters, "atm", "mvc")
#' bbb_ccc(1, "nil", "vec")
#' atm_gen(letters)
#' atm_scl(1)
#' @export
bbb_ccc <- function(x, bbb, ccc, ...) {
  if (uj::isCHR(bbb)) {bbb <- base::tolower(bbb)}
  if (uj::isCHR(ccc)) {ccc <- base::tolower(ccc)}
  uj::errs_if_pop(base::c(uj:::.meets_errs(x, ...),
                          uj::f0(uj::isIN1(bbb, uj:::.bbb), NULL, "[bbb] is not a scalar value from bbb_props()."),
                          uj::f0(uj::isIN1(ccc, uj:::.ccc), NULL, "[ccc] is not a scalar value from ccc_props().")), PKG = "uj")
  arr <- ccc == "arr"; ARR <- uj::ARR(x)
  dtf <- ccc == "dtf"; DTF <- uj::DTF(x)
  gen <- ccc == "gen"; GEN <- uj::GEN(x)
  mat <- ccc == "mat"; MAT <- uj::MAT(x)
  mvc <- ccc == "mvc"; MVC <- uj::MVC(x)
  scl <- ccc == "scl"; SCL <- uj::SCL(x)
  vec <- ccc == "vec"; VEC <- uj::VEC(x)
  vls <- ccc == "vls"; VLS <- uj::VLS(x)
  pop <- bbb == "pop"; POP <- uj::POP(x)
  nil <- bbb == "nil"; NIL <- !POP & !uj::NLL(x)
  atm <- bbb == "atm"
  if (!uj::meets(x, ...)) {F}
  else if ((pop & !POP) | (nil & !NIL) | (arr & !ARR) | (dtf & !DTF) | (gen & !GEN) | (mat & !MAT) | (mvc & !MVC) | (scl & !SCL) | (vec & !VEC) | (vls & !VLS)) {F}
  else if (atm & dtf) {base::all(base::apply(x, 2, uj::isATM))}
  else if (atm & vls) {base::all(base::sapply(x, uj::isATM))}
  else if (atm) {uj::isATM(x)}
  else {uj::run("uj::", base::toupper(bbb), "(x)")}
}

#' @rdname bbb_ccc
#' @export
bbb_ccc_funs <- function() {base::sort(base::c("atm_arr", "atm_dtf", "atm_gen", "atm_mat", "atm_mvc", "atm_scl", "atm_vec", "atm_vls",
                                               "nil_arr", "nil_dtf", "nil_gen", "nil_mat",                       "nil_vec", "nil_vls",
                                               "pop_arr", "pop_dtf", "pop_gen", "pop_mat", "pop_mvc", "pop_scl", "pop_vec", "pop_vls"))}

#' @rdname bbb_ccc
#' @export
atm_arr <- function(x, ...) {uj::bbb_ccc(x, 'atm', 'arr', ...)}

#' @rdname bbb_ccc
#' @export
atm_dtf <- function(x, ...) {uj::bbb_ccc(x, 'atm', 'dtf', ...)}

#' @rdname bbb_ccc
#' @export
atm_gen <- function(x, ...) {uj::bbb_ccc(x, 'atm', 'gen', ...)}

#' @rdname bbb_ccc
#' @export
atm_mat <- function(x, ...) {uj::bbb_ccc(x, 'atm', 'mat', ...)}

#' @rdname bbb_ccc
#' @export
atm_mvc <- function(x, ...) {uj::bbb_ccc(x, 'atm', 'mvc', ...)}

#' @rdname bbb_ccc
#' @export
atm_scl <- function(x, ...) {uj::bbb_ccc(x, 'atm', 'scl', ...)}

#' @rdname bbb_ccc
#' @export
atm_vec <- function(x, ...) {uj::bbb_ccc(x, 'atm', 'vec', ...)}

#' @rdname bbb_ccc
#' @export
atm_vls <- function(x, ...) {uj::bbb_ccc(x, 'atm', 'vls', ...)}

#' @rdname bbb_ccc
#' @export
nil_arr <- function(x, ...) {uj::bbb_ccc(x, "nil", "arr", ...)}

#' @rdname bbb_ccc
#' @export
nil_dtf <- function(x, ...) {uj::bbb_ccc(x, "nil", "dtf", ...)}

#' @rdname bbb_ccc
#' @export
nil_gen <- function(x, ...) {uj::bbb_ccc(x, "nil", "gen", ...)}

#' @rdname bbb_ccc
#' @export
nil_mat <- function(x, ...) {uj::bbb_ccc(x, "nil", "mat", ...)}

#' @rdname bbb_ccc
#' @export
nil_vec <- function(x, ...) {uj::bbb_ccc(x, "nil", "vec", ...)}

#' @rdname bbb_ccc
#' @export
nil_vls <- function(x, ...) {uj::bbb_ccc(x, "nil", "vls", ...)}

#' @rdname bbb_ccc
#' @export
pop_arr <- function(x, ...) {uj::bbb_ccc(x, 'pop', 'arr', ...)}

#' @rdname bbb_ccc
#' @export
pop_dtf <- function(x, ...) {uj::bbb_ccc(x, 'pop', 'dtf', ...)}

#' @rdname bbb_ccc
#' @export
pop_gen <- function(x, ...) {uj::bbb_ccc(x, 'pop', 'gen', ...)}

#' @rdname bbb_ccc
#' @export
pop_mat <- function(x, ...) {uj::bbb_ccc(x, 'pop', 'mat', ...)}

#' @rdname bbb_ccc
#' @export
pop_mvc <- function(x, ...) {uj::bbb_ccc(x, 'pop', 'mvc', ...)}

#' @rdname bbb_ccc
#' @export
pop_scl <- function(x, ...) {uj::bbb_ccc(x, 'pop', 'scl', ...)}

#' @rdname bbb_ccc
#' @export
pop_vec <- function(x, ...) {uj::bbb_ccc(x, 'pop', 'vec', ...)}

#' @rdname bbb_ccc
#' @export
pop_vls <- function(x, ...) {uj::bbb_ccc(x, 'pop', 'vls', ...)}
