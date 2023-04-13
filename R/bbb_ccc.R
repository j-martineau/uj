#' @encoding UTF-8
#' @family props
#' @title basic + xclass combination properties
#' @description Check for combinations of \link[=bbb]{basic} and \link[=ccc]{xclass} properties.
#' @details
#' \tabular{ll}{  `bbb_cccfuns`   \tab What \link[=bbb]{basic} + \link[=ccc]{xclass} combination \link[=prop_funs]{property functions} are there?                                                                                                  \cr   \tab   \cr
#'                `{bbb}_{ccc}`   \tab Is `X` a match to single basic property `'{bbb}'` and single xmode property `'{mmm}'`, where `{bbb}` and `{mmm}` are placeholders for any given basic property and any given xclass property, respectively? \cr   \tab   \cr
#'                `bbb_ccc`       \tab Is `X` a match to the single basic and xclass properties in `bbb` and `ccc`, respectively?                                                                                                                                 }
#' \cr\cr Some combinations of basic + xclass properties are nonsensical. For this reason, the basic properties represented in this family of functions are `c('atm', 'nil',  'pop')`, or atomic, nil (non-`NULL` and of length 0), and populated (of length `1+`).
#' \cr\cr In addition, the base property `'nil'` is nonsensical in combination with xclasses `'mvc'` (multivec) and `'scl'` (scalar), which thus do not have combined `bbb_ccc` property functions.
#' @param X An R object.
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
bbb_ccc <- function(X, BBB, CCC, ...) {
  if (base::is.character(BBB)) {BBB <- base::tolower(BBB)}
  if (base::is.character(CCC)) {CCC <- base::tolower(CCC)}
  ErrBBB <- "[BBB] is not a scalar value from bbb_props()."
  ErrCCC <- "[CCC] is not a scalar value from ccc_props()."
  Errors <- uj:::.meets_errs(X, ...)
  if (base::length(BBB) != 1) {Errors <- base::c(Errors, ErrBBB)} else if (!(BBB %in% uj::v(bbb))) {Errors <- base::c(Errors, ErrBBB)}
  if (base::length(CCC) != 1) {Errors <- base::c(Errors, ErrCCC)} else if (!(CCC %in% uj::v(ccc))) {Errors <- base::c(Errors, ErrCCC)}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  arr <- CCC == "arr"; ARR <- uj:::.ARR(X)
  dtf <- CCC == "dtf"; DTF <- uj:::.DTF(X)
  gen <- CCC == "gen"; GEN <- uj:::.GEN(X)
  mat <- CCC == "mat"; MAT <- uj:::.MAT(X)
  mvc <- CCC == "mvc"; MVC <- uj:::.MVC(X)
  scl <- CCC == "scl"; SCL <- uj:::.SCL(X)
  vec <- CCC == "vec"; VEC <- uj:::.VEC(X)
  vls <- CCC == "vls"; VLS <- uj:::.VLS(X)
  pop <- BBB == "pop"; POP <- uj:::.POP(X)
  nil <- BBB == "nil"; NIL <- !POP & !base::is.null(X)
  atm <- BBB == "atm"
  if (!uj::meets(X, ...)) {F}
  else if ((pop & !POP) | (nil & !NIL) | (arr & !ARR) | (dtf & !DTF) | (gen & !GEN) | (mat & !MAT) | (mvc & !MVC) | (scl & !SCL) | (vec & !VEC) | (vls & !VLS)) {F}
  else if (atm & dtf) {base::all(base::apply(X, 2, base::is.atomic))}
  else if (atm & vls) {base::all(base::sapply(X, base::is.atomic))}
  else if (atm) {base::is.atomic(X)}
  else {uj::run("uj:::.", base::toupper(BBB), "(X)")}
}

#' @rdname bbb_ccc
#' @export
bbb_ccc_funs <- function() {base::sort(base::c("atm_arr", "atm_dtf", "atm_gen", "atm_mat", "atm_mvc", "atm_scl", "atm_vec", "atm_vls",
                                               "nil_arr", "nil_dtf", "nil_gen", "nil_mat",                       "nil_vec", "nil_vls",
                                               "pop_arr", "pop_dtf", "pop_gen", "pop_mat", "pop_mvc", "pop_scl", "pop_vec", "pop_vls"))}

#' @rdname bbb_ccc
#' @export
atm_arr <- function(X, ...) {uj::bbb_ccc(X, 'atm', 'arr', ...)}

#' @rdname bbb_ccc
#' @export
atm_dtf <- function(X, ...) {uj::bbb_ccc(X, 'atm', 'dtf', ...)}

#' @rdname bbb_ccc
#' @export
atm_gen <- function(X, ...) {uj::bbb_ccc(X, 'atm', 'gen', ...)}

#' @rdname bbb_ccc
#' @export
atm_mat <- function(X, ...) {uj::bbb_ccc(X, 'atm', 'mat', ...)}

#' @rdname bbb_ccc
#' @export
atm_mvc <- function(X, ...) {uj::bbb_ccc(X, 'atm', 'mvc', ...)}

#' @rdname bbb_ccc
#' @export
atm_scl <- function(X, ...) {uj::bbb_ccc(X, 'atm', 'scl', ...)}

#' @rdname bbb_ccc
#' @export
atm_vec <- function(X, ...) {uj::bbb_ccc(X, 'atm', 'vec', ...)}

#' @rdname bbb_ccc
#' @export
atm_vls <- function(X, ...) {uj::bbb_ccc(X, 'atm', 'vls', ...)}

#' @rdname bbb_ccc
#' @export
nil_arr <- function(X, ...) {uj::bbb_ccc(X, "nil", "arr", ...)}

#' @rdname bbb_ccc
#' @export
nil_dtf <- function(X, ...) {uj::bbb_ccc(X, "nil", "dtf", ...)}

#' @rdname bbb_ccc
#' @export
nil_gen <- function(X, ...) {uj::bbb_ccc(X, "nil", "gen", ...)}

#' @rdname bbb_ccc
#' @export
nil_mat <- function(X, ...) {uj::bbb_ccc(X, "nil", "mat", ...)}

#' @rdname bbb_ccc
#' @export
nil_vec <- function(X, ...) {uj::bbb_ccc(X, "nil", "vec", ...)}

#' @rdname bbb_ccc
#' @export
nil_vls <- function(X, ...) {uj::bbb_ccc(X, "nil", "vls", ...)}

#' @rdname bbb_ccc
#' @export
pop_arr <- function(X, ...) {uj::bbb_ccc(X, 'pop', 'arr', ...)}

#' @rdname bbb_ccc
#' @export
pop_dtf <- function(X, ...) {uj::bbb_ccc(X, 'pop', 'dtf', ...)}

#' @rdname bbb_ccc
#' @export
pop_gen <- function(X, ...) {uj::bbb_ccc(X, 'pop', 'gen', ...)}

#' @rdname bbb_ccc
#' @export
pop_mat <- function(X, ...) {uj::bbb_ccc(X, 'pop', 'mat', ...)}

#' @rdname bbb_ccc
#' @export
pop_mvc <- function(X, ...) {uj::bbb_ccc(X, 'pop', 'mvc', ...)}

#' @rdname bbb_ccc
#' @export
pop_scl <- function(X, ...) {uj::bbb_ccc(X, 'pop', 'scl', ...)}

#' @rdname bbb_ccc
#' @export
pop_vec <- function(X, ...) {uj::bbb_ccc(X, 'pop', 'vec', ...)}

#' @rdname bbb_ccc
#' @export
pop_vls <- function(X, ...) {uj::bbb_ccc(X, 'pop', 'vls', ...)}
