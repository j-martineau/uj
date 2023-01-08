#' @encoding UTF-8
#' @family props
#' @title basic + xclass combination properties
#' @description \tabular{rl}{
#'     `bbb_ccc_funs`   \tab What \link[=bbb]{basic} + \link[=ccc]{xclass} combination \link[=prop_funs]{property functions} are there?
#'   \cr                \tab  
#'   \cr    `bbb_ccc`   \tab Is `x` a match to the single basic and xclass properties in `bbb` and `ccc`, respectively?
#'   \cr                \tab  
#'   \cr    `BBB_CCC`   \tab Is `x` a match to single basic and xclass properties `'BBB'` and `'CCC'`, respectively?
#' }
#' Some combinations of basic + xclass properties are non-sensical. For this reason, the basic properties represented in this family of functions are `c('atm', 'nil',  'pop')`.
#' \cr
#' \cr In addition, the base property `'nil'` is nonsensical in combination with xclasses `'mvc'` and `'scl'` (which thus do not have combined `BBB_CCC` property functions).
#' @param x An R object.
#' @param bbb A character scalar single basic property from \code{\link{bbb_props}()}.
#' @param ccc A character scalar single xclass property from \code{\link{ccc_props}()}.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return *A character vector*
#'  \cr   `bbb_ccc_funs`
#'  \cr
#'  \cr *A logical scalar*
#'  \cr   `BBB_CCC`
#'  \cr   `bbb_ccc`
#' @examples
#' bbb_ccc_funs()
#' bbb_ccc_props()
#' bbb_ccc(letters, "atm", "mvc")
#' bbb_ccc(1, "nil", "vec")
#' atm_gen(letters)
#' atm_scl(1)
#' @export
bbb_ccc <- function(x, bbb, ccc, ...) {
  errs <- c(uj:::.meets_errs(x, ...),
            uj::f0(uj::isIN(bbb, .bbbs), NULL, "[bbb] is not a scalar value from bbb_props()."),
            uj::f0(uj::isIN(ccc, .cccs), NULL, "[ccc] is not a scalar value from ccc_props()."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  arr <- ccc == "arr"; ARR <- uj::iarr(x)
  dtf <- ccc == "dtf"; DTF <- uj::idtf(x)
  gen <- ccc == "gen"; GEN <- uj::igen(x)
  mat <- ccc == "mat"; MAT <- uj::imat(x)
  mvc <- ccc == "mvc"; MVC <- uj::imvc(x)
  scl <- ccc == "scl"; SCL <- uj::iscl(x)
  vec <- ccc == "vec"; VEC <- uj::ivec(x)
  vls <- ccc == "vls"; VLS <- uj::ivls(x)
  pop <- bbb == "pop"; POP <- uj::f0(DTF, base::NROW(x) * base::NCOL(x) > 0, base::length(x) > 0)
  nil <- bbb == "nil"; NIL <- !POP
  atm <- bbb == "atm"
  if (!uj::meets(x, ...)) {F}
  else if ((pop & !POP) | (nil & !NIL) | (arr & !ARR) | (dtf & !DTF) | (gen & !GEN) | (mat & !MAT) | (mvc & !MVC) | (scl & !SCL) | (vec & !VEC) | (vls & !VLS)) {F}
  else if (atm & dtf) {base::all(base::apply(x, 2, base::is.atomic))}
  else if (atm & vls) {base::all(base::sapply(x, base::is.atomic))}
  else if (atm) {base::is.atomic(x)}
  else {uj::run("uj::i", bbb)}
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
