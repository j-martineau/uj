#' @encoding UTF-8
#' @family properties
#' @title Completeness + xclass combination properties
#' @description Check for combinations of \link[=CMP]{completeness} and \link[=ccc]{xclass}.
#' @details
#' \tabular{ll}{  `cmp_ccc_funs`   \tab What complete + xclass combination property functions are there?                                                                       \cr   \tab  }
#' \tabular{ll}{  `cmp_{ccc}`      \tab Is `x` both complete and a match to the single xclass property `'{ccc}'` where `{ccc}` is a placeholder for any given xclass property? \cr   \tab  }
#' \tabular{ll}{  `cmp_ccc`        \tab Is `x` both complete and a match to the single xclass property in `ccc`?                                                               \cr   \tab  }
#' @param x An R object.
#' @param ccc A character scalar single xclass property from \code{\link{ccc_props}()}.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return **A character vector** \cr `cmp_ccc_funs`
#' \cr\cr  **A logical scalar**   \cr `cmp_ccc, cmp_{ccc}`
#' @examples
#' cmp_ccc_funs()
#' cmp_ccc(letters, "mvc")
#' cmp_ccc(1, "scl")
#' cmp_ccc(NA, "gen")
#' cmp_mvc(letters)
#' cmp_scl(1)
#'
#' @export
cmp_ccc <- function(x, ccc, ...) {
  cfun <- function(cx) {uj::run("uj::i", base::toupper(ccc), "(cx)")}
  afun <- function(ax) {uj::f0(uj::notATM(ax), F, uj::f0(uj::N0(ax), F, uj::noneNA(ax)))}
  dfun <- function(dx) {base::all(base::apply(dx, 2, afun))}
  vfun <- function(vx) {base::all(base::sapply(vx, afun))}
  if (uj::isCHR(ccc)) {ccc <- base::tolower(ccc)}
  uj::errs_if_pop(base::c(uj:::.meets_errs(x, ...), uj::f0(uj::isIN1(ccc, uj:::.ccc), NULL, '[ccc] is not a scalar value from ccc_props().')), PKG = "uj")
  uj::f0(!uj::meets(x, ...), F, uj::f0(!cfun(x), F, uj::f0(ccc == "dtf", dfun(x), uj::f0(ccc == "vls", vfun(x), afun(x)))))
}

#' @rdname cmp_ccc
#' @export
cmp_ccc_funs <- function() {uj::p0('cmp', base::toupper(uj:::.cccs))}

#' @rdname cmp_ccc
#' @export
cmp_arr <- function(x, ...) {uj::cmp_ccc(x, 'arr', ...)}

#' @rdname cmp_ccc
#' @export
cmp_dtf <- function(x, ...) {uj::cmp_ccc(x, 'dtf', ...)}

#' @rdname cmp_ccc
#' @export
cmp_gen <- function(x, ...) {uj::cmp_ccc(x, 'gen', ...)}

#' @rdname cmp_ccc
#' @export
cmp_mat <- function(x, ...) {uj::cmp_ccc(x, 'mat', ...)}

#' @rdname cmp_ccc
#' @export
cmp_mvc <- function(x, ...) {uj::cmp_ccc(x, 'mvc', ...)}

#' @rdname cmp_ccc
#' @export
cmp_scl <- function(x, ...) {uj::cmp_ccc(x, 'scl', ...)}

#' @rdname cmp_ccc
#' @export
cmp_vec <- function(x, ...) {uj::cmp_ccc(x, 'vec', ...)}

#' @rdname cmp_ccc
#' @export
cmp_vls <- function(x, ...) {uj::cmp_ccc(x, 'vls', ...)}
