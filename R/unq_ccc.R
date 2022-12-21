#' @encoding UTF-8
#' @family properties
#' @title unique + xclass combination properties
#' @description \tabular{rl}{
#'     `unq_ccc_funs`   \tab What \link[=iunq]{unique} + \link[=ccc]{xclass} combination \link[=prop_funs]{property functions} are there?
#'   \cr                \tab  
#'   \cr    `unq_ccc`   \tab Is `x` both unique and a match to the single xclass property in argument `ccc`?
#'   \cr                \tab  
#'   \cr    `unq_CCC`   \tab Is `x` both unique and a match to the single xclass property `'CCC'`?
#' }
#' @param x An R object.
#' @param ccc A character scalar single xclass property from `ccc_props()`.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return *A character vector*
#'   \cr    `unq_ccc_funs`
#'   \cr
#'   \cr *A logical scalar*
#'   \cr    `unq_ccc`
#'   \cr    `unq_CCC`
#' @examples
#' unq_ccc_fun()
#' unq_ccc(letters, "mvc")
#' unq_ccc(1, "scl")
#' unq_ccc(NA, "gen")
#' unq_mvc(letters)
#' unq_scl(1)
#' @export
unq_ccc <- function(x, ccc, ...) {
  cfun <- function(cx) {run("i", ccc, "(cx)")}
  afun <- function(ax) {f0(!is.atomic(ax), F, f0(length(ax) == 0, F, f0(any(is.na(ax)), F, length(ax) == length(unique(ax)))))}
  dfun <- function(dx) {all(apply(dx, 2, afun))}
  vfun <- function(vx) {all(sapply(vx, afun))}
  errs <- c(.meets_errs(x, ...), f0(f0(length(ccc) != 1 | !is.character(ccc), F, f0(is.na(ccc), F, ccc %in% .cccs)), NULL, '[ccc] is not a scalar value from ccc_props().'))
  if (!is.null(errs)) {stop(.errs(errs))}
  f0(!meets(x, ...), F, f0(!cfun(x), F, f0(ccc == "dtf", dfun(x),  f0(ccc == "vls", vfun(x), afun(x)))))
}

#' @rdname unq_ccc
#' @export
unq_ccc_funs <- function() {paste0('unq_', .cccs)}

#' @rdname unq_ccc
#' @export
unq_arr <- function(x, ...) {unq_ccc(x, 'arr', ...)}

#' @rdname unq_ccc
#' @export
unq_dtf <- function(x, ...) {unq_ccc(x, 'dtf', ...)}

#' @rdname unq_ccc
#' @export
unq_gen <- function(x, ...) {unq_ccc(x, 'gen', ...)}

#' @rdname unq_ccc
#' @export
unq_mat <- function(x, ...) {unq_ccc(x, 'mat', ...)}

#' @rdname unq_ccc
#' @export
unq_mvc <- function(x, ...) {unq_ccc(x, 'mvc', ...)}

#' @rdname unq_ccc
#' @export
unq_scl <- function(x, ...) {unq_ccc(x, 'scl', ...)}

#' @rdname unq_ccc
#' @export
unq_vec <- function(x, ...) {unq_ccc(x, 'vec', ...)}

#' @rdname unq_ccc
#' @export
unq_vls <- function(x, ...) {unq_ccc(x, 'vls', ...)}
