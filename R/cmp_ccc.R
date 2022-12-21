#' @encoding UTF-8
#' @family properties
#' @title complete + xclass combination properties
#' @description \tabular{rl}{
#'     `cmp_ccc_funs`   \tab What complete + xclass combination property functions are there?
#'   \cr                \tab  
#'   \cr    `cmp_CCC`   \tab Is `x` both complete and a match to the single xclass property `'CCC'`?
#'   \cr                \tab  
#'   \cr    `cmp_ccc`   \tab Is `x` both complete and a match to the single xclass property in `ccc`?
#' }
#' @param x An R object.
#' @param ccc A character scalar single xclass property from \code{\link{ccc_props}()}.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return *A character vector*
#'  \cr    `cmp_ccc_funs`
#'  \cr\cr *A logical scalar*
#'  \cr    `cmp_CCC`
#'  \cr    `cmp_ccc`
#' @examples
#' cmp_ccc_fun()
#' cmp_ccc(letters, "mvc")
#' cmp_ccc(1, "scl")
#' cmp_ccc(NA, "gen")
#' cmp_mvc(letters)
#' cmp_scl(1)
#' @export
cmp_ccc <- function(x, ccc, ...) {
  cfun <- function(cx) {run("i", ccc, "(cx)")}
  afun <- function(ax) {f0(!is.atomic(ax), F, f0(length(ax) == 0, F, !any(is.na(ax))))}
  dfun <- function(dx) {all(apply(dx, 2, afun))}
  vfun <- function(vx) {all(sapply(vx, afun))}
  errs <- c(.meets_errs(x, ...), f0(f0(length(ccc) != 1 | !is.character(ccc), F, f0(is.na(ccc), F, ccc %in% .cccs)), NULL, '[ccc] is not a scalar value from ccc_props().'))
  if (!is.null(errs)) {stop(.errs(errs))}
  f0(!meets(x, ...), F, f0(!cfun(x), F, f0(ccc == "dtf", dfun(x),  f0(ccc == "vls", vfun(x), afun(x)))))
}

#' @rdname cmp_ccc
#' @export
cmp_ccc_funs <- function() {paste0('cmp_', .cccs)}

#' @rdname cmp_ccc
#' @export
cmp_arr <- function(x, ...) {cmp_ccc(x, 'arr', ...)}

#' @rdname cmp_ccc
#' @export
cmp_dtf <- function(x, ...) {cmp_ccc(x, 'dtf', ...)}

#' @rdname cmp_ccc
#' @export
cmp_gen <- function(x, ...) {cmp_ccc(x, 'gen', ...)}

#' @rdname cmp_ccc
#' @export
cmp_mat <- function(x, ...) {cmp_ccc(x, 'mat', ...)}

#' @rdname cmp_ccc
#' @export
cmp_mvc <- function(x, ...) {cmp_ccc(x, 'mvc', ...)}

#' @rdname cmp_ccc
#' @export
cmp_scl <- function(x, ...) {cmp_ccc(x, 'scl', ...)}

#' @rdname cmp_ccc
#' @export
cmp_vec <- function(x, ...) {cmp_ccc(x, 'vec', ...)}

#' @rdname cmp_ccc
#' @export
cmp_vls <- function(x, ...) {cmp_ccc(x, 'vls', ...)}
