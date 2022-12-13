#' @family props
#' @title Complete + `xclass` properties
#' @description \tabular{rl}{
#'     `cmp_ccc_props`   \tab Gets all possible \link[=icmp]{complete} + \code{\link[=ccc]{xclass}} properties.
#'   \cr                 \tab   
#'   \cr     `cmp_ccc`   \tab Is `x` complete and does it match `xclass` spec `ccc`?
#'   \cr                 \tab   
#'   \cr     `cmp_CCC`   \tab Is `x` complete and does it match `xclass` property `'CCC'`?
#' }
#' @param x An R object.
#' @param spec A character scalar `xclass` spec built from values in `ccc_props()`.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return \tabular{rl}{
#'     `cmp_ccc_props`   \tab A character vector.
#'   \cr     `cmp_ccc`   \tab A logical scalar.
#'   \cr     `cmp_CCC`   \tab A logical scalar.
#' }
#' @examples
#' cmp_ccc_props()
#' cmp_ccc(letters, "mvc")
#' cmp_ccc(1, "scl")
#' cmp_ccc(NA, "gen")
#' cmp_scl(1)
#' cmp_mvc(letters)
#' @export
cmp_ccc <- function(x, spec, ...) {
  errs <- c(.meets_errs(x, ...), f0(f0(length(spec) != 1 | !is.character(spec), F, f0(is.na(spec), F, spec %in% .cccs)), NULL, '[ccc] is not a scalar value from ccc_props().'))
  if (!is.null(errs)) {stop(.errs(errs))}
  if (!iccc(x, spec, ...)) {F} else {x <- av(x); !any(is.na(x))}
}

#' @rdname cmp_ccc
#' @export
cmp_ccc_props <- function() {paste0('cmp_', .cccs)}

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
