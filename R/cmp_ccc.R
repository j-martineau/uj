#' @family props
#' @title Completeness + extended class properties
#' @description \itemize{
#'   \item **`cmp_ccc`**: evaluates whether `x` is complete and matches the extended class specified in the argument `ccc` subject to any restrictions in `...`.
#'   \item **`cmp_xxx`**: evaluates whether `x` is complete and matches extended class `xxx` subject to any restrictions in `...`, where `xxx` is a placeholder for any given extended class property.
#'   \item **`cmp_ccc_props`**: gets a character vector of all possible completeness + extended class properties.
#' }
#' @param x An R object.
#' @param ccc A character scalar extended class property from `ccc_props()`.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return \itemize{
#'   \item **`cmp_ccc_props`**: a character vector.
#'   \item **`cmp_ccc, cmp_CCC`**: a logical scalar.
#' }
#' @export
cmp_ccc <- function(x, ccc, ...) {
  errs <- c(.meets_errs(x, ...), f0(f0(length(ccc) != 1 | !is.character(ccc), F, f0(is.na(ccc), F, ccc %in% .cccs)), NULL, '\n \u2022 [ccc] is not a scalar value from ccc_props().'))
  if (!is.null(errs)) {stop(errs)}
  if (!iccc(x, ccc, ...)) {F} else {x <- av(x); !any(is.na(x))}
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
