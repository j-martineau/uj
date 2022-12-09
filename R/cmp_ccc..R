#' @family props
#' @title Completeness + extended class properties
#' @description \tabular{rl}{
#'     `cmp_ccc_props`   \tab Gets a character vector of all possible completeness + extended class properties.
#'   \cr                 \tab  
#'   \cr     `cmp_ccc`   \tab Evaluates whether `x` is complete and matches the extended class in argument `ccc` (subject to any restrictions in `...`).
#'   \cr                 \tab  
#'   \cr     `cmp_xxx`   \tab Evaluates whether `x` is complete and matches property `xxx`\eqn{^1} (subject to any restrictions in `...`).
#' }
#' \eqn{^{1.}} An extended class property.
#' @param x An R object.
#' @param ccc A character scalar extended class property from `ccc_props()`.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return \tabular{rl}{
#'     `cmp_ccc_props`   \tab A character vector.
#'   \cr     `cmp_ccc`   \tab A logical scalar.
#'   \cr     `cmp_xxx`   \tab A logical scalar\eqn{^2}.
#' }
#' \eqn{^{2.}} `xxx` is an extended class property.
#' @examples
#' cmp_ccc_props()
#' cmp_ccc(letters, "mvc")
#' cmp_ccc(1, "scl")
#' cmp_ccc(NA, "gen")
#' cmp_scl(1)
#' cmp_mvc(letters)
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
