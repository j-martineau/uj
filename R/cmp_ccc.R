#' @family props
#' @title Completeness + Extended Class Properties
#' @description \tabular{ll}{
#'   FUNCTION          \tab WHAT IT DOES                                     \cr
#'   `cmp_ccc`         \tab Evaluate whether `x` is complete and matches the
#'                          extended class specified in the argument `ccc`
#'                          subject to any restrictions in `...`.            \cr
#'   `cmp_CCC`         \tab Evaluates whether `x` is complete and matches the
#'                          extended class property `CCC` subject to any
#'                          restrictions in `...`, where `CCC` is a placeholder
#'                          for any given extended class property.           \cr
#'   `cmp_ccc_props`   \tab Gets a character vector of all possible completeness
#'                          + extended class properties.                       }
#' @param x An R object.
#' @param ccc A character scalar extended class property from `ccc_props()`.
#' @inheritDotParams meets
#' @inheritSection meets Specifying Count and Value Restrictions
#' @return \tabular{ll}{
#'   FUNCTION               \tab WHAT IT DOES                                \cr
#'   `cmp_ccc_props`        \tab A character vector.                         \cr
#'   `cmp_ccc`, `cmp_CCC`   \tab A logical scalar.                             }
#' @export
cmp_ccc <- function(x, ccc, ...) {
  errs <- c(.meets_errs(x, ...),
            f0(f0(length(ccc) != 1 | !is.character(ccc), F, f0(is.na(ccc), F, ccc %in% .cccs)), NULL, '\n \u2022 [ccc] is not a scalar value from ccc_props().'))
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
