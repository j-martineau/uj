#' @name cmp_ccc
#' @family props
#' @title Completeness + Extended class Properties
#' @description NOTE: \code{CCC} is used as a wildcard for any
#'   given extended class property.
#'   \cr\cr
#'   \strong{\code{cmp_ccc}}: Evaluates whether \code{x} is complete and
#'   matches the extended class specified in the argument \code{ccc}
#'   (subject to any restrictions in \code{...}).
#'   \cr\cr
#'   \strong{\code{cmp_CCC}}: Evaluates whether \code{x} is complete and
#'   matches the extended class property \code{CCC}.
#'   (subject to any restrictions in \code{...}).
#'   \cr\cr
#'   \strong{\code{cmp_ccc_props}}: Gets a character vector of all possible
#'   completeness + extended class properties.
#' @param x An R object
#' @param ccc A character scalar containing an
#'   extended class property from ccc_props().
#' @inheritDotParams meets
#' @inheritSection meets Specifying Count and Value Restrictions
#' @return \strong{\code{cmp_ccc_props}}: A character vector.
#'   \cr\cr\strong{cmp_ccc, cmp_CCC}: A logical scalar.
#' @export
cmp_ccc <- function(x, ccc, ...) {
  errs <- c(.meets_errs(x, ...),
            f0(f0(length(ccc) != 1 | !is.character(ccc), F, f0(is.na(ccc), F, ccc %in% ccc_props())), NULL, '\n \u2022 [ccc] is not a scalar value from ccc_props().'))
  if (!is.null(errs)) {stop(errs)}
  if (!iccc(x)) {F} else {x <- av(x); !any(is.na(x))}
}

#' @name cmp_ccc
#' @export
cmp_ccc_props <- function() {paste0('cmp_', ccc_props())}
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
