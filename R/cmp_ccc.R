#' @name cmp_ccc.
#' @family props
#' @title Complete + atomic extended class properties
#' @param x. An object.
#' @param xxx. A character scalar containing one or more values from
#'   \code{ccc_vals()} separated by pipes and/or underscores. Combinations of
#'   extended classes can be specified by separating them with underscores.
#'   Separating extended classes or combinations of extended classes with pipes
#'   will result in a value of \code{TRUE} if any of them applies to \code{x.}.
#' @param ... Additional arguments to \code{\link{meets}} containing value and
#'   element/row/column count restrictions.
#' @section Submitting additional arguments to \code{ccc} via \code{...}:
#'   Allows for checking not just the ccc but whether length, number of rows,
#'   number of columns, and element values meet flexible criteria.
#' @return \code{ccc_vals} returns a character vector containing all valid
#'   extended class property values. \code{ccc} returns a character scalar
#'   or vector containing all extended class properties from
#'   \code{ccc_vals()} applicable to \code{x.}. All others return either
#'   \code{TRUE} or \code{FALSE}.
#' @export
cmp_ccc. <- function() {help("cmp_ccc.", package = "uj")}

#' @describeIn cmp_ccc. Get a character vector of all possible complete +
#'   atomic extended class properties.
#' @export
cmp_ccc_vals <- function() {
  x. <- c('cmp_arr', 'cmp_gen', 'cmp_tab', 'cmp_vls', 'cmp_vtp', 'cmp_mat', 'cmp_mvc', 'cmp_scl', 'cmp_vec')
  names(x.) <- rep.int("ccc", length(x.))
  x.
}

#' @describeIn cmp_ccc. Is \code{x.} a complete atomic array?
#' @export
cmp_arr <- function(x.) {atm_arr(x.) & !any(is.na(av(x.)))}

#' @describeIn cmp_ccc. Is \code{x.} a complete atomic generic?
#' @export
cmp_gen <- function(x.) {atm_gen(x.) & !any(is.na(av(x.)))}

#' @describeIn cmp_ccc. Is \code{x.} a complete \code{\link[itab]{atomic df}}?
#' @export
cmp_dtf <- function(x.) {atm_dtf(x.) & !any(is.na(av(x.)))}

#' @describeIn cmp_ccc. Is \code{x.} a complete atomic vlist?
#' @export
cmp_vls <- function(x.) {atm_vls(x.) & !any(is.na(av(x.)))}

#' @describeIn cmp_ccc. Is \code{x.} a complete atomic vtype?
#' @export
cmp_vtp <- function(x.) {atm_vtp(x.) & !any(is.na(av(x.)))}

#' @describeIn cmp_ccc. Is \code{x.} a complete atomic matrix?
#' @export
cmp_mat <- function(x.) {atm_mat(x.) & !any(is.na(av(x.)))}

#' @describeIn cmp_ccc. Is \code{x.} a complete atomic mvect?
#' @export
cmp_mvc <- function(x.) {atm_mvc(x.) & !any(is.na(av(x.)))}

#' @describeIn cmp_ccc. Is \code{x.} a complete atomic scalar?
#' @export
cmp_scl <- function(x.) {atm_scl(x.) & !any(is.na(av(x.)))}

#' @describeIn cmp_ccc. Is \code{x.} a complete atomic vect?
#' @export
cmp_vec <- function(x.) {atm_vec(x.) & !any(is.na(av(x.)))}

