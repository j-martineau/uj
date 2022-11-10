#' @name cmp_ccc.
#' @family props
#' @title Complete plus atomic extended class properties
#' @description Combinations of \link[icmp]{completeness} and
#'   \link[ccc]{extended class}.
#' @param x An object.
#' @section Submitting additional arguments to \code{ccc} via \code{...}: Allows
#'   for checking not just the extended class but whether length, number of
#'   rows, number of columns, and element values meet flexible criteria.
#' @return \code{ccc_vals} returns a character vector containing all valid
#'   extended class property values. \code{ccc} returns a character scalar or
#'   vector containing all extended class properties from \code{ccc_vals()}
#'   applicable to \code{x}. All others return either \code{TRUE} or
#'   \code{FALSE}.
#' @export
cmp_ccc. <- function() {help("cmp_ccc.", package = "uj")}

#' @describeIn cmp_ccc. Get a character vector of all possible completeness plus
#'   atomic extended class properties.
#' @export
cmp_ccc_vals <- function() {
  x <- c('cmp_arr', 'cmp_gen', 'cmp_tab', 'cmp_vls', 'cmp_vtp', 'cmp_mat', 'cmp_mvc', 'cmp_scl', 'cmp_vec')
  names(x) <- rep.int("ccc", length(x))
  x
}

#' @describeIn cmp_ccc. Is \code{x} a \link[icmp]{complete} \link[iarr]{atomic
#'   array}?
#' @export
cmp_arr <- function(x) {atm_arr(x) & !any(is.na(av(x)))}

#' @describeIn cmp_ccc. Is \code{x} a \link[icmp]{complete} \link[igen]{atomic
#'   generic}?
#' @export
cmp_gen <- function(x) {atm_gen(x) & !any(is.na(av(x)))}

#' @describeIn cmp_ccc. Is \code{x} a \link[icmp]{complete} \link[idtf]{atomic
#'   dtf}?
#' @export
cmp_dtf <- function(x) {atm_dtf(x) & !any(is.na(av(x)))}

#' @describeIn cmp_ccc. Is \code{x} a \link[icmp]{complete} \link[ivls]{atomic
#'   vlist}?
#' @export
cmp_vls <- function(x) {atm_vls(x) & !any(is.na(av(x)))}

#' @describeIn cmp_ccc. Is \code{x} a \link[icmp]{complete} \link[ivtp]{atomic
#'   vtype}?
#' @export
cmp_vtp <- function(x) {atm_vtp(x) & !any(is.na(av(x)))}

#' @describeIn cmp_ccc. Is \code{x} a \link[icmp]{complete} \link[imat]{atomic
#'   matrix}?
#' @export
cmp_mat <- function(x) {atm_mat(x) & !any(is.na(av(x)))}

#' @describeIn cmp_ccc. Is \code{x} a \link[icmp]{complete} \link[imvc]{atomic
#'   multivec}?
#' @export
cmp_mvc <- function(x) {atm_mvc(x) & !any(is.na(av(x)))}

#' @describeIn cmp_ccc. Is \code{x} a \link[icmp]{complete} \link[iscl]{atomic
#'   scalar}?
#' @export
cmp_scl <- function(x) {atm_scl(x) & !any(is.na(av(x)))}

#' @describeIn cmp_ccc. Is \code{x} a \link[icmp]{complete} \link[ivec]{atomic
#'   vec}?
#' @export
cmp_vec <- function(x) {atm_vec(x) & !any(is.na(av(x)))}

