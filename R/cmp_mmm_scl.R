#' @name cmp_mmm_scl
#' @family props
#' @title Complete + Extended Mode + Atomic Scalar (cmp + mmm + scl)
#' @description Get all possible complete + extended mode + atomic scalar
#'   properties.
#' @details See \code{\link{mmm}}, and \code{\link{cmp_scl}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}.
#' @export
cmp_mmm_scl_vals <- function() {
  x <- paste0("cmp_", c("atm", mmm_vals()), "_scl")
  names(x) <- rep.int("cmp_mmm_scl", length(x))
  x
}

#' @describeIn cmp_mmm_scl Is \code{x} a complete atomic scalar?
#' @export
cmp_atm_scl <- function(x) {cmp_scl(x) & iatm(x)}

#' @describeIn cmp_mmm_scl Is \code{x} a complete character atomic scalar?
#' @export
cmp_chr_scl <- function(x) {cmp_scl(x, "chr")}

#' @describeIn cmp_mmm_scl Is \code{x} a complete onechar atomic scalar?
#' @export
cmp_ch1_scl <- function(x) {cmp_scl(x, "ch1")}

#' @describeIn cmp_mmm_scl Is \code{x} a complete color atomic scalar?
#' @export
cmp_clr_scl <- function(x) {cmp_scl(x, "clr")}

#' @describeIn cmp_mmm_scl Is \code{x} a complete even-numeric atomic scalar?
#' @export
cmp_evn_scl <- function(x) {cmp_scl(x, "evn")}

#' @describeIn cmp_mmm_scl Is \code{x} a complete factor atomic scalar?
#' @export
cmp_fac_scl <- function(x) {cmp_scl(x, "fac")}

#' @describeIn cmp_mmm_scl Is \code{x} a complete fractional-numeric atomic
#'   scalar?
#' @export
cmp_frc_scl <- function(x) {cmp_scl(x, "frc")}

#' @describeIn cmp_mmm_scl Is \code{x} a complete indexer atomic scalar?
#' @export
cmp_ind_scl <- function(x) {cmp_scl(x, "ind")}

#' @describeIn cmp_mmm_scl Is \code{x} a complete logical atomic scalar?
#' @export
cmp_lgl_scl <- function(x) {cmp_scl(x, "lgl")}

#' @describeIn cmp_mmm_scl Is \code{x} a complete negative-numeric atomic
#'   scalar?
#' @export
cmp_neg_scl <- function(x) {cmp_scl(x, "neg")}

#' @describeIn cmp_mmm_scl Is \code{x} a complete negative-whole-numeric atomic
#'   scalar?
#' @export
cmp_ngw_scl <- function(x) {cmp_scl(x, "ngw")}

#' @describeIn cmp_mmm_scl Is \code{x} a complete non-negative-numeric atomic
#'   scalar?
#' @export
cmp_nng_scl <- function(x) {cmp_scl(x, "nng")}

#' @describeIn cmp_mmm_scl Is \code{x} a complete non-negative-whole-numeric
#'   atomic scalar?
#' @export
cmp_nnw_scl <- function(x) {cmp_scl(x, "nnw")}

#' @describeIn cmp_mmm_scl Is \code{x} a complete non-positive-numeric atomic
#'   scalar?
#' @export
cmp_nps_scl <- function(x) {cmp_scl(x, "nps")}

#' @describeIn cmp_mmm_scl Is \code{x} a complete non-positive-whole-numeric
#'   atomic scalar?
#' @export
cmp_npw_scl <- function(x) {cmp_scl(x, "npw")}

#' @describeIn cmp_mmm_scl Is \code{x} a complete non-sortable atomic scalar?
#' @export
cmp_nst_scl <- function(x) {cmp_scl(x, "nst")}

#' @describeIn cmp_mmm_scl Is \code{x} a complete numeric atomic scalar?
#' @export
cmp_num_scl <- function(x) {cmp_scl(x, "num")}

#' @describeIn cmp_mmm_scl Is \code{x} a complete odd-numeric atomic scalar?
#' @export
cmp_odd_scl <- function(x) {cmp_scl(x, "odd")}

#' @describeIn cmp_mmm_scl Is \code{x} a complete ordered-factor atomic scalar?
#' @export
cmp_ord_scl <- function(x) {cmp_scl(x, "ord")}

#' @describeIn cmp_mmm_scl Is \code{x} a complete percent-numeric atomic scalar?
#' @export
cmp_pct_scl <- function(x) {cmp_scl(x, "pct")}

#' @describeIn cmp_mmm_scl Is \code{x} a complete positive-numeric atomic
#'   scalar?
#' @export
cmp_pos_scl <- function(x) {cmp_scl(x, "pos")}

#' @describeIn cmp_mmm_scl Is \code{x} a complete proportion-numeric atomic
#'   scalar?
#' @export
cmp_ppn_scl <- function(x) {cmp_scl(x, "ppn")}

#' @describeIn cmp_mmm_scl Is \code{x} a complete positive-whole-numeric atomic
#'   scalar?
#' @export
cmp_psw_scl <- function(x) {cmp_scl(x, "psw")}

#' @describeIn cmp_mmm_scl Is \code{x} a complete sortable atomic scalar?
#' @export
cmp_srt_scl <- function(x) {cmp_scl(x, "srt")}

#' @describeIn cmp_mmm_scl Is \code{x} a complete string atomic scalar?
#' @export
cmp_str_scl <- function(x) {cmp_scl(x, "str")}

#' @describeIn cmp_mmm_scl Is \code{x} a complete unordered-factor atomic
#'   scalar?
#' @export
cmp_uno_scl <- function(x) {cmp_scl(x, "uno")}

#' @describeIn cmp_mmm_scl Is \code{x} a complete whole-numeric atomic scalar?
#' @export
cmp_whl_scl <- function(x) {cmp_scl(x, "whl")}
