#' @name cmp_mmm_agn
#' @family props
#' @title Complete + Extended Mode + Atomic Generic (cmp + mmm + agn)
#' @description Get all possible complete + extended mode + atomic generic
#'   properties.
#' @details See \code{\link{mmm}}, and \code{\link{cmp_gen}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}.
#' @export
cmp_mmm_agn_vals <- function() {
  x <- paste0("cmp_", c("atm", mmm_vals()), "_agn")
  names(x) <- rep.int("cmp_mmm_agn", length(x))
  x
}

#' @describeIn cmp_mmm_agn Is \code{x} a complete atomic generic?
#' @export
cmp_atm_agn <- function(x) {cmp_gen(x) & is.atomic(x)}

#' @describeIn cmp_mmm_agn Is \code{x} a complete character atomic generic?
#' @export
cmp_chr_agn <- function(x) {cmp_gen(x) & is.character(x)}

#' @describeIn cmp_mmm_agn Is \code{x} a complete onechar atomic generic?
#' @export
cmp_ch1_agn <- function(x) {cmp_gen(x) & is_ch1(x)}

#' @describeIn cmp_mmm_agn Is \code{x} a complete color atomic generic?
#' @export
cmp_clr_agn <- function(x) {cmp_gen(x) & is_clr(x)}

#' @describeIn cmp_mmm_agn Is \code{x} a complete even-numeric atomic generic?
#' @export
cmp_evn_agn <- function(x) {cmp_gen(x, "evn")}

#' @describeIn cmp_mmm_agn Is \code{x} a complete factor atomic generic?
#' @export
cmp_fac_agn <- function(x) {cmp_gen(x, "fac")}

#' @describeIn cmp_mmm_agn Is \code{x} a complete fractional-numeric atomic generic?
#' @export
cmp_frc_agn <- function(x) {cmp_gen(x, "frc")}

#' @describeIn cmp_mmm_agn Is \code{x} a complete indexer atomic generic?
#' @export
cmp_ind_agn <- function(x) {cmp_gen(x, "ind")}

#' @describeIn cmp_mmm_agn Is \code{x} a complete logical atomic generic?
#' @export
cmp_lgl_agn <- function(x) {cmp_gen(x, "lgl")}

#' @describeIn cmp_mmm_agn Is \code{x} a complete negative-numeric atomic generic?
#' @export
cmp_neg_agn <- function(x) {cmp_gen(x, "neg")}

#' @describeIn cmp_mmm_agn Is \code{x} a complete negative-whole-numeric atomic generic?
#' @export
cmp_ngw_agn <- function(x) {cmp_gen(x, "ngw")}

#' @describeIn cmp_mmm_agn Is \code{x} a complete non-negative-numeric atomic generic?
#' @export
cmp_nng_agn <- function(x) {cmp_gen(x, "nng")}

#' @describeIn cmp_mmm_agn Is \code{x} a complete non-negative-whole-numeric atomic generic?
#' @export
cmp_nnw_agn <- function(x) {cmp_gen(x, "nnw")}

#' @describeIn cmp_mmm_agn Is \code{x} a complete non-positive-numeric atomic generic?
#' @export
cmp_nps_agn <- function(x) {cmp_gen(x, "nps")}

#' @describeIn cmp_mmm_agn Is \code{x} a complete non-positive-whole-numeric atomic generic?
#' @export
cmp_npw_agn <- function(x) {cmp_gen(x, "npw")}

#' @describeIn cmp_mmm_agn Is \code{x} a complete non-sortable atomic generic?
#' @export
cmp_nst_agn <- function(x) {cmp_gen(x, "nst")}

#' @describeIn cmp_mmm_agn Is \code{x} a complete numeric atomic generic?
#' @export
cmp_num_agn <- function(x) {cmp_gen(x, "num")}

#' @describeIn cmp_mmm_agn Is \code{x} a complete odd-numeric atomic generic?
#' @export
cmp_odd_agn <- function(x) {cmp_gen(x, "odd")}

#' @describeIn cmp_mmm_agn Is \code{x} a complete ordered-factor atomic generic?
#' @export
cmp_ord_agn <- function(x) {cmp_gen(x, "ord")}

#' @describeIn cmp_mmm_agn Is \code{x} a complete percent-numeric atomic generic?
#' @export
cmp_pct_agn <- function(x) {cmp_gen(x, "pct")}

#' @describeIn cmp_mmm_agn Is \code{x} a complete positive-numeric atomic generic?
#' @export
cmp_pos_agn <- function(x) {cmp_gen(x, "pos")}

#' @describeIn cmp_mmm_agn Is \code{x} a complete proportion-numeric atomic generic?
#' @export
cmp_ppn_agn <- function(x) {cmp_gen(x, "ppn")}

#' @describeIn cmp_mmm_agn Is \code{x} a complete positive-whole-numeric atomic generic?
#' @export
cmp_psw_agn <- function(x) {cmp_gen(x, "psw")}

#' @describeIn cmp_mmm_agn Is \code{x} a complete sortable atomic generic?
#' @export
cmp_srt_agn <- function(x) {cmp_gen(x, "srt")}

#' @describeIn cmp_mmm_agn Is \code{x} a complete string atomic generic?
#' @export
cmp_str_agn <- function(x) {cmp_gen(x, "str")}

#' @describeIn cmp_mmm_agn Is \code{x} a complete unordered-factor atomic generic?
#' @export
cmp_uno_agn <- function(x) {cmp_gen(x, "uno")}

#' @describeIn cmp_mmm_agn Is \code{x} a complete whole-numeric atomic generic?
#' @export
cmp_whl_agn <- function(x) {cmp_gen(x, "whl")}
