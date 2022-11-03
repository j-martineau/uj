#' @name cmp_mmm_avl
#' @family props
#' @title Complete + Extended Mode + Atomic Vlist (cmp + mmm + avl)
#' @details See \code{\link{mmm}}, and \code{\link{cmp_vls}}.
#' @description Get all possible complete + extended mode + atomic vlist
#'   properties.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}.
#' @export
cmp_mmm_avl_vals <- function() {
  x <- paste0("cmp_", c("atm", mmm_vals()), "_avl")
  names(x) <- rep.int("cmp_mmm_avl", length(x))
  x
}

#' @describeIn cmp_mmm_avl Is \code{x} a complete atomic vlist?
#' @export
cmp_atm_avl <- function(x) {cmp_vls(x)}

#' @describeIn cmp_mmm_avl Is \code{x} a complete character atomic vlist?
#' @export
cmp_chr_avl <- function(x) {cmp_vls(x, "chr")}

#' @describeIn cmp_mmm_avl Is \code{x} a complete onechar atomic vlist?
#' @export
cmp_ch1_avl <- function(x) {cmp_vls(x, "ch1")}

#' @describeIn cmp_mmm_avl Is \code{x} a complete color atomic vlist?
#' @export
cmp_clr_avl <- function(x) {cmp_vls(x, "clr")}

#' @describeIn cmp_mmm_avl Is \code{x} a complete even-numbered atomic vlist?
#' @export
cmp_evn_avl <- function(x) {cmp_vls(x, "evn")}

#' @describeIn cmp_mmm_avl Is \code{x} a complete factor atomic vlist?
#' @export
cmp_fac_avl <- function(x) {cmp_vls(x, "fac")}

#' @describeIn cmp_mmm_avl Is \code{x} a complete fractional-numbered atomic
#'   vlist?
#' @export
cmp_frc_avl <- function(x) {cmp_vls(x, "frc")}

#' @describeIn cmp_mmm_avl Is \code{x} a complete indexer atomic vlist?
#' @export
cmp_ind_avl <- function(x) {cmp_vls(x, "ind")}

#' @describeIn cmp_mmm_avl Is \code{x} a complete logical atomic vlist?
#' @export
cmp_lgl_avl <- function(x) {cmp_vls(x, "lgl")}

#' @describeIn cmp_mmm_avl Is \code{x} a complete negative-number atomic vlist?
#' @export
cmp_neg_avl <- function(x) {cmp_vls(x, "neg")}

#' @describeIn cmp_mmm_avl Is \code{x} a complete negative-whole-number atomic
#'   vlist?
#' @export
cmp_ngw_avl <- function(x) {cmp_vls(x, "ngw")}

#' @describeIn cmp_mmm_avl Is \code{x} a complete non-negative-number atomic
#'   vlist?
#' @export
cmp_nng_avl <- function(x) {cmp_vls(x, "nng")}

#' @describeIn cmp_mmm_avl Is \code{x} a complete non-negative-whole-number
#'   atomic vlist?
#' @export
cmp_nnw_avl <- function(x) {cmp_vls(x, "nnw")}

#' @describeIn cmp_mmm_avl Is \code{x} a complete non-positive-numbered atomic
#'   vlist?
#' @export
cmp_nps_avl <- function(x) {cmp_vls(x, "nps")}

#' @describeIn cmp_mmm_avl Is \code{x} a complete non-positive-number atomic
#'   vlist?
#' @export
cmp_npw_avl <- function(x) {cmp_vls(x, "npw")}

#' @describeIn cmp_mmm_avl Is \code{x} a complete non-sortable atomic vlist?
#' @export
cmp_nst_avl <- function(x) {cmp_vls(x, "nst")}

#' @describeIn cmp_mmm_avl Is \code{x} a complete number atomic vlist?
#' @export
cmp_num_avl <- function(x) {cmp_vls(x, "num")}

#' @describeIn cmp_mmm_avl Is \code{x} a complete odd-number atomic vlist?
#' @export
cmp_odd_avl <- function(x) {cmp_vls(x, "odd")}

#' @describeIn cmp_mmm_avl Is \code{x} a complete ordered-factor atomic vlist?
#' @export
cmp_ord_avl <- function(x) {cmp_vls(x, "ord")}

#' @describeIn cmp_mmm_avl Is \code{x} a complete percent-number atomic vlist?
#' @export
cmp_pct_avl <- function(x) {cmp_vls(x, "pct")}

#' @describeIn cmp_mmm_avl Is \code{x} a complete positive-number atomic vlist?
#' @export
cmp_pos_avl <- function(x) {cmp_vls(x, "pos")}

#' @describeIn cmp_mmm_avl Is \code{x} a complete proportion-number atomic
#'   vlist?
#' @export
cmp_ppn_avl <- function(x) {cmp_vls(x, "ppn")}

#' @describeIn cmp_mmm_avl Is \code{x} a complete positive-whole-number atomic
#'   vlist?
#' @export
cmp_psw_avl <- function(x) {cmp_vls(x, "psw")}

#' @describeIn cmp_mmm_avl Is \code{x} a complete sortable atomic vlist?
#' @export
cmp_srt_avl <- function(x) {cmp_vls(x, "srt")}

#' @describeIn cmp_mmm_avl Is \code{x} a complete string atomic vlist?
#' @export
cmp_str_avl <- function(x) {cmp_vls(x, "str")}

#' @describeIn cmp_mmm_avl Is \code{x} a complete unordered-factor atomic vlist?
#' @export
cmp_uno_avl <- function(x) {cmp_vls(x, "uno")}

#' @describeIn cmp_mmm_avl Is \code{x} a complete whole-number atomic vlist?
#' @export
cmp_whl_avl <- function(x) {cmp_vls(x, "whl")}
