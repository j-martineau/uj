#' @name cmp_mmm_arr
#' @family props
#' @title Complete + Extended Mode + Atomic Array (cmp + mmm + arr)
#' @description Get all possible complete + extended mode + array properties.
#' @details See \code{\link{mmm}}, and \code{\link{cmp_arr}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}
#' @export
cmp_mmm_arr_vals <- function() {
  x <- paste0("cmp_", c("atm", mmm_vals()), "_arr")
  names(x) <- rep.int("cmp_mmm_arr", length(x))
  x
}

#' @describeIn cmp_mmm_arr Is \code{x} a complete atomic array?
#' @export
cmp_atm_arr <- function(x) {cmp_arr(x) & icmp(x)}

#' @describeIn cmp_mmm_arr Is \code{x} a complete character atomic array?
#' @export
cmp_chr_arr <- function(x) {cmp_arr(x, "chr")}

#' @describeIn cmp_mmm_arr Is \code{x} a complete onechar atomic array?
#' @export
cmp_ch1_arr <- function(x) {cmp_arr(x, "ch1")}

#' @describeIn cmp_mmm_arr Is \code{x} a complete color atomic array?
#' @export
cmp_clr_arr <- function(x) {cmp_arr(x, "clr")}

#' @describeIn cmp_mmm_arr Is \code{x} a complete even-numeric atomic array?
#' @export
cmp_evn_arr <- function(x) {cmp_arr(x, "evn")}

#' @describeIn cmp_mmm_arr Is \code{x} a complete factor atomic array?
#' @export
cmp_fac_arr <- function(x) {cmp_arr(x, "fac")}

#' @describeIn cmp_mmm_arr Is \code{x} a complete fractional-numeric atomic
#'   array?
#' @export
cmp_frc_arr <- function(x) {cmp_arr(x, "frc")}

#' @describeIn cmp_mmm_arr Is \code{x} a complete indexer atomic array?
#' @export
cmp_ind_arr <- function(x) {cmp_arr(x, "ind")}

#' @describeIn cmp_mmm_arr Is \code{x} a complete logical atomic array?
#' @export
cmp_lgl_arr <- function(x) {cmp_arr(x, "lgl")}

#' @describeIn cmp_mmm_arr Is \code{x} a complete negative-numeric atomic array?
#' @export
cmp_neg_arr <- function(x) {cmp_arr(x, "neg")}

#' @describeIn cmp_mmm_arr Is \code{x} a complete negative-whole-numeric atomic
#'   array?
#' @export
cmp_ngw_arr <- function(x) {cmp_arr(x, "ngw")}

#' @describeIn cmp_mmm_arr Is \code{x} a complete non-negative-numeric atomic
#'   array?
#' @export
cmp_nng_arr <- function(x) {cmp_arr(x, "nng")}

#' @describeIn cmp_mmm_arr Is \code{x} a complete non-negative-whole-numeric
#'   atomic array?
#' @export
cmp_nnw_arr <- function(x) {cmp_arr(x, "nnw")}

#' @describeIn cmp_mmm_arr Is \code{x} a complete non-positive-numeric atomic
#'   array?
#' @export
cmp_nps_arr <- function(x) {cmp_arr(x, "nps")}

#' @describeIn cmp_mmm_arr Is \code{x} a complete non-positive-whole-numeric
#'   atomic array?
#' @export
cmp_npw_arr <- function(x) {cmp_arr(x, "npw")}

#' @describeIn cmp_mmm_arr Is \code{x} a complete non-sortable atomic array?
#' @export
cmp_nst_arr <- function(x) {cmp_arr(x, "nst")}

#' @describeIn cmp_mmm_arr Is \code{x} a complete numeric atomic array?
#' @export
cmp_num_arr <- function(x) {cmp_arr(x, "num")}

#' @describeIn cmp_mmm_arr Is \code{x} a complete odd-numeric atomic array?
#' @export
cmp_odd_arr <- function(x) {cmp_arr(x, "odd")}

#' @describeIn cmp_mmm_arr Is \code{x} a complete ordered-factor atomic array?
#' @export
cmp_ord_arr <- function(x) {cmp_arr(x, "ord")}

#' @describeIn cmp_mmm_arr Is \code{x} a complete percent-numeric atomic array?
#' @export
cmp_pct_arr <- function(x) {cmp_arr(x, "pct")}

#' @describeIn cmp_mmm_arr Is \code{x} a complete positive-numeric atomic array?
#' @export
cmp_pos_arr <- function(x) {cmp_arr(x, "pos")}

#' @describeIn cmp_mmm_arr Is \code{x} a complete proportion-numeric atomic
#'   array?
#' @export
cmp_ppn_arr <- function(x) {cmp_arr(x, "ppn")}

#' @describeIn cmp_mmm_arr Is \code{x} a complete positive-whole-numeric atomic
#'   array?
#' @export
cmp_psw_arr <- function(x) {cmp_arr(x, "psw")}

#' @describeIn cmp_mmm_arr Is \code{x} a complete sortable atomic array?
#' @export
cmp_srt_arr <- function(x) {cmp_arr(x, "srt")}

#' @describeIn cmp_mmm_arr Is \code{x} a complete string atomic array?
#' @export
cmp_str_arr <- function(x) {cmp_arr(x, "str")}

#' @describeIn cmp_mmm_arr Is \code{x} a complete unordered-factor atomic array?
#' @export
cmp_uno_arr <- function(x) {cmp_arr(x, "uno")}

#' @describeIn cmp_mmm_arr Is \code{x} a complete whole-numeric atomic array?
#' @export
cmp_whl_arr <- function(x) {cmp_arr(x, "whl")}
