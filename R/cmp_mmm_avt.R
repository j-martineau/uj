#' @name cmp_mmm_avt
#' @family props
#' @title Complete + Extended Mode + Atomic Vtype (cmp + mmm + avt)
#' @details See \code{\link{mmm}}, and \code{\link{cmp_vtp}}.
#' @description Get all possible complete + extended mode + atomic vtype
#'   properties.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}.
#' @export
cmp_mmm_avt_vals <- function() {
  x <- paste0("cmp_", c("atm", mmm_vals()), "_avt")
  names(x) <- rep.int("cmp_mmm_avt", length(x))
  x
}

#' @describeIn cmp_mmm_avt Is \code{x} a complete atomic vtype?
#' @export
cmp_atm_avt <- function(x) {cmp_vtp(x) & iatm(x)}

#' @describeIn cmp_mmm_avt Is \code{x} a complete character atomic vtype?
#' @export
cmp_chr_avt <- function(x) {cmp_vtp(x, "chr")}

#' @describeIn cmp_mmm_avt Is \code{x} a complete onechar atomic vtype?
#' @export
cmp_ch1_avt <- function(x) {cmp_vtp(x, "ch1")}

#' @describeIn cmp_mmm_avt Is \code{x} a complete color atomic vtype?
#' @export
cmp_clr_avt <- function(x) {cmp_vtp(x, "clr")}

#' @describeIn cmp_mmm_avt Is \code{x} a complete even-number atomic vtype?
#' @export
cmp_evn_avt <- function(x) {cmp_vtp(x, "evn")}

#' @describeIn cmp_mmm_avt Is \code{x} a complete factor atomic vtype?
#' @export
cmp_fac_avt <- function(x) {cmp_vtp(x, "fac")}

#' @describeIn cmp_mmm_avt Is \code{x} a complete fractional-number atomic
#'   vtype?
#' @export
cmp_frc_avt <- function(x) {cmp_vtp(x, "frc")}

#' @describeIn cmp_mmm_avt Is \code{x} a complete indexer atomic vtype?
#' @export
cmp_ind_avt <- function(x) {cmp_vtp(x, "ind")}

#' @describeIn cmp_mmm_avt Is \code{x} a complete logical atomic vtype?
#' @export
cmp_lgl_avt <- function(x) {cmp_vtp(x, "lgl")}

#' @describeIn cmp_mmm_avt Is \code{x} a complete negative-number atomic vtype?
#' @export
cmp_neg_avt <- function(x) {cmp_vtp(x, "neg")}

#' @describeIn cmp_mmm_avt Is \code{x} a complete negative-whole-number atomic
#'   vtype?
#' @export
cmp_ngw_avt <- function(x) {cmp_vtp(x, "ngw")}

#' @describeIn cmp_mmm_avt Is \code{x} a complete non-negative-number atomic
#'   vtype?
#' @export
cmp_nng_avt <- function(x) {cmp_vtp(x, "nng")}

#' @describeIn cmp_mmm_avt Is \code{x} a complete non-negative-whole-number
#'   atomic vtype?
#' @export
cmp_nnw_avt <- function(x) {cmp_vtp(x, "nnw")}

#' @describeIn cmp_mmm_avt Is \code{x} a complete non-positive-number atomic
#'   vtype?
#' @export
cmp_nps_avt <- function(x) {cmp_vtp(x, "nps")}

#' @describeIn cmp_mmm_avt Is \code{x} a complete non-positive-whole-number
#'   atomic vtype?
#' @export
cmp_npw_avt <- function(x) {cmp_vtp(x, "npw")}

#' @describeIn cmp_mmm_avt Is \code{x} a complete non-sortable atomic vtype?
#' @export
cmp_nst_avt <- function(x) {cmp_vtp(x, "nst")}

#' @describeIn cmp_mmm_avt Is \code{x} a complete number atomic vtype?
#' @export
cmp_num_avt <- function(x) {cmp_vtp(x, "num")}

#' @describeIn cmp_mmm_avt Is \code{x} a complete odd-number atomic vtype?
#' @export
cmp_odd_avt <- function(x) {cmp_vtp(x, "odd")}

#' @describeIn cmp_mmm_avt Is \code{x} a complete ordered-factor atomic vtype?
#' @export
cmp_ord_avt <- function(x) {cmp_vtp(x, "ord")}

#' @describeIn cmp_mmm_avt Is \code{x} a complete percentage-number atomic
#'   vtype?
#' @export
cmp_pct_avt <- function(x) {cmp_vtp(x, "pct")}

#' @describeIn cmp_mmm_avt Is \code{x} a complete positive-number atomic vtype?
#' @export
cmp_pos_avt <- function(x) {cmp_vtp(x, "pos")}

#' @describeIn cmp_mmm_avt Is \code{x} a complete proportional-number atomic
#'   vtype?
#' @export
cmp_ppn_avt <- function(x) {cmp_vtp(x, "ppn")}

#' @describeIn cmp_mmm_avt Is \code{x} a complete positive-whole-number atomic
#'   vtype?
#' @export
cmp_psw_avt <- function(x) {cmp_vtp(x, "psw")}

#' @describeIn cmp_mmm_avt Is \code{x} a complete sortable atomic vtype?
#' @export
cmp_srt_avt <- function(x) {cmp_vtp(x, "srt")}

#' @describeIn cmp_mmm_avt Is \code{x} a complete string atomic vtype?
#' @export
cmp_str_avt <- function(x) {cmp_vtp(x, "str")}

#' @describeIn cmp_mmm_avt Is \code{x} a complete unordered-factor atomic vtype?
#' @export
cmp_uno_avt <- function(x) {cmp_vtp(x, "uno")}

#' @describeIn cmp_mmm_avt Is \code{x} a complete whole-numbered atomic vtype?
#' @export
cmp_whl_avt <- function(x) {cmp_vtp(x, "whl")}
