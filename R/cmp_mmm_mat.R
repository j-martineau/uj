#' @name cmp_mmm_mat
#' @family props
#' @title Complete + Extended Mode + Atomic Matrix (comp + mmm + mat)
#' @description Get all possible complete + extended mode + atomic matrix
#'   properties.
#' @details See \code{\link{mmm}}, and \code{\link{cmp_mat}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}.
#' @export
cmp_mmm_mat_vals <- function() {
  x <- paste0("cmp_", c("atm", mmm_vals()), "_mat")
  names(x) <- rep.int("cmp_mmm_mat", length(x))
  x
}

#' @describeIn cmp_mmm_mat Is \code{x} a complete atomic matrix?
#' @export
cmp_atm_mat <- function(x) {cmp_mat(x) & iatm(x)}

#' @describeIn cmp_mmm_mat Is \code{x} a complete character atomic matrix?
#' @export
cmp_chr_mat <- function(x) {cmp_mat(x, "chr")}

#' @describeIn cmp_mmm_mat Is \code{x} a complete onechar atomic matrix?
#' @export
cmp_ch1_mat <- function(x) {cmp_mat(x, "ch1")}

#' @describeIn cmp_mmm_mat Is \code{x} a complete color atomic matrix?
#' @export
cmp_clr_mat <- function(x) {cmp_mat(x, "clr")}

#' @describeIn cmp_mmm_mat Is \code{x} a complete even-number atomic matrix?
#' @export
cmp_evn_mat <- function(x) {cmp_mat(x, "evn")}

#' @describeIn cmp_mmm_mat Is \code{x} a complete factor atomic matrix?
#' @export
cmp_fac_mat <- function(x) {cmp_mat(x, "fac")}

#' @describeIn cmp_mmm_mat Is \code{x} a complete fractional-number atomic
#'   matrix?
#' @export
cmp_frc_mat <- function(x) {cmp_mat(x, "frc")}

#' @describeIn cmp_mmm_mat Is \code{x} a complete indexer atomic matrix?
#' @export
cmp_ind_mat <- function(x) {cmp_mat(x, "ind")}

#' @describeIn cmp_mmm_mat Is \code{x} a complete logical atomic matrix?
#' @export
cmp_lgl_mat <- function(x) {cmp_mat(x, "lgl")}

#' @describeIn cmp_mmm_mat Is \code{x} a complete negative-number atomic matrix?
#' @export
cmp_neg_mat <- function(x) {cmp_mat(x, "neg")}

#' @describeIn cmp_mmm_mat Is \code{x} a complete negative-whole-number atomic
#'   matrix?
#' @export
cmp_ngw_mat <- function(x) {cmp_mat(x, "ngw")}

#' @describeIn cmp_mmm_mat Is \code{x} a complete non-negative-number atomic
#'   matrix?
#' @export
cmp_nng_mat <- function(x) {cmp_mat(x, "nng")}

#' @describeIn cmp_mmm_mat Is \code{x} a complete non-negative-whole-number
#'   atomic matrix?
#' @export
cmp_nnw_mat <- function(x) {cmp_mat(x, "nnw")}

#' @describeIn cmp_mmm_mat Is \code{x} a complete non-positive-number atomic
#'   matrix?
#' @export
cmp_nps_mat <- function(x) {cmp_mat(x, "nps")}

#' @describeIn cmp_mmm_mat Is \code{x} a complete non-positive-whole-number
#'   atomic matrix?
#' @export
cmp_npw_mat <- function(x) {cmp_mat(x, "npw")}

#' @describeIn cmp_mmm_mat Is \code{x} a complete non-sortable atomic matrix?
#' @export
cmp_nst_mat <- function(x) {cmp_mat(x, "nst")}

#' @describeIn cmp_mmm_mat Is \code{x} a complete number atomic matrix?
#' @export
cmp_num_mat <- function(x) {cmp_mat(x, "num")}

#' @describeIn cmp_mmm_mat Is \code{x} a complete odd-number atomic matrix?
#' @export
cmp_odd_mat <- function(x) {cmp_mat(x, "odd")}

#' @describeIn cmp_mmm_mat Is \code{x} a complete ordered-factor atomic matrix?
#' @export
cmp_ord_mat <- function(x) {cmp_mat(x, "ord")}

#' @describeIn cmp_mmm_mat Is \code{x} a complete percent-number atomic matrix?
#' @export
cmp_pct_mat <- function(x) {cmp_mat(x, "pct")}

#' @describeIn cmp_mmm_mat Is \code{x} a complete positive-number atomic matrix?
#' @export
cmp_pos_mat <- function(x) {cmp_mat(x, "pos")}

#' @describeIn cmp_mmm_mat Is \code{x} a complete proportion-number atomic
#'   matrix?
#' @export
cmp_ppn_mat <- function(x) {cmp_mat(x, "ppn")}

#' @describeIn cmp_mmm_mat Is \code{x} a complete positive-whole-number atomic
#'   matrix?
#' @export
cmp_psw_mat <- function(x) {cmp_mat(x, "psw")}

#' @describeIn cmp_mmm_mat Is \code{x} a complete sortable atomic matrix?
#' @export
cmp_srt_mat <- function(x) {cmp_mat(x, "srt")}

#' @describeIn cmp_mmm_mat Is \code{x} a complete string atomic matrix?
#' @export
cmp_str_mat <- function(x) {cmp_mat(x, "str")}

#' @describeIn cmp_mmm_mat Is \code{x} a complete unordered-factor atomic
#'   matrix?
#' @export
cmp_uno_mat <- function(x) {cmp_mat(x, "uno")}

#' @describeIn cmp_mmm_mat Is \code{x} a complete whole-number atomic matrix?
#' @export
cmp_whl_mat <- function(x) {cmp_mat(x, "whl")}
