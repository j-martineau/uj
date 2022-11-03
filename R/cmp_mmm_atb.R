#' @name cmp_mmm_atb
#' @family props
#' @title Complete + Extended Mode + Atomic Tibble (cmp + mmm + atb)
#' @description Get all possible complete + extended mode + atomic tibble
#'   properties.
#' @details See \code{\link{mmm}}, and \code{\link{is_cmp_tbl}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}.
#' @export
cmp_mmm_atb_vals <- function() {
  x <- paste0("cmp_", c("atm", mmm_vals()), "_atb")
  names(x) <- rep.int("cmp_mmm_atb", length(x))
  x
}

#' @describeIn cmp_mmm_atb Is \code{x} a complete atomic tibble?
#' @export
cmp_atm_atb <- function(x) {cmp_tbl(x)}

#' @describeIn cmp_mmm_atb Is \code{x} a complete character atomic tibble?
#' @export
cmp_chr_atb <- function(x) {cmp_tbl(x, "chr")}

#' @describeIn cmp_mmm_atb Is \code{x} a complete onechar atomic tibble?
#' @export
cmp_ch1_atb <- function(x) {cmp_tbl(x, "ch1")}

#' @describeIn cmp_mmm_atb Is \code{x} a complete color atomic tibble?
#' @export
cmp_clr_atb <- function(x) {cmp_tbl(x, "clr")}

#' @describeIn cmp_mmm_atb Is \code{x} a complete even-numeric atomic tibble?
#' @export
cmp_evn_atb <- function(x) {cmp_tbl(x, "evn")}

#' @describeIn cmp_mmm_atb Is \code{x} a complete factor atomic tibble?
#' @export
cmp_fac_atb <- function(x) {cmp_tbl(x, "fac")}

#' @describeIn cmp_mmm_atb Is \code{x} a complete fractional-numeric atomic
#'   tibble?
#' @export
cmp_frc_atb <- function(x) {cmp_tbl(x, "frc")}

#' @describeIn cmp_mmm_atb Is \code{x} a complete indexer atomic tibble?
#' @export
cmp_ind_atb <- function(x) {cmp_tbl(x, "ind")}

#' @describeIn cmp_mmm_atb Is \code{x} a complete logical atomic tibble?
#' @export
cmp_lgl_atb <- function(x) {cmp_tbl(x, "lgl")}

#' @describeIn cmp_mmm_atb Is \code{x} a complete negative-numeric atomic
#'   tibble?
#' @export
cmp_neg_atb <- function(x) {cmp_tbl(x, "neg")}

#' @describeIn cmp_mmm_atb Is \code{x} a complete negative-whole-numeric atomic
#'   tibble?
#' @export
cmp_ngw_atb <- function(x) {cmp_tbl(x, "ngw")}

#' @describeIn cmp_mmm_atb Is \code{x} a complete non-negative-numeric atomic
#'   tibble?
#' @export
cmp_nng_atb <- function(x) {cmp_tbl(x, "nng")}

#' @describeIn cmp_mmm_atb Is \code{x} a complete non-negative-numeric atomic
#'   tibble?
#' @export
cmp_nnw_atb <- function(x) {cmp_tbl(x, "nnw")}

#' @describeIn cmp_mmm_atb Is \code{x} a complete non-positive-numeric atomic
#'   tibble?
#' @export
cmp_nps_atb <- function(x) {cmp_tbl(x, "nps")}

#' @describeIn cmp_mmm_atb Is \code{x} a complete non-positive-whole-numeric
#'   atomic tibble?
#' @export
cmp_npw_atb <- function(x) {cmp_tbl(x, "npw")}

#' @describeIn cmp_mmm_atb Is \code{x} a complete non-sortable atomic tibble?
#' @export
cmp_nst_atb <- function(x) {cmp_tbl(x, "nst")}

#' @describeIn cmp_mmm_atb Is \code{x} a complete numeric atomic tibble?
#' @export
cmp_num_atb <- function(x) {cmp_tbl(x, "num")}

#' @describeIn cmp_mmm_atb Is \code{x} a complete odd-numeric atomic tibble?
#' @export
cmp_odd_atb <- function(x) {cmp_tbl(x, "odd")}

#' @describeIn cmp_mmm_atb Is \code{x} a complete ordered-factor atomic tibble?
#' @export
cmp_ord_atb <- function(x) {cmp_tbl(x, "ord")}

#' @describeIn cmp_mmm_atb Is \code{x} a complete percent-numeric atomic tibble?
#' @export
cmp_pct_atb <- function(x) {cmp_tbl(x, "pct")}

#' @describeIn cmp_mmm_atb Is \code{x} a complete positive-numeric atomic
#'   tibble?
#' @export
cmp_pos_atb <- function(x) {cmp_tbl(x, "pos")}

#' @describeIn cmp_mmm_atb Is \code{x} a complete proportion-numeric atomic
#'   tibble?
#' @export
cmp_ppn_atb <- function(x) {cmp_tbl(x, "ppn")}

#' @describeIn cmp_mmm_atb Is \code{x} a complete positive-whole-numeric atomic
#'   tibble?
#' @export
cmp_psw_atb <- function(x) {cmp_tbl(x, "psw")}

#' @describeIn cmp_mmm_atb Is \code{x} a complete sortable atomic tibble?
#' @export
cmp_srt_atb <- function(x) {cmp_tbl(x, "srt")}

#' @describeIn cmp_mmm_atb Is \code{x} a complete string atomic tibble?
#' @export
cmp_str_atb <- function(x) {cmp_tbl(x, "str")}

#' @describeIn cmp_mmm_atb Is \code{x} a complete unordered-factor atomic
#'   tibble?
#' @export
cmp_uno_atb <- function(x) {cmp_tbl(x, "uno")}

#' @describeIn cmp_mmm_atb Is \code{x} a complete whole-numeric atomic tibble?
#' @export
cmp_whl_atb <- function(x) {cmp_tbl(x, "whl")}
