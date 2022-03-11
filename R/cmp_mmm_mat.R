#' @name cmp_mmm_mat
#' @family props
#' @title Complete + Extended Mode + Atomic Matrix (comp + mmm + mat)
#' @description See \code{\link{mmm}}, and \code{\link{is_cmp_matrix}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}.
#' @export
cmp_mmm_mat_vals <- function() {
  x <- paste0("cmp_", c("atm", mmm_vals()), "_mat")
  names(x) <- rep.int("cmp_mmm_mat", length(x))
  x
}

#' @rdname cmp_mmm_mat
#' @export
cmp_atm_mat <- function(x) {is_cmp_matrix(x)}

#' @rdname cmp_mmm_mat
#' @export
cmp_chr_mat <- function(x) {is_cmp_matrix(x, "chr")}

#' @rdname cmp_mmm_mat
#' @export
cmp_ch1_mat <- function(x) {is_cmp_matrix(x, "ch1")}

#' @rdname cmp_mmm_mat
#' @export
cmp_clr_mat <- function(x) {is_cmp_matrix(x, "clr")}

#' @rdname cmp_mmm_mat
#' @export
cmp_evn_mat <- function(x) {is_cmp_matrix(x, "evn")}

#' @rdname cmp_mmm_mat
#' @export
cmp_fac_mat <- function(x) {is_cmp_matrix(x, "fac")}

#' @rdname cmp_mmm_mat
#' @export
cmp_frc_mat <- function(x) {is_cmp_matrix(x, "frc")}

#' @rdname cmp_mmm_mat
#' @export
cmp_ind_mat <- function(x) {is_cmp_matrix(x, "ind")}

#' @rdname cmp_mmm_mat
#' @export
cmp_lgc_mat <- function(x) {is_cmp_matrix(x, "lgc")}

#' @rdname cmp_mmm_mat
#' @export
cmp_cnb_mat <- function(x) {is_cmp_matrix(x, "cnb")}

#' @rdname cmp_mmm_mat
#' @export
cmp_neg_mat <- function(x) {is_cmp_matrix(x, "neg")}

#' @rdname cmp_mmm_mat
#' @export
cmp_ngw_mat <- function(x) {is_cmp_matrix(x, "ngw")}

#' @rdname cmp_mmm_mat
#' @export
cmp_nng_mat <- function(x) {is_cmp_matrix(x, "nng")}

#' @rdname cmp_mmm_mat
#' @export
cmp_nnw_mat <- function(x) {is_cmp_matrix(x, "nnw")}

#' @rdname cmp_mmm_mat
#' @export
cmp_nps_mat <- function(x) {is_cmp_matrix(x, "nps")}

#' @rdname cmp_mmm_mat
#' @export
cmp_npw_mat <- function(x) {is_cmp_matrix(x, "npw")}

#' @rdname cmp_mmm_mat
#' @export
cmp_nst_mat <- function(x) {is_cmp_matrix(x, "nst")}

#' @rdname cmp_mmm_mat
#' @export
cmp_num_mat <- function(x) {is_cmp_matrix(x, "num")}

#' @rdname cmp_mmm_mat
#' @export
cmp_odd_mat <- function(x) {is_cmp_matrix(x, "odd")}

#' @rdname cmp_mmm_mat
#' @export
cmp_ord_mat <- function(x) {is_cmp_matrix(x, "ord")}

#' @rdname cmp_mmm_mat
#' @export
cmp_pct_mat <- function(x) {is_cmp_matrix(x, "pct")}

#' @rdname cmp_mmm_mat
#' @export
cmp_pos_mat <- function(x) {is_cmp_matrix(x, "pos")}

#' @rdname cmp_mmm_mat
#' @export
cmp_ppn_mat <- function(x) {is_cmp_matrix(x, "ppn")}

#' @rdname cmp_mmm_mat
#' @export
cmp_psw_mat <- function(x) {is_cmp_matrix(x, "psw")}

#' @rdname cmp_mmm_mat
#' @export
cmp_srt_mat <- function(x) {is_cmp_matrix(x, "srt")}

#' @rdname cmp_mmm_mat
#' @export
cmp_str_mat <- function(x) {is_cmp_matrix(x, "str")}

#' @rdname cmp_mmm_mat
#' @export
cmp_uno_mat <- function(x) {is_cmp_matrix(x, "uno")}

#' @rdname cmp_mmm_mat
#' @export
cmp_whl_mat <- function(x) {is_cmp_matrix(x, "whl")}
