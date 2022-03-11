#' @name cmp_mmm_avt
#' @family props
#' @title Complete + Extended Mode + Atomic Vtype (cmp + mmm + avt)
#' @description See \code{\link{mmm}}, and \code{\link{is_cmp_vtype}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}.
#' @export
cmp_mmm_avt_vals <- function() {
  x <- paste0("cmp_", c("atm", mmm_vals()), "_avt")
  names(x) <- rep.int("cmp_mmm_avt", length(x))
  x
}

#' @rdname cmp_mmm_avt
#' @export
cmp_atm_avt <- function(x) {is_cmp_vtype(x)}

#' @rdname cmp_mmm_avt
#' @export
cmp_chr_avt <- function(x) {is_cmp_vtype(x, "chr")}

#' @rdname cmp_mmm_avt
#' @export
cmp_ch1_avt <- function(x) {is_cmp_vtype(x, "ch1")}

#' @rdname cmp_mmm_avt
#' @export
cmp_clr_avt <- function(x) {is_cmp_vtype(x, "clr")}

#' @rdname cmp_mmm_avt
#' @export
cmp_evn_avt <- function(x) {is_cmp_vtype(x, "evn")}

#' @rdname cmp_mmm_avt
#' @export
cmp_fac_avt <- function(x) {is_cmp_vtype(x, "fac")}

#' @rdname cmp_mmm_avt
#' @export
cmp_frc_avt <- function(x) {is_cmp_vtype(x, "frc")}

#' @rdname cmp_mmm_avt
#' @export
cmp_ind_avt <- function(x) {is_cmp_vtype(x, "ind")}

#' @rdname cmp_mmm_avt
#' @export
cmp_lgc_avt <- function(x) {is_cmp_vtype(x, "lgc")}

#' @rdname cmp_mmm_avt
#' @export
cmp_cnb_avt <- function(x) {is_cmp_vtype(x, "cnb")}

#' @rdname cmp_mmm_avt
#' @export
cmp_neg_avt <- function(x) {is_cmp_vtype(x, "neg")}

#' @rdname cmp_mmm_avt
#' @export
cmp_ngw_avt <- function(x) {is_cmp_vtype(x, "ngw")}

#' @rdname cmp_mmm_avt
#' @export
cmp_nng_avt <- function(x) {is_cmp_vtype(x, "nng")}

#' @rdname cmp_mmm_avt
#' @export
cmp_nnw_avt <- function(x) {is_cmp_vtype(x, "nnw")}

#' @rdname cmp_mmm_avt
#' @export
cmp_nps_avt <- function(x) {is_cmp_vtype(x, "nps")}

#' @rdname cmp_mmm_avt
#' @export
cmp_npw_avt <- function(x) {is_cmp_vtype(x, "npw")}

#' @rdname cmp_mmm_avt
#' @export
cmp_nst_avt <- function(x) {is_cmp_vtype(x, "nst")}

#' @rdname cmp_mmm_avt
#' @export
cmp_num_avt <- function(x) {is_cmp_vtype(x, "num")}

#' @rdname cmp_mmm_avt
#' @export
cmp_odd_avt <- function(x) {is_cmp_vtype(x, "odd")}

#' @rdname cmp_mmm_avt
#' @export
cmp_ord_avt <- function(x) {is_cmp_vtype(x, "ord")}

#' @rdname cmp_mmm_avt
#' @export
cmp_pct_avt <- function(x) {is_cmp_vtype(x, "pct")}

#' @rdname cmp_mmm_avt
#' @export
cmp_pos_avt <- function(x) {is_cmp_vtype(x, "pos")}

#' @rdname cmp_mmm_avt
#' @export
cmp_ppn_avt <- function(x) {is_cmp_vtype(x, "ppn")}

#' @rdname cmp_mmm_avt
#' @export
cmp_psw_avt <- function(x) {is_cmp_vtype(x, "psw")}

#' @rdname cmp_mmm_avt
#' @export
cmp_srt_avt <- function(x) {is_cmp_vtype(x, "srt")}

#' @rdname cmp_mmm_avt
#' @export
cmp_str_avt <- function(x) {is_cmp_vtype(x, "str")}

#' @rdname cmp_mmm_avt
#' @export
cmp_uno_avt <- function(x) {is_cmp_vtype(x, "uno")}

#' @rdname cmp_mmm_avt
#' @export
cmp_whl_avt <- function(x) {is_cmp_vtype(x, "whl")}
