#' @name cmp_mmm_avl
#' @family props
#' @title Complete + Extended Mode + Atomic Vlist (cmp + mmm + avl)
#' @description See \code{\link{mmm}}, and \code{\link{is_cmp_vlist}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}.
#' @export
cmp_mmm_avl_vals <- function() {
  x <- paste0("cmp_", c("atm", mmm_vals()), "_avl")
  names(x) <- rep.int("cmp_mmm_avl", length(x))
  x
}

#' @rdname cmp_mmm_avl
#' @export
cmp_atm_avl <- function(x) {is_cmp_vlist(x)}

#' @rdname cmp_mmm_avl
#' @export
cmp_chr_avl <- function(x) {is_cmp_vlist(x, "chr")}

#' @rdname cmp_mmm_avl
#' @export
cmp_ch1_avl <- function(x) {is_cmp_vlist(x, "ch1")}

#' @rdname cmp_mmm_avl
#' @export
cmp_clr_avl <- function(x) {is_cmp_vlist(x, "clr")}

#' @rdname cmp_mmm_avl
#' @export
cmp_evn_avl <- function(x) {is_cmp_vlist(x, "evn")}

#' @rdname cmp_mmm_avl
#' @export
cmp_fac_avl <- function(x) {is_cmp_vlist(x, "fac")}

#' @rdname cmp_mmm_avl
#' @export
cmp_frc_avl <- function(x) {is_cmp_vlist(x, "frc")}

#' @rdname cmp_mmm_avl
#' @export
cmp_ind_avl <- function(x) {is_cmp_vlist(x, "ind")}

#' @rdname cmp_mmm_avl
#' @export
cmp_lgc_avl <- function(x) {is_cmp_vlist(x, "lgc")}

#' @rdname cmp_mmm_avl
#' @export
cmp_cnb_avl <- function(x) {is_cmp_vlist(x, "cnb")}

#' @rdname cmp_mmm_avl
#' @export
cmp_neg_avl <- function(x) {is_cmp_vlist(x, "neg")}

#' @rdname cmp_mmm_avl
#' @export
cmp_ngw_avl <- function(x) {is_cmp_vlist(x, "ngw")}

#' @rdname cmp_mmm_avl
#' @export
cmp_nng_avl <- function(x) {is_cmp_vlist(x, "nng")}

#' @rdname cmp_mmm_avl
#' @export
cmp_nnw_avl <- function(x) {is_cmp_vlist(x, "nnw")}

#' @rdname cmp_mmm_avl
#' @export
cmp_nps_avl <- function(x) {is_cmp_vlist(x, "nps")}

#' @rdname cmp_mmm_avl
#' @export
cmp_npw_avl <- function(x) {is_cmp_vlist(x, "npw")}

#' @rdname cmp_mmm_avl
#' @export
cmp_nst_avl <- function(x) {is_cmp_vlist(x, "nst")}

#' @rdname cmp_mmm_avl
#' @export
cmp_num_avl <- function(x) {is_cmp_vlist(x, "num")}

#' @rdname cmp_mmm_avl
#' @export
cmp_odd_avl <- function(x) {is_cmp_vlist(x, "odd")}

#' @rdname cmp_mmm_avl
#' @export
cmp_ord_avl <- function(x) {is_cmp_vlist(x, "ord")}

#' @rdname cmp_mmm_avl
#' @export
cmp_pct_avl <- function(x) {is_cmp_vlist(x, "pct")}

#' @rdname cmp_mmm_avl
#' @export
cmp_pos_avl <- function(x) {is_cmp_vlist(x, "pos")}

#' @rdname cmp_mmm_avl
#' @export
cmp_ppn_avl <- function(x) {is_cmp_vlist(x, "ppn")}

#' @rdname cmp_mmm_avl
#' @export
cmp_psw_avl <- function(x) {is_cmp_vlist(x, "psw")}

#' @rdname cmp_mmm_avl
#' @export
cmp_srt_avl <- function(x) {is_cmp_vlist(x, "srt")}

#' @rdname cmp_mmm_avl
#' @export
cmp_str_avl <- function(x) {is_cmp_vlist(x, "str")}

#' @rdname cmp_mmm_avl
#' @export
cmp_uno_avl <- function(x) {is_cmp_vlist(x, "uno")}

#' @rdname cmp_mmm_avl
#' @export
cmp_whl_avl <- function(x) {is_cmp_vlist(x, "whl")}
