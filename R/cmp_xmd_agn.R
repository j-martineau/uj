#' @name cmp_xmd_agn
#' @family props
#' @title complete + xmd + atomic generic
#' @description See \code{\link{xmd}}, and \code{\link{is_cmp_generic}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}.
#' @export
cmp_xmd_agn_vals <- function() {
  x <- paste0("cmp_", c("atm", xmd_vals()), "_agn")
  names(x) <- rep.int("cmp_xmd_agn", length(x))
  x
}

#' @rdname cmp_xmd_agn
#' @export
cmp_atm_agn <- function(x) {is_cmp_generic(x)}

#' @rdname cmp_xmd_agn
#' @export
cmp_chr_agn <- function(x) {is_cmp_generic(x, "chr")}

#' @rdname cmp_xmd_agn
#' @export
cmp_ch1_agn <- function(x) {is_cmp_generic(x, "ch1")}

#' @rdname cmp_xmd_agn
#' @export
cmp_clr_agn <- function(x) {is_cmp_generic(x, "clr")}

#' @rdname cmp_xmd_agn
#' @export
cmp_evn_agn <- function(x) {is_cmp_generic(x, "evn")}

#' @rdname cmp_xmd_agn
#' @export
cmp_fac_agn <- function(x) {is_cmp_generic(x, "fac")}

#' @rdname cmp_xmd_agn
#' @export
cmp_frc_agn <- function(x) {is_cmp_generic(x, "frc")}

#' @rdname cmp_xmd_agn
#' @export
cmp_ind_agn <- function(x) {is_cmp_generic(x, "ind")}

#' @rdname cmp_xmd_agn
#' @export
cmp_lgc_agn <- function(x) {is_cmp_generic(x, "lgc")}

#' @rdname cmp_xmd_agn
#' @export
cmp_cnb_agn <- function(x) {is_cmp_generic(x, "cnb")}

#' @rdname cmp_xmd_agn
#' @export
cmp_neg_agn <- function(x) {is_cmp_generic(x, "neg")}

#' @rdname cmp_xmd_agn
#' @export
cmp_ngw_agn <- function(x) {is_cmp_generic(x, "ngw")}

#' @rdname cmp_xmd_agn
#' @export
cmp_nng_agn <- function(x) {is_cmp_generic(x, "nng")}

#' @rdname cmp_xmd_agn
#' @export
cmp_nnw_agn <- function(x) {is_cmp_generic(x, "nnw")}

#' @rdname cmp_xmd_agn
#' @export
cmp_nps_agn <- function(x) {is_cmp_generic(x, "nps")}

#' @rdname cmp_xmd_agn
#' @export
cmp_npw_agn <- function(x) {is_cmp_generic(x, "npw")}

#' @rdname cmp_xmd_agn
#' @export
cmp_nst_agn <- function(x) {is_cmp_generic(x, "nst")}

#' @rdname cmp_xmd_agn
#' @export
cmp_num_agn <- function(x) {is_cmp_generic(x, "num")}

#' @rdname cmp_xmd_agn
#' @export
cmp_odd_agn <- function(x) {is_cmp_generic(x, "odd")}

#' @rdname cmp_xmd_agn
#' @export
cmp_ord_agn <- function(x) {is_cmp_generic(x, "ord")}

#' @rdname cmp_xmd_agn
#' @export
cmp_pct_agn <- function(x) {is_cmp_generic(x, "pct")}

#' @rdname cmp_xmd_agn
#' @export
cmp_pos_agn <- function(x) {is_cmp_generic(x, "pos")}

#' @rdname cmp_xmd_agn
#' @export
cmp_ppn_agn <- function(x) {is_cmp_generic(x, "ppn")}

#' @rdname cmp_xmd_agn
#' @export
cmp_psw_agn <- function(x) {is_cmp_generic(x, "psw")}

#' @rdname cmp_xmd_agn
#' @export
cmp_srt_agn <- function(x) {is_cmp_generic(x, "srt")}

#' @rdname cmp_xmd_agn
#' @export
cmp_str_agn <- function(x) {is_cmp_generic(x, "str")}

#' @rdname cmp_xmd_agn
#' @export
cmp_uno_agn <- function(x) {is_cmp_generic(x, "uno")}

#' @rdname cmp_xmd_agn
#' @export
cmp_whl_agn <- function(x) {is_cmp_generic(x, "whl")}
