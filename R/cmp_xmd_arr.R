#' @name cmp_xmd_arr
#' @family props
#' @title Complete + xmd + Atomic Array
#' @description See \code{\link{xmd}}, and \code{\link{is_cmp_array}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}
#' @export
cmp_xmd_arr_vals <- function() {
  x <- paste0("cmp_", c("atm", xmd_vals()), "_arr")
  names(x) <- rep.int("cmp_xmd_arr", length(x))
  x
}

#' @rdname cmp_xmd_arr
#' @export
cmp_atm_arr <- function(x) {is_cmp_array(x)}

#' @rdname cmp_xmd_arr
#' @export
cmp_chr_arr <- function(x) {is_cmp_array(x, "chr")}

#' @rdname cmp_xmd_arr
#' @export
cmp_ch1_arr <- function(x) {is_cmp_array(x, "ch1")}

#' @rdname cmp_xmd_arr
#' @export
cmp_clr_arr <- function(x) {is_cmp_array(x, "clr")}

#' @rdname cmp_xmd_arr
#' @export
cmp_evn_arr <- function(x) {is_cmp_array(x, "evn")}

#' @rdname cmp_xmd_arr
#' @export
cmp_fac_arr <- function(x) {is_cmp_array(x, "fac")}

#' @rdname cmp_xmd_arr
#' @export
cmp_frc_arr <- function(x) {is_cmp_array(x, "frc")}

#' @rdname cmp_xmd_arr
#' @export
cmp_ind_arr <- function(x) {is_cmp_array(x, "ind")}

#' @rdname cmp_xmd_arr
#' @export
cmp_lgc_arr <- function(x) {is_cmp_array(x, "lgc")}

#' @rdname cmp_xmd_arr
#' @export
cmp_cnb_arr <- function(x) {is_cmp_array(x, "cnb")}

#' @rdname cmp_xmd_arr
#' @export
cmp_neg_arr <- function(x) {is_cmp_array(x, "neg")}

#' @rdname cmp_xmd_arr
#' @export
cmp_ngw_arr <- function(x) {is_cmp_array(x, "ngw")}

#' @rdname cmp_xmd_arr
#' @export
cmp_nng_arr <- function(x) {is_cmp_array(x, "nng")}

#' @rdname cmp_xmd_arr
#' @export
cmp_nnw_arr <- function(x) {is_cmp_array(x, "nnw")}

#' @rdname cmp_xmd_arr
#' @export
cmp_nps_arr <- function(x) {is_cmp_array(x, "nps")}

#' @rdname cmp_xmd_arr
#' @export
cmp_npw_arr <- function(x) {is_cmp_array(x, "npw")}

#' @rdname cmp_xmd_arr
#' @export
cmp_nst_arr <- function(x) {is_cmp_array(x, "nst")}

#' @rdname cmp_xmd_arr
#' @export
cmp_num_arr <- function(x) {is_cmp_array(x, "num")}

#' @rdname cmp_xmd_arr
#' @export
cmp_odd_arr <- function(x) {is_cmp_array(x, "odd")}

#' @rdname cmp_xmd_arr
#' @export
cmp_ord_arr <- function(x) {is_cmp_array(x, "ord")}

#' @rdname cmp_xmd_arr
#' @export
cmp_pct_arr <- function(x) {is_cmp_array(x, "pct")}

#' @rdname cmp_xmd_arr
#' @export
cmp_pos_arr <- function(x) {is_cmp_array(x, "pos")}

#' @rdname cmp_xmd_arr
#' @export
cmp_ppn_arr <- function(x) {is_cmp_array(x, "ppn")}

#' @rdname cmp_xmd_arr
#' @export
cmp_psw_arr <- function(x) {is_cmp_array(x, "psw")}

#' @rdname cmp_xmd_arr
#' @export
cmp_srt_arr <- function(x) {is_cmp_array(x, "srt")}

#' @rdname cmp_xmd_arr
#' @export
cmp_str_arr <- function(x) {is_cmp_array(x, "str")}

#' @rdname cmp_xmd_arr
#' @export
cmp_uno_arr <- function(x) {is_cmp_array(x, "uno")}

#' @rdname cmp_xmd_arr
#' @export
cmp_whl_arr <- function(x) {is_cmp_array(x, "whl")}
