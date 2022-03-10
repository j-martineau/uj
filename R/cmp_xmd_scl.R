#' @name cmp_xmd_scl
#' @family props
#' @title complete + xmd + atomic scalar
#' @description See \code{\link{xmd}}, and \code{\link{is_cmp_scalar}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}.
#' @export
cmp_xmd_scl_vals <- function() {
  x <- paste0("cmp_", c("atm", xmd_vals()), "_scl")
  names(x) <- rep.int("cmp_xmd_scl", length(x))
  x
}

#' @rdname cmp_xmd_scl
#' @export
cmp_atm_scl <- function(x) {is_cmp_scalar(x)}

#' @rdname cmp_xmd_scl
#' @export
cmp_chr_scl <- function(x) {is_cmp_scalar(x, "chr")}

#' @rdname cmp_xmd_scl
#' @export
cmp_ch1_scl <- function(x) {is_cmp_scalar(x, "ch1")}

#' @rdname cmp_xmd_scl
#' @export
cmp_clr_scl <- function(x) {is_cmp_scalar(x, "clr")}

#' @rdname cmp_xmd_scl
#' @export
cmp_evn_scl <- function(x) {is_cmp_scalar(x, "evn")}

#' @rdname cmp_xmd_scl
#' @export
cmp_fac_scl <- function(x) {is_cmp_scalar(x, "fac")}

#' @rdname cmp_xmd_scl
#' @export
cmp_frc_scl <- function(x) {is_cmp_scalar(x, "frc")}

#' @rdname cmp_xmd_scl
#' @export
cmp_ind_scl <- function(x) {is_cmp_scalar(x, "ind")}

#' @rdname cmp_xmd_scl
#' @export
cmp_lgc_scl <- function(x) {is_cmp_scalar(x, "lgc")}

#' @rdname cmp_xmd_scl
#' @export
cmp_cnb_scl <- function(x) {is_cmp_scalar(x, "cnb")}

#' @rdname cmp_xmd_scl
#' @export
cmp_neg_scl <- function(x) {is_cmp_scalar(x, "neg")}

#' @rdname cmp_xmd_scl
#' @export
cmp_ngw_scl <- function(x) {is_cmp_scalar(x, "ngw")}

#' @rdname cmp_xmd_scl
#' @export
cmp_nng_scl <- function(x) {is_cmp_scalar(x, "nng")}

#' @rdname cmp_xmd_scl
#' @export
cmp_nnw_scl <- function(x) {is_cmp_scalar(x, "nnw")}

#' @rdname cmp_xmd_scl
#' @export
cmp_nps_scl <- function(x) {is_cmp_scalar(x, "nps")}

#' @rdname cmp_xmd_scl
#' @export
cmp_npw_scl <- function(x) {is_cmp_scalar(x, "npw")}

#' @rdname cmp_xmd_scl
#' @export
cmp_nst_scl <- function(x) {is_cmp_scalar(x, "nst")}

#' @rdname cmp_xmd_scl
#' @export
cmp_num_scl <- function(x) {is_cmp_scalar(x, "num")}

#' @rdname cmp_xmd_scl
#' @export
cmp_odd_scl <- function(x) {is_cmp_scalar(x, "odd")}

#' @rdname cmp_xmd_scl
#' @export
cmp_ord_scl <- function(x) {is_cmp_scalar(x, "ord")}

#' @rdname cmp_xmd_scl
#' @export
cmp_pct_scl <- function(x) {is_cmp_scalar(x, "pct")}

#' @rdname cmp_xmd_scl
#' @export
cmp_pos_scl <- function(x) {is_cmp_scalar(x, "pos")}

#' @rdname cmp_xmd_scl
#' @export
cmp_ppn_scl <- function(x) {is_cmp_scalar(x, "ppn")}

#' @rdname cmp_xmd_scl
#' @export
cmp_psw_scl <- function(x) {is_cmp_scalar(x, "psw")}

#' @rdname cmp_xmd_scl
#' @export
cmp_srt_scl <- function(x) {is_cmp_scalar(x, "srt")}

#' @rdname cmp_xmd_scl
#' @export
cmp_str_scl <- function(x) {is_cmp_scalar(x, "str")}

#' @rdname cmp_xmd_scl
#' @export
cmp_uno_scl <- function(x) {is_cmp_scalar(x, "uno")}

#' @rdname cmp_xmd_scl
#' @export
cmp_whl_scl <- function(x) {is_cmp_scalar(x, "whl")}
