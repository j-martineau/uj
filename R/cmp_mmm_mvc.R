#' @name cmp_mmm_mvc
#' @family props
#' @title Complete + Extended Mode + Atomic Mvect (cmp + mmm + mvc)
#' @description See \code{\link{mmm}}, and \code{\link{is_cmp_mvect}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}.
#' @export
cmp_mmm_mvc_vals <- function() {
  x <- paste0("cmp_", c("atm", mmm_vals()), "_mvc")
  names(x) <- rep.int("cmp_mmm_mvc", length(x))
  x
}

#' @rdname cmp_mmm_mvc
#' @export
cmp_atm_mvc <- function(x) {is_cmp_mvect(x)}

#' @rdname cmp_mmm_mvc
#' @export
cmp_chr_mvc <- function(x) {is_cmp_mvect(x, "chr")}

#' @rdname cmp_mmm_mvc
#' @export
cmp_ch1_mvc <- function(x) {is_cmp_mvect(x, "ch1")}

#' @rdname cmp_mmm_mvc
#' @export
cmp_clr_mvc <- function(x) {is_cmp_mvect(x, "clr")}

#' @rdname cmp_mmm_mvc
#' @export
cmp_evn_mvc <- function(x) {is_cmp_mvect(x, "evn")}

#' @rdname cmp_mmm_mvc
#' @export
cmp_fac_mvc <- function(x) {is_cmp_mvect(x, "fac")}

#' @rdname cmp_mmm_mvc
#' @export
cmp_frc_mvc <- function(x) {is_cmp_mvect(x, "frc")}

#' @rdname cmp_mmm_mvc
#' @export
cmp_ind_mvc <- function(x) {is_cmp_mvect(x, "ind")}

#' @rdname cmp_mmm_mvc
#' @export
cmp_lgc_mvc <- function(x) {is_cmp_mvect(x, "lgc")}

#' @rdname cmp_mmm_mvc
#' @export
cmp_cnb_mvc <- function(x) {is_cmp_mvect(x, "cnb")}

#' @rdname cmp_mmm_mvc
#' @export
cmp_neg_mvc <- function(x) {is_cmp_mvect(x, "neg")}

#' @rdname cmp_mmm_mvc
#' @export
cmp_ngw_mvc <- function(x) {is_cmp_mvect(x, "ngw")}

#' @rdname cmp_mmm_mvc
#' @export
cmp_nng_mvc <- function(x) {is_cmp_mvect(x, "nng")}

#' @rdname cmp_mmm_mvc
#' @export
cmp_nnw_mvc <- function(x) {is_cmp_mvect(x, "nnw")}

#' @rdname cmp_mmm_mvc
#' @export
cmp_nps_mvc <- function(x) {is_cmp_mvect(x, "nps")}

#' @rdname cmp_mmm_mvc
#' @export
cmp_npw_mvc <- function(x) {is_cmp_mvect(x, "npw")}

#' @rdname cmp_mmm_mvc
#' @export
cmp_nst_mvc <- function(x) {is_cmp_mvect(x, "nst")}

#' @rdname cmp_mmm_mvc
#' @export
cmp_num_mvc <- function(x) {is_cmp_mvect(x, "num")}

#' @rdname cmp_mmm_mvc
#' @export
cmp_odd_mvc <- function(x) {is_cmp_mvect(x, "odd")}

#' @rdname cmp_mmm_mvc
#' @export
cmp_ord_mvc <- function(x) {is_cmp_mvect(x, "ord")}

#' @rdname cmp_mmm_mvc
#' @export
cmp_pct_mvc <- function(x) {is_cmp_mvect(x, "pct")}

#' @rdname cmp_mmm_mvc
#' @export
cmp_pos_mvc <- function(x) {is_cmp_mvect(x, "pos")}

#' @rdname cmp_mmm_mvc
#' @export
cmp_ppn_mvc <- function(x) {is_cmp_mvect(x, "ppn")}

#' @rdname cmp_mmm_mvc
#' @export
cmp_psw_mvc <- function(x) {is_cmp_mvect(x, "psw")}

#' @rdname cmp_mmm_mvc
#' @export
cmp_srt_mvc <- function(x) {is_cmp_mvect(x, "srt")}

#' @rdname cmp_mmm_mvc
#' @export
cmp_str_mvc <- function(x) {is_cmp_mvect(x, "str")}

#' @rdname cmp_mmm_mvc
#' @export
cmp_uno_mvc <- function(x) {is_cmp_mvect(x, "uno")}

#' @rdname cmp_mmm_mvc
#' @export
cmp_whl_mvc <- function(x) {is_cmp_mvect(x, "whl")}
