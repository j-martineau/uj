#' @name cmp_mmm_atb
#' @family props
#' @title Complete + Extended Mode + Atomic Tibble (cmp + mmm + atb)
#' @description See \code{\link{mmm}}, and \code{\link{is_cmp_tibble}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}.
#' @export
cmp_mmm_atb_vals <- function() {
  x <- paste0("cmp_", c("atm", mmm_vals()), "_atb")
  names(x) <- rep.int("cmp_mmm_atb", length(x))
  x
}

#' @rdname cmp_mmm_atb
#' @export
cmp_atm_atb <- function(x) {is_cmp_tibble(x)}

#' @rdname cmp_mmm_atb
#' @export
cmp_chr_atb <- function(x) {is_cmp_tibble(x, "chr")}

#' @rdname cmp_mmm_atb
#' @export
cmp_ch1_atb <- function(x) {is_cmp_tibble(x, "ch1")}

#' @rdname cmp_mmm_atb
#' @export
cmp_clr_atb <- function(x) {is_cmp_tibble(x, "clr")}

#' @rdname cmp_mmm_atb
#' @export
cmp_evn_atb <- function(x) {is_cmp_tibble(x, "evn")}

#' @rdname cmp_mmm_atb
#' @export
cmp_fac_atb <- function(x) {is_cmp_tibble(x, "fac")}

#' @rdname cmp_mmm_atb
#' @export
cmp_frc_atb <- function(x) {is_cmp_tibble(x, "frc")}

#' @rdname cmp_mmm_atb
#' @export
cmp_ind_atb <- function(x) {is_cmp_tibble(x, "ind")}

#' @rdname cmp_mmm_atb
#' @export
cmp_lgc_atb <- function(x) {is_cmp_tibble(x, "lgc")}

#' @rdname cmp_mmm_atb
#' @export
cmp_cnb_atb <- function(x) {is_cmp_tibble(x, "cnb")}

#' @rdname cmp_mmm_atb
#' @export
cmp_neg_atb <- function(x) {is_cmp_tibble(x, "neg")}

#' @rdname cmp_mmm_atb
#' @export
cmp_ngw_atb <- function(x) {is_cmp_tibble(x, "ngw")}

#' @rdname cmp_mmm_atb
#' @export
cmp_nng_atb <- function(x) {is_cmp_tibble(x, "nng")}

#' @rdname cmp_mmm_atb
#' @export
cmp_nnw_atb <- function(x) {is_cmp_tibble(x, "nnw")}

#' @rdname cmp_mmm_atb
#' @export
cmp_nps_atb <- function(x) {is_cmp_tibble(x, "nps")}

#' @rdname cmp_mmm_atb
#' @export
cmp_npw_atb <- function(x) {is_cmp_tibble(x, "npw")}

#' @rdname cmp_mmm_atb
#' @export
cmp_nst_atb <- function(x) {is_cmp_tibble(x, "nst")}

#' @rdname cmp_mmm_atb
#' @export
cmp_num_atb <- function(x) {is_cmp_tibble(x, "num")}

#' @rdname cmp_mmm_atb
#' @export
cmp_odd_atb <- function(x) {is_cmp_tibble(x, "odd")}

#' @rdname cmp_mmm_atb
#' @export
cmp_ord_atb <- function(x) {is_cmp_tibble(x, "ord")}

#' @rdname cmp_mmm_atb
#' @export
cmp_pct_atb <- function(x) {is_cmp_tibble(x, "pct")}

#' @rdname cmp_mmm_atb
#' @export
cmp_pos_atb <- function(x) {is_cmp_tibble(x, "pos")}

#' @rdname cmp_mmm_atb
#' @export
cmp_ppn_atb <- function(x) {is_cmp_tibble(x, "ppn")}

#' @rdname cmp_mmm_atb
#' @export
cmp_psw_atb <- function(x) {is_cmp_tibble(x, "psw")}

#' @rdname cmp_mmm_atb
#' @export
cmp_srt_atb <- function(x) {is_cmp_tibble(x, "srt")}

#' @rdname cmp_mmm_atb
#' @export
cmp_str_atb <- function(x) {is_cmp_tibble(x, "str")}

#' @rdname cmp_mmm_atb
#' @export
cmp_uno_atb <- function(x) {is_cmp_tibble(x, "uno")}

#' @rdname cmp_mmm_atb
#' @export
cmp_whl_atb <- function(x) {is_cmp_tibble(x, "whl")}
