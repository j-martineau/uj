#' @name cmp_mmm_vec
#' @family props
#' @title Complete + Extended Mode + Atomic Vect (cmp + mmm + vec)
#' @description See \code{\link{mmm}}, and \code{\link{is_cmp_vect}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}.
#' @export
cmp_mmm_vec_vals <- function() {
  x <- paste0("cmp_", c("atm", mmm_vals()), "_vec")
  names(x) <- rep.int("cmp_mmm_vec", length(x))
  x
}

#' @rdname cmp_mmm_vec
#' @export
cmp_atm_vec <- function(x) {is_cmp_vect(x)}

#' @rdname cmp_mmm_vec
#' @export
cmp_chr_vec <- function(x) {is_cmp_vect(x, "chr")}

#' @rdname cmp_mmm_vec
#' @export
cmp_ch1_vec <- function(x) {is_cmp_vect(x, "ch1")}

#' @rdname cmp_mmm_vec
#' @export
cmp_clr_vec <- function(x) {is_cmp_vect(x, "clr")}

#' @rdname cmp_mmm_vec
#' @export
cmp_evn_vec <- function(x) {is_cmp_vect(x, "evn")}

#' @rdname cmp_mmm_vec
#' @export
cmp_fac_vec <- function(x) {is_cmp_vect(x, "fac")}

#' @rdname cmp_mmm_vec
#' @export
cmp_frc_vec <- function(x) {is_cmp_vect(x, "frc")}

#' @rdname cmp_mmm_vec
#' @export
cmp_ind_vec <- function(x) {is_cmp_vect(x, "ind")}

#' @rdname cmp_mmm_vec
#' @export
cmp_lgc_vec <- function(x) {is_cmp_vect(x, "lgc")}

#' @rdname cmp_mmm_vec
#' @export
cmp_cnb_vec <- function(x) {is_cmp_vect(x, "cnb")}

#' @rdname cmp_mmm_vec
#' @export
cmp_neg_vec <- function(x) {is_cmp_vect(x, "neg")}

#' @rdname cmp_mmm_vec
#' @export
cmp_ngw_vec <- function(x) {is_cmp_vect(x, "ngw")}

#' @rdname cmp_mmm_vec
#' @export
cmp_nng_vec <- function(x) {is_cmp_vect(x, "nng")}

#' @rdname cmp_mmm_vec
#' @export
cmp_nnw_vec <- function(x) {is_cmp_vect(x, "nnw")}

#' @rdname cmp_mmm_vec
#' @export
cmp_nps_vec <- function(x) {is_cmp_vect(x, "nps")}

#' @rdname cmp_mmm_vec
#' @export
cmp_npw_vec <- function(x) {is_cmp_vect(x, "npw")}

#' @rdname cmp_mmm_vec
#' @export
cmp_nst_vec <- function(x) {is_cmp_vect(x, "nst")}

#' @rdname cmp_mmm_vec
#' @export
cmp_num_vec <- function(x) {is_cmp_vect(x, "num")}

#' @rdname cmp_mmm_vec
#' @export
cmp_odd_vec <- function(x) {is_cmp_vect(x, "odd")}

#' @rdname cmp_mmm_vec
#' @export
cmp_ord_vec <- function(x) {is_cmp_vect(x, "ord")}

#' @rdname cmp_mmm_vec
#' @export
cmp_pct_vec <- function(x) {is_cmp_vect(x, "pct")}

#' @rdname cmp_mmm_vec
#' @export
cmp_pos_vec <- function(x) {is_cmp_vect(x, "pos")}

#' @rdname cmp_mmm_vec
#' @export
cmp_ppn_vec <- function(x) {is_cmp_vect(x, "ppn")}

#' @rdname cmp_mmm_vec
#' @export
cmp_psw_vec <- function(x) {is_cmp_vect(x, "psw")}

#' @rdname cmp_mmm_vec
#' @export
cmp_srt_vec <- function(x) {is_cmp_vect(x, "srt")}

#' @rdname cmp_mmm_vec
#' @export
cmp_str_vec <- function(x) {is_cmp_vect(x, "str")}

#' @rdname cmp_mmm_vec
#' @export
cmp_uno_vec <- function(x) {is_cmp_vect(x, "uno")}

#' @rdname cmp_mmm_vec
#' @export
cmp_whl_vec <- function(x) {is_cmp_vect(x, "whl")}
