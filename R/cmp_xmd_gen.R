#' @name cmp_xmd_gen
#' @family props
#' @title Complete + xmd + Generic Atomic (includes atomic tibbles and atomic
#'   vlists)
#' @description See \code{\link{xmd}}, and \code{\link{is_cmp_generic}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}
#' @export
cmp_xmd_gen_vals <- function() {
  x <- paste0("cmp_", c("atm", xmd_vals()), "_gen")
  names(x) <- rep.int("cmp_xmd_gen", length(x))
  x
}

#' @rdname cmp_xmd_gen
#' @export
cmp_atm_gen <- function(x) {is_cmp_generic(x)}

#' @rdname cmp_xmd_gen
#' @export
cmp_chr_gen <- function(x) {is_cmp_generic(x, "chr")}

#' @rdname cmp_xmd_gen
#' @export
cmp_ch1_gen <- function(x) {is_cmp_generic(x, "ch1")}

#' @rdname cmp_xmd_gen
#' @export
cmp_clr_gen <- function(x) {is_cmp_generic(x, "clr")}

#' @rdname cmp_xmd_gen
#' @export
cmp_evn_gen <- function(x) {is_cmp_generic(x, "evn")}

#' @rdname cmp_xmd_gen
#' @export
cmp_fac_gen <- function(x) {is_cmp_generic(x, "fac")}

#' @rdname cmp_xmd_gen
#' @export
cmp_frc_gen <- function(x) {is_cmp_generic(x, "frc")}

#' @rdname cmp_xmd_gen
#' @export
cmp_ind_gen <- function(x) {is_cmp_generic(x, "ind")}

#' @rdname cmp_xmd_gen
#' @export
cmp_lgc_gen <- function(x) {is_cmp_generic(x, "lgc")}

#' @rdname cmp_xmd_gen
#' @export
cmp_neg_gen <- function(x) {is_cmp_generic(x, "neg")}

#' @rdname cmp_xmd_gen
#' @export
cmp_ngw_gen <- function(x) {is_cmp_generic(x, "ngw")}

#' @rdname cmp_xmd_gen
#' @export
cmp_nng_gen <- function(x) {is_cmp_generic(x, "nng")}

#' @rdname cmp_xmd_gen
#' @export
cmp_nnw_gen <- function(x) {is_cmp_generic(x, "nnw")}

#' @rdname cmp_xmd_gen
#' @export
cmp_nps_gen <- function(x) {is_cmp_generic(x, "nps")}

#' @rdname cmp_xmd_gen
#' @export
cmp_npw_gen <- function(x) {is_cmp_generic(x, "npw")}

#' @rdname cmp_xmd_gen
#' @export
cmp_nst_gen <- function(x) {is_cmp_generic(x, "nst")}

#' @rdname cmp_xmd_gen
#' @export
cmp_num_gen <- function(x) {is_cmp_generic(x, "num")}

#' @rdname cmp_xmd_gen
#' @export
cmp_odd_gen <- function(x) {is_cmp_generic(x, "odd")}

#' @rdname cmp_xmd_gen
#' @export
cmp_ord_gen <- function(x) {is_cmp_generic(x, "ord")}

#' @rdname cmp_xmd_gen
#' @export
cmp_pct_gen <- function(x) {is_cmp_generic(x, "pct")}

#' @rdname cmp_xmd_gen
#' @export
cmp_pos_gen <- function(x) {is_cmp_generic(x, "pos")}

#' @rdname cmp_xmd_gen
#' @export
cmp_ppn_gen <- function(x) {is_cmp_generic(x, "ppn")}

#' @rdname cmp_xmd_gen
#' @export
cmp_psw_gen <- function(x) {is_cmp_generic(x, "psw")}

#' @rdname cmp_xmd_gen
#' @export
cmp_srt_gen <- function(x) {is_cmp_generic(x, "srt")}

#' @rdname cmp_xmd_gen
#' @export
cmp_str_gen <- function(x) {is_cmp_generic(x, "str")}

#' @rdname cmp_xmd_gen
#' @export
cmp_uno_gen <- function(x) {is_cmp_generic(x, "uno")}

#' @rdname cmp_xmd_gen
#' @export
cmp_whl_gen <- function(x) {is_cmp_generic(x, "whl")}
