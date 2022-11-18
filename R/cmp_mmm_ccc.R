#' @name cmp_mmm_ccc.
#' @family props
#' @title Completeness + Extended Mode + Extended Class Properties
#' @description \tabular{ll}{
#'   \code{cmp_mmm_ccc_vals}   \tab Gets a character vector of all possible
#'                                  \link[=icmp]{completeness} +
#'                                  \link[=mmm]{extended mode} +
#'                                  \link[=ccc]{extended class} properties.  \cr
#'   \code{cmp_[mmm]_[ccc]}    \tab Evaluates whether \code{x} is
#'                                  \link[=cmp]{complete}, of the
#'                                  \link[=mmm]{extended mode} represented by
#'                                  the placeholder \code{[mmm]} and of the
#'                                  \link[=ccc]{extended class} represented by
#'                                  the placeholder \code{[ccc]}.              }
#' @param x An R object.
#' @return \tabular{ll}{
#'   \code{cmp_mmm_ccc_vals}   \tab A character vector.                      \cr
#'   \code{cmp_[mmm]_[ccc]}    \tab A logical scalar.                          }
#' @export
cmp_mmm_ccc. <- function() {help("cmp_mmm_ccc.", package = "uj")}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_mmm_ccc_vals <- function() {
  join <- function(x) {paste0(av(x), collapse = "_")}
  out <- av(apply(expand.grid(mmm = mmm_vals(), ccc = ccc_vals()), 1, join))
  paste0("cmp_", out)
}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_ch1_arr <- function(x) {cmp_arr(x) & ch1_arr(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_ch1_gen <- function(x) {cmp_gen(x) & ch1_gen(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_ch1_dtf <- function(x) {cmp_dtf(x) & ch1_dtf(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_ch1_vls <- function(x) {cmp_vls(x) & ch1_vls(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_ch1_mat <- function(x) {cmp_mat(x) & ch1_mat(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_ch1_mvc <- function(x) {cmp_mvc(x) & ch1_mvc(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_ch1_scl <- function(x) {cmp_scl(x) & ch1_scl(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_ch1_vec <- function(x) {cmp_vec(x) & ch1_vec(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_chr_arr <- function(x) {cmp_arr(x) & chr_arr(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_chr_gen <- function(x) {cmp_gen(x) & chr_gen(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_chr_dtf <- function(x) {cmp_dtf(x) & chr_dtf(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_chr_vls <- function(x) {cmp_vls(x) & chr_vls(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_chr_mat <- function(x) {cmp_mat(x) & chr_mat(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_chr_mvc <- function(x) {cmp_mvc(x) & chr_mvc(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_chr_scl <- function(x) {cmp_scl(x) & chr_scl(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_chr_vec <- function(x) {cmp_vec(x) & chr_vec(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_clr_arr <- function(x) {cmp_arr(x) & clr_arr(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_clr_gen <- function(x) {cmp_gen(x) & clr_gen(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_clr_dtf <- function(x) {cmp_dtf(x) & clr_dtf(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_clr_vls <- function(x) {cmp_vls(x) & clr_vls(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_clr_mat <- function(x) {cmp_mat(x) & clr_mat(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_clr_mvc <- function(x) {cmp_mvc(x) & clr_mvc(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_clr_scl <- function(x) {cmp_scl(x) & clr_scl(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_clr_vec <- function(x) {cmp_vec(x) & clr_vec(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_evn_arr <- function(x) {cmp_arr(x) & evn_arr(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_evn_gen <- function(x) {cmp_gen(x) & evn_gen(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_evn_dtf <- function(x) {cmp_dtf(x) & evn_dtf(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_evn_vls <- function(x) {cmp_vls(x) & evn_vls(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_evn_mat <- function(x) {cmp_mat(x) & evn_mat(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_evn_mvc <- function(x) {cmp_mvc(x) & evn_mvc(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_evn_scl <- function(x) {cmp_scl(x) & evn_scl(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_evn_vec <- function(x) {cmp_vec(x) & evn_vec(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_fac_arr <- function(x) {cmp_arr(x) & fac_arr(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_fac_gen <- function(x) {cmp_gen(x) & fac_gen(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_fac_dtf <- function(x) {cmp_dtf(x) & fac_dtf(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_fac_vls <- function(x) {cmp_vls(x) & fac_vls(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_fac_mat <- function(x) {cmp_mat(x) & fac_mat(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_fac_mvc <- function(x) {cmp_mvc(x) & fac_mvc(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_fac_scl <- function(x) {cmp_scl(x) & fac_scl(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_fac_vec <- function(x) {cmp_vec(x) & fac_vec(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_frc_arr <- function(x) {cmp_arr(x) & frc_arr(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_frc_gen <- function(x) {cmp_gen(x) & frc_gen(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_frc_dtf <- function(x) {cmp_dtf(x) & frc_dtf(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_frc_vls <- function(x) {cmp_vls(x) & frc_vls(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_frc_mat <- function(x) {cmp_mat(x) & frc_mat(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_frc_mvc <- function(x) {cmp_mvc(x) & frc_mvc(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_frc_scl <- function(x) {cmp_scl(x) & frc_scl(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_frc_vec <- function(x) {cmp_vec(x) & frc_vec(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_ind_arr <- function(x) {cmp_arr(x) & ind_arr(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_ind_gen <- function(x) {cmp_gen(x) & ind_gen(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_ind_dtf <- function(x) {cmp_dtf(x) & ind_dtf(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_ind_vls <- function(x) {cmp_vls(x) & ind_vls(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_ind_mat <- function(x) {cmp_mat(x) & ind_mat(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_ind_mvc <- function(x) {cmp_mvc(x) & ind_mvc(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_ind_scl <- function(x) {cmp_scl(x) & ind_scl(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_ind_vec <- function(x) {cmp_vec(x) & ind_vec(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_lgl_arr <- function(x) {cmp_arr(x) & lgl_arr(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_lgl_gen <- function(x) {cmp_gen(x) & lgl_gen(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_lgl_dtf <- function(x) {cmp_dtf(x) & lgl_dtf(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_lgl_vls <- function(x) {cmp_vls(x) & lgl_vls(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_lgl_mat <- function(x) {cmp_mat(x) & lgl_mat(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_lgl_mvc <- function(x) {cmp_mvc(x) & lgl_mvc(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_lgl_scl <- function(x) {cmp_scl(x) & lgl_scl(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_lgl_vec <- function(x) {cmp_vec(x) & lgl_vec(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_neg_arr <- function(x) {cmp_arr(x) & neg_arr(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_neg_gen <- function(x) {cmp_gen(x) & neg_gen(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_neg_dtf <- function(x) {cmp_dtf(x) & neg_dtf(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_neg_vls <- function(x) {cmp_vls(x) & neg_vls(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_neg_mat <- function(x) {cmp_mat(x) & neg_mat(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_neg_mvc <- function(x) {cmp_mvc(x) & neg_mvc(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_neg_scl <- function(x) {cmp_scl(x) & neg_scl(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_neg_vec <- function(x) {cmp_vec(x) & neg_vec(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_ngw_arr <- function(x) {cmp_arr(x) & ngw_arr(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_ngw_gen <- function(x) {cmp_gen(x) & ngw_gen(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_ngw_dtf <- function(x) {cmp_dtf(x) & ngw_dtf(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_ngw_vls <- function(x) {cmp_vls(x) & ngw_vls(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_ngw_mat <- function(x) {cmp_mat(x) & ngw_mat(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_ngw_mvc <- function(x) {cmp_mvc(x) & ngw_mvc(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_ngw_scl <- function(x) {cmp_scl(x) & ngw_scl(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_ngw_vec <- function(x) {cmp_vec(x) & ngw_vec(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_nng_arr <- function(x) {cmp_arr(x) & nng_arr(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_nng_gen <- function(x) {cmp_gen(x) & nng_gen(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_nng_dtf <- function(x) {cmp_dtf(x) & nng_dtf(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_nng_vls <- function(x) {cmp_vls(x) & nng_vls(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_nng_mat <- function(x) {cmp_mat(x) & nng_mat(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_nng_mvc <- function(x) {cmp_mvc(x) & nng_mvc(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_nng_scl <- function(x) {cmp_scl(x) & nng_scl(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_nng_vec <- function(x) {cmp_vec(x) & nng_vec(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_nnw_arr <- function(x) {cmp_arr(x) & nnw_arr(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_nnw_gen <- function(x) {cmp_gen(x) & nnw_gen(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_nnw_dtf <- function(x) {cmp_dtf(x) & nnw_dtf(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_nnw_vls <- function(x) {cmp_vls(x) & nnw_vls(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_nnw_mat <- function(x) {cmp_mat(x) & nnw_mat(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_nnw_mvc <- function(x) {cmp_mvc(x) & nnw_mvc(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_nnw_scl <- function(x) {cmp_scl(x) & nnw_scl(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_nnw_vec <- function(x) {cmp_vec(x) & nnw_vec(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_nps_arr <- function(x) {cmp_arr(x) & nps_arr(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_nps_gen <- function(x) {cmp_gen(x) & nps_gen(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_nps_dtf <- function(x) {cmp_dtf(x) & nps_dtf(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_nps_vls <- function(x) {cmp_vls(x) & nps_vls(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_nps_mat <- function(x) {cmp_mat(x) & nps_mat(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_nps_mvc <- function(x) {cmp_mvc(x) & nps_mvc(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_nps_scl <- function(x) {cmp_scl(x) & nps_scl(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_nps_vec <- function(x) {cmp_vec(x) & nps_vec(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_npw_arr <- function(x) {cmp_arr(x) & npw_arr(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_npw_gen <- function(x) {cmp_gen(x) & npw_gen(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_npw_dtf <- function(x) {cmp_dtf(x) & npw_dtf(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_npw_vls <- function(x) {cmp_vls(x) & npw_vls(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_npw_mat <- function(x) {cmp_mat(x) & npw_mat(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_npw_mvc <- function(x) {cmp_mvc(x) & npw_mvc(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_npw_scl <- function(x) {cmp_scl(x) & npw_scl(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_npw_vec <- function(x) {cmp_vec(x) & npw_vec(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_nst_arr <- function(x) {cmp_arr(x) & nst_arr(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_nst_gen <- function(x) {cmp_gen(x) & nst_gen(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_nst_dtf <- function(x) {cmp_dtf(x) & nst_dtf(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_nst_vls <- function(x) {cmp_vls(x) & nst_vls(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_nst_mat <- function(x) {cmp_mat(x) & nst_mat(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_nst_mvc <- function(x) {cmp_mvc(x) & nst_mvc(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_nst_scl <- function(x) {cmp_scl(x) & nst_scl(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_nst_vec <- function(x) {cmp_vec(x) & nst_vec(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_num_arr <- function(x) {cmp_arr(x) & num_arr(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_num_gen <- function(x) {cmp_gen(x) & num_gen(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_num_dtf <- function(x) {cmp_dtf(x) & num_dtf(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_num_vls <- function(x) {cmp_vls(x) & num_vls(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_num_mat <- function(x) {cmp_mat(x) & num_mat(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_num_mvc <- function(x) {cmp_mvc(x) & num_mvc(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_num_scl <- function(x) {cmp_scl(x) & num_scl(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_num_vec <- function(x) {cmp_vec(x) & num_vec(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_odd_arr <- function(x) {cmp_arr(x) & odd_arr(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_odd_gen <- function(x) {cmp_gen(x) & odd_gen(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_odd_dtf <- function(x) {cmp_dtf(x) & odd_dtf(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_odd_vls <- function(x) {cmp_vls(x) & odd_vls(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_odd_mat <- function(x) {cmp_mat(x) & odd_mat(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_odd_mvc <- function(x) {cmp_mvc(x) & odd_mvc(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_odd_scl <- function(x) {cmp_scl(x) & odd_scl(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_odd_vec <- function(x) {cmp_vec(x) & odd_vec(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_ord_arr <- function(x) {cmp_arr(x) & ord_arr(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_ord_gen <- function(x) {cmp_gen(x) & ord_gen(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_ord_dtf <- function(x) {cmp_dtf(x) & ord_dtf(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_ord_vls <- function(x) {cmp_vls(x) & ord_vls(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_ord_mat <- function(x) {cmp_mat(x) & ord_mat(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_ord_mvc <- function(x) {cmp_mvc(x) & ord_mvc(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_ord_scl <- function(x) {cmp_scl(x) & ord_scl(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_ord_vec <- function(x) {cmp_vec(x) & ord_vec(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_pct_arr <- function(x) {cmp_arr(x) & pct_arr(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_pct_gen <- function(x) {cmp_gen(x) & pct_gen(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_pct_dtf <- function(x) {cmp_dtf(x) & pct_dtf(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_pct_vls <- function(x) {cmp_vls(x) & pct_vls(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_pct_mat <- function(x) {cmp_mat(x) & pct_mat(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_pct_mvc <- function(x) {cmp_mvc(x) & pct_mvc(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_pct_scl <- function(x) {cmp_scl(x) & pct_scl(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_pct_vec <- function(x) {cmp_vec(x) & pct_vec(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_pos_arr <- function(x) {cmp_arr(x) & pos_arr(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_pos_gen <- function(x) {cmp_gen(x) & pos_gen(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_pos_dtf <- function(x) {cmp_dtf(x) & pos_dtf(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_pos_vls <- function(x) {cmp_vls(x) & pos_vls(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_pos_mat <- function(x) {cmp_mat(x) & pos_mat(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_pos_mvc <- function(x) {cmp_mvc(x) & pos_mvc(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_pos_scl <- function(x) {cmp_scl(x) & pos_scl(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_pos_vec <- function(x) {cmp_vec(x) & pos_vec(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_ppn_arr <- function(x) {cmp_arr(x) & ppn_arr(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_ppn_gen <- function(x) {cmp_gen(x) & ppn_gen(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_ppn_dtf <- function(x) {cmp_dtf(x) & ppn_dtf(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_ppn_vls <- function(x) {cmp_vls(x) & ppn_vls(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_ppn_mat <- function(x) {cmp_mat(x) & ppn_mat(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_ppn_mvc <- function(x) {cmp_mvc(x) & ppn_mvc(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_ppn_scl <- function(x) {cmp_scl(x) & ppn_scl(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_ppn_vec <- function(x) {cmp_vec(x) & ppn_vec(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_psw_arr <- function(x) {cmp_arr(x) & psw_arr(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_psw_gen <- function(x) {cmp_gen(x) & psw_gen(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_psw_dtf <- function(x) {cmp_dtf(x) & psw_dtf(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_psw_vls <- function(x) {cmp_vls(x) & psw_vls(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_psw_mat <- function(x) {cmp_mat(x) & psw_mat(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_psw_mvc <- function(x) {cmp_mvc(x) & psw_mvc(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_psw_scl <- function(x) {cmp_scl(x) & psw_scl(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_psw_vec <- function(x) {cmp_vec(x) & psw_vec(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_srt_arr <- function(x) {cmp_arr(x) & srt_arr(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_srt_gen <- function(x) {cmp_gen(x) & srt_gen(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_srt_dtf <- function(x) {cmp_dtf(x) & srt_dtf(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_srt_vls <- function(x) {cmp_vls(x) & srt_vls(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_srt_mat <- function(x) {cmp_mat(x) & srt_mat(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_srt_mvc <- function(x) {cmp_mvc(x) & srt_mvc(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_srt_scl <- function(x) {cmp_scl(x) & srt_scl(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_srt_vec <- function(x) {cmp_vec(x) & srt_vec(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_str_arr <- function(x) {cmp_arr(x) & str_arr(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_str_gen <- function(x) {cmp_gen(x) & str_gen(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_str_dtf <- function(x) {cmp_dtf(x) & str_dtf(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_str_vls <- function(x) {cmp_vls(x) & str_vls(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_str_mat <- function(x) {cmp_mat(x) & str_mat(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_str_mvc <- function(x) {cmp_mvc(x) & str_mvc(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_str_scl <- function(x) {cmp_scl(x) & str_scl(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_str_vec <- function(x) {cmp_vec(x) & str_vec(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_uno_arr <- function(x) {cmp_arr(x) & uno_arr(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_uno_gen <- function(x) {cmp_gen(x) & uno_gen(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_uno_dtf <- function(x) {cmp_dtf(x) & uno_dtf(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_uno_vls <- function(x) {cmp_vls(x) & uno_vls(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_uno_mat <- function(x) {cmp_mat(x) & uno_mat(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_uno_mvc <- function(x) {cmp_mvc(x) & uno_mvc(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_uno_scl <- function(x) {cmp_scl(x) & uno_scl(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_uno_vec <- function(x) {cmp_vec(x) & uno_vec(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_whl_arr <- function(x) {cmp_arr(x) & whl_arr(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_whl_gen <- function(x) {cmp_gen(x) & whl_gen(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_whl_dtf <- function(x) {cmp_dtf(x) & whl_dtf(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_whl_vls <- function(x) {cmp_vls(x) & whl_vls(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_whl_mat <- function(x) {cmp_mat(x) & whl_mat(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_whl_mvc <- function(x) {cmp_mvc(x) & whl_mvc(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_whl_scl <- function(x) {cmp_scl(x) & whl_scl(x)}

#' @rdname cmp_mmm_ccc.
#' @export
cmp_whl_vec <- function(x) {cmp_vec(x) & whl_vec(x)}
