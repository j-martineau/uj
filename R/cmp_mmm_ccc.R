#' @name cmp_mmm_ccc_uj
#' @family props
#' @title Complete + Extended Mode + Extended Class (cmp + mmm)
#' @description Check whether an object is complete, of a specific extended
#'   mode, and of a specific extended class. See \code{\link{icmp}}, and
#'   \code{\link{immm}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE} except for \code{cmp_mmm_ccc_vals}, which
#'   returns a character vector.
#' @export
cmp_mmm_ccc_uj <- function() {help("cmp_mmm_uj", package = "uj")}

#' @describeIn cmp_mmm_ccc_uj Get a character vector of all possible complete +
#'   extended mode + extended class properties.
#' @export
cmp_mmm_ccc_vals <- function() {
  x <- expand.grid(mmm = mmm_vals(), ccc = ccc_vals())
  x <- av(apply(x, 1, paste0, collapse = "_"))
  x <- paste0("cmp_", x)
  sort(x)
}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete onechar array?
#'   (\code{\link{icmp}}, \code{\link{ich1}}, \code{\link{iarr}})
#' @export
cmp_ch1_arr <- function(x) {cmp_arr(x) & ch1_arr(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete onechar
#'   generic?
#'   (\code{\link{icmp}}, \code{\link{ich1}}, \code{\link{igen}})
#' @export
cmp_ch1_gen <- function(x) {cmp_gen(x) & ch1_gen(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete onechar
#'   tabular?
#'   (\code{\link{icmp}}, \code{\link{ich1}}, \code{\link{itab}})
#' @export
cmp_ch1_tab <- function(x) {cmp_tab(x) & ch1_tab(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete onechar
#'   vlist?
#'   (\code{\link{icmp}}, \code{\link{ich1}}, \code{\link{ivls}})
#' @export
cmp_ch1_vls <- function(x) {cmp_vls(x) & ch1_vls(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete onechar
#'   vtype?
#'   (\code{\link{icmp}}, \code{\link{ich1}}, \code{\link{ivtp}})
#' @export
cmp_ch1_vtp <- function(x) {cmp_vtp(x) & ch1_vtp(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete onechar
#'   matrix?
#'   (\code{\link{icmp}}, \code{\link{ich1}}, \code{\link{imat}})
#' @export
cmp_ch1_mat <- function(x) {cmp_mat(x) & ch1_mat(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete onechar
#'   mvector?
#'   (\code{\link{icmp}}, \code{\link{ich1}}, \code{\link{imvc}})
#' @export
cmp_ch1_mvc <- function(x) {cmp_mvc(x) & ch1_mvc(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete onechar
#'   scalar?
#'   (\code{\link{icmp}}, \code{\link{ich1}}, \code{\link{iscl}})
#' @export
cmp_ch1_scl <- function(x) {cmp_scl(x) & ch1_scl(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete onechar
#'   vector?
#'   (\code{\link{icmp}}, \code{\link{ich1}}, \code{\link{ivec}})
#' @export
cmp_ch1_vec <- function(x) {cmp_vec(x) & ch1_vec(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete character
#'   array?
#'   (\code{\link{icmp}}, \code{\link{ichr}}, \code{\link{iarr}})
#' @export
cmp_chr_arr <- function(x) {cmp_arr(x) & chr_arr(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete character
#'   generic?
#'   (\code{\link{icmp}}, \code{\link{ichr}}, \code{\link{igen}})
#' @export
cmp_chr_gen <- function(x) {cmp_gen(x) & chr_gen(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete character
#'   tabular?
#'   (\code{\link{icmp}}, \code{\link{ichr}}, \code{\link{itab}})
#' @export
cmp_chr_tab <- function(x) {cmp_tab(x) & chr_tab(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete character
#'   vlist?
#'   (\code{\link{icmp}}, \code{\link{ichr}}, \code{\link{ivls}})
#' @export
cmp_chr_vls <- function(x) {cmp_vls(x) & chr_vls(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete character
#'   vtype?
#'   (\code{\link{icmp}}, \code{\link{ichr}}, \code{\link{ivtp}})
#' @export
cmp_chr_vtp <- function(x) {cmp_vtp(x) & chr_vtp(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete character
#'   matrix?
#'   (\code{\link{icmp}}, \code{\link{ichr}}, \code{\link{imat}})
#' @export
cmp_chr_mat <- function(x) {cmp_mat(x) & chr_mat(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete character
#'   mvector?
#'   (\code{\link{icmp}}, \code{\link{ichr}}, \code{\link{imvc}})
#' @export
cmp_chr_mvc <- function(x) {cmp_mvc(x) & chr_mvc(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete character
#'   scalar?
#'   (\code{\link{icmp}}, \code{\link{ichr}}, \code{\link{iscl}})
#' @export
cmp_chr_scl <- function(x) {cmp_scl(x) & chr_scl(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete character
#'   vector?
#'   (\code{\link{icmp}}, \code{\link{ichr}}, \code{\link{ivec}})
#' @export
cmp_chr_vec <- function(x) {cmp_vec(x) & chr_vec(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete color
#'   array?
#'   (\code{\link{icmp}}, \code{\link{iclr}}, \code{\link{iarr}})
#' @export
cmp_clr_arr <- function(x) {cmp_arr(x) & clr_arr(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete color
#'   generic?
#'   (\code{\link{icmp}}, \code{\link{iclr}}, \code{\link{igen}})
#' @export
cmp_clr_gen <- function(x) {cmp_gen(x) & clr_gen(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete color
#'   tabular?
#'   (\code{\link{icmp}}, \code{\link{iclr}}, \code{\link{itab}})
#' @export
cmp_clr_tab <- function(x) {cmp_tab(x) & clr_tab(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete color
#'   vlist?
#'   (\code{\link{icmp}}, \code{\link{iclr}}, \code{\link{ivls}})
#' @export
cmp_clr_vls <- function(x) {cmp_vls(x) & clr_vls(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete color
#'   vtype?
#'   (\code{\link{icmp}}, \code{\link{iclr}}, \code{\link{ivtp}})
#' @export
cmp_clr_vtp <- function(x) {cmp_vtp(x) & clr_vtp(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete color
#'   matrix?
#'   (\code{\link{icmp}}, \code{\link{iclr}}, \code{\link{imat}})
#' @export
cmp_clr_mat <- function(x) {cmp_mat(x) & clr_mat(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete color
#'   mvector?
#'   (\code{\link{icmp}}, \code{\link{iclr}}, \code{\link{imvc}})
#' @export
cmp_clr_mvc <- function(x) {cmp_mvc(x) & clr_mvc(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete color
#'   scalar?
#'   (\code{\link{icmp}}, \code{\link{iclr}}, \code{\link{iscl}})
#' @export
cmp_clr_scl <- function(x) {cmp_scl(x) & clr_scl(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete color
#'   vector?
#'   (\code{\link{icmp}}, \code{\link{iclr}}, \code{\link{ivec}})
#' @export
cmp_clr_vec <- function(x) {cmp_vec(x) & clr_vec(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete even-valued
#'   whole-number array?
#'   (\code{\link{icmp}}, \code{\link{ievn}}, \code{\link{iarr}})
#' @export
cmp_evn_arr <- function(x) {cmp_arr(x) & evn_arr(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete even-valued
#'   whole-number generic?
#'   (\code{\link{icmp}}, \code{\link{ievn}}, \code{\link{igen}})
#' @export
cmp_evn_gen <- function(x) {cmp_gen(x) & evn_gen(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete even-valued
#'   whole-number tabular?
#'   (\code{\link{icmp}}, \code{\link{ievn}}, \code{\link{itab}})
#' @export
cmp_evn_tab <- function(x) {cmp_tab(x) & evn_tab(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete even-valued
#'   whole-number vlist?
#'   (\code{\link{icmp}}, \code{\link{ievn}}, \code{\link{ivls}})
#' @export
cmp_evn_vls <- function(x) {cmp_vls(x) & evn_vls(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete even-valued
#'   whole-number vtype?
#'   (\code{\link{icmp}}, \code{\link{ievn}}, \code{\link{ivtp}})
#' @export
cmp_evn_vtp <- function(x) {cmp_vtp(x) & evn_vtp(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete even-valued
#'   whole-number matrix?
#'   (\code{\link{icmp}}, \code{\link{ievn}}, \code{\link{imat}})
#' @export
cmp_evn_mat <- function(x) {cmp_mat(x) & evn_mat(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete even-valued
#'   whole-number mvector?
#'   (\code{\link{icmp}}, \code{\link{ievn}}, \code{\link{imvc}})
#' @export
cmp_evn_mvc <- function(x) {cmp_mvc(x) & evn_mvc(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete even-valued
#'   whole-number scalar?
#'   (\code{\link{icmp}}, \code{\link{ievn}}, \code{\link{iscl}})
#' @export
cmp_evn_scl <- function(x) {cmp_scl(x) & evn_scl(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete even-valued
#'   whole-number vector?
#'   (\code{\link{icmp}}, \code{\link{ievn}}, \code{\link{ivec}})
#' @export
cmp_evn_vec <- function(x) {cmp_vec(x) & evn_vec(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete factor
#'   array?
#'   (\code{\link{icmp}}, \code{\link{ifac}}, \code{\link{iarr}})
#' @export
cmp_fac_arr <- function(x) {cmp_arr(x) & fac_arr(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete factor
#'   generic?
#'   (\code{\link{icmp}}, \code{\link{ifac}}, \code{\link{igen}})
#' @export
cmp_fac_gen <- function(x) {cmp_gen(x) & fac_gen(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete factor
#'   tabular?
#'   (\code{\link{icmp}}, \code{\link{ifac}}, \code{\link{itab}})
#' @export
cmp_fac_tab <- function(x) {cmp_tab(x) & fac_tab(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete factor
#'   vlist?
#'   (\code{\link{icmp}}, \code{\link{ifac}}, \code{\link{ivls}})
#' @export
cmp_fac_vls <- function(x) {cmp_vls(x) & fac_vls(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete factor
#'   vtype?
#'   (\code{\link{icmp}}, \code{\link{ifac}}, \code{\link{ivtp}})
#' @export
cmp_fac_vtp <- function(x) {cmp_vtp(x) & fac_vtp(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete factor
#'   matrix?
#'   (\code{\link{icmp}}, \code{\link{ifac}}, \code{\link{imat}})
#' @export
cmp_fac_mat <- function(x) {cmp_mat(x) & fac_mat(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete factor
#'   mvector?
#'   (\code{\link{icmp}}, \code{\link{ifac}}, \code{\link{imvc}})
#' @export
cmp_fac_mvc <- function(x) {cmp_mvc(x) & fac_mvc(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete factor
#'   scalar?
#'   (\code{\link{icmp}}, \code{\link{ifac}}, \code{\link{iscl}})
#' @export
cmp_fac_scl <- function(x) {cmp_scl(x) & fac_scl(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete factor
#'   vector?
#'   (\code{\link{icmp}}, \code{\link{ifac}}, \code{\link{ivec}})
#' @export
cmp_fac_vec <- function(x) {cmp_vec(x) & fac_vec(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete fractional-valued
#'   numeric array?
#'   (\code{\link{icmp}}, \code{\link{ifrc}}, \code{\link{iarr}})
#' @export
cmp_frc_arr <- function(x) {cmp_arr(x) & frc_arr(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete fractional-valued
#'   numeric generic?
#'   (\code{\link{icmp}}, \code{\link{ifrc}}, \code{\link{igen}})
#' @export
cmp_frc_gen <- function(x) {cmp_gen(x) & frc_gen(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete fractional-valued
#'   numeric tabular?
#'   (\code{\link{icmp}}, \code{\link{ifrc}}, \code{\link{itab}})
#' @export
cmp_frc_tab <- function(x) {cmp_tab(x) & frc_tab(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete fractional-valued
#'   numeric vlist?
#'   (\code{\link{icmp}}, \code{\link{ifrc}}, \code{\link{ivls}})
#' @export
cmp_frc_vls <- function(x) {cmp_vls(x) & frc_vls(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete fractional-valued
#'   numeric vtype?
#'   (\code{\link{icmp}}, \code{\link{ifrc}}, \code{\link{ivtp}})
#' @export
cmp_frc_vtp <- function(x) {cmp_vtp(x) & frc_vtp(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete fractional-valued
#'   numeric matrix?
#'   (\code{\link{icmp}}, \code{\link{ifrc}}, \code{\link{imat}})
#' @export
cmp_frc_mat <- function(x) {cmp_mat(x) & frc_mat(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete fractional-valued
#'   numeric mvector?
#'   (\code{\link{icmp}}, \code{\link{ifrc}}, \code{\link{imvc}})
#' @export
cmp_frc_mvc <- function(x) {cmp_mvc(x) & frc_mvc(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete fractional-valued
#'   numeric scalar?
#'   (\code{\link{icmp}}, \code{\link{ifrc}}, \code{\link{iscl}})
#' @export
cmp_frc_scl <- function(x) {cmp_scl(x) & frc_scl(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete fractional-valued
#'   numeric vector?
#'   (\code{\link{icmp}}, \code{\link{ifrc}}, \code{\link{ivec}})
#' @export
cmp_frc_vec <- function(x) {cmp_vec(x) & frc_vec(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete indexer
#'   array?
#'   (\code{\link{icmp}}, \code{\link{iind}}, \code{\link{iarr}})
#' @export
cmp_ind_arr <- function(x) {cmp_arr(x) & ind_arr(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete indexer
#'   generic?
#'   (\code{\link{icmp}}, \code{\link{iind}}, \code{\link{igen}})
#' @export
cmp_ind_gen <- function(x) {cmp_gen(x) & ind_gen(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete indexer
#'   tabular?
#'   (\code{\link{icmp}}, \code{\link{iind}}, \code{\link{itab}})
#' @export
cmp_ind_tab <- function(x) {cmp_tab(x) & ind_tab(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete indexer
#'   vlist?
#'   (\code{\link{icmp}}, \code{\link{iind}}, \code{\link{ivls}})
#' @export
cmp_ind_vls <- function(x) {cmp_vls(x) & ind_vls(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete indexer
#'   vtype?
#'   (\code{\link{icmp}}, \code{\link{iind}}, \code{\link{ivtp}})
#' @export
cmp_ind_vtp <- function(x) {cmp_vtp(x) & ind_vtp(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete indexer
#'   matrix?
#'   (\code{\link{icmp}}, \code{\link{iind}}, \code{\link{imat}})
#' @export
cmp_ind_mat <- function(x) {cmp_mat(x) & ind_mat(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete indexer
#'   mvector?
#'   (\code{\link{icmp}}, \code{\link{iind}}, \code{\link{imvc}})
#' @export
cmp_ind_mvc <- function(x) {cmp_mvc(x) & ind_mvc(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete indexer
#'   scalar?
#'   (\code{\link{icmp}}, \code{\link{iind}}, \code{\link{iscl}})
#' @export
cmp_ind_scl <- function(x) {cmp_scl(x) & ind_scl(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete indexer
#'   vector?
#'   (\code{\link{icmp}}, \code{\link{iind}}, \code{\link{ivec}})
#' @export
cmp_ind_vec <- function(x) {cmp_vec(x) & ind_vec(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete logical
#'   array?
#'   (\code{\link{icmp}}, \code{\link{ilgl}}, \code{\link{iarr}})
#' @export
cmp_lgl_arr <- function(x) {cmp_arr(x) & lgl_arr(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete logical
#'   generic?
#'   (\code{\link{icmp}}, \code{\link{ilgl}}, \code{\link{igen}})
#' @export
cmp_lgl_gen <- function(x) {cmp_gen(x) & lgl_gen(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete logical
#'   tabular?
#'   (\code{\link{icmp}}, \code{\link{ilgl}}, \code{\link{itab}})
#' @export
cmp_lgl_tab <- function(x) {cmp_tab(x) & lgl_tab(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete logical
#'   vlist?
#'   (\code{\link{icmp}}, \code{\link{ilgl}}, \code{\link{ivls}})
#' @export
cmp_lgl_vls <- function(x) {cmp_vls(x) & lgl_vls(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete logical
#'   vtype?
#'   (\code{\link{icmp}}, \code{\link{ilgl}}, \code{\link{ivtp}})
#' @export
cmp_lgl_vtp <- function(x) {cmp_vtp(x) & lgl_vtp(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete logical
#'   matrix?
#'   (\code{\link{icmp}}, \code{\link{ilgl}}, \code{\link{imat}})
#' @export
cmp_lgl_mat <- function(x) {cmp_mat(x) & lgl_mat(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete logical
#'   mvector?
#'   (\code{\link{icmp}}, \code{\link{ilgl}}, \code{\link{imvc}})
#' @export
cmp_lgl_mvc <- function(x) {cmp_mvc(x) & lgl_mvc(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete logical
#'   scalar?
#'   (\code{\link{icmp}}, \code{\link{ilgl}}, \code{\link{iscl}})
#' @export
cmp_lgl_scl <- function(x) {cmp_scl(x) & lgl_scl(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete logical
#'   vector?
#'   (\code{\link{icmp}}, \code{\link{ilgl}}, \code{\link{ivec}})
#' @export
cmp_lgl_vec <- function(x) {cmp_vec(x) & lgl_vec(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete negative-valued
#'   numeric array?
#'   (\code{\link{icmp}}, \code{\link{ineg}}, \code{\link{iarr}})
#' @export
cmp_neg_arr <- function(x) {cmp_arr(x) & neg_arr(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete negative-valued
#'   numeric generic?
#'   (\code{\link{icmp}}, \code{\link{ineg}}, \code{\link{igen}})
#' @export
cmp_neg_gen <- function(x) {cmp_gen(x) & neg_gen(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete negative-valued
#'   numeric tabular?
#'   (\code{\link{icmp}}, \code{\link{ineg}}, \code{\link{itab}})
#' @export
cmp_neg_tab <- function(x) {cmp_tab(x) & neg_tab(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete negative-valued
#'   numeric vlist?
#'   (\code{\link{icmp}}, \code{\link{ineg}}, \code{\link{ivls}})
#' @export
cmp_neg_vls <- function(x) {cmp_vls(x) & neg_vls(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete negative-valued
#'   numeric vtype?
#'   (\code{\link{icmp}}, \code{\link{ineg}}, \code{\link{ivtp}})
#' @export
cmp_neg_vtp <- function(x) {cmp_vtp(x) & neg_vtp(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete negative-valued
#'   numeric matrix?
#'   (\code{\link{icmp}}, \code{\link{ineg}}, \code{\link{imat}})
#' @export
cmp_neg_mat <- function(x) {cmp_mat(x) & neg_mat(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete negative-valued
#'   numeric mvector?
#'   (\code{\link{icmp}}, \code{\link{ineg}}, \code{\link{imvc}})
#' @export
cmp_neg_mvc <- function(x) {cmp_mvc(x) & neg_mvc(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete negative-valued
#'   numeric scalar?
#'   (\code{\link{icmp}}, \code{\link{ineg}}, \code{\link{iscl}})
#' @export
cmp_neg_scl <- function(x) {cmp_scl(x) & neg_scl(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete negative-valued
#'   numeric vector?
#'   (\code{\link{icmp}}, \code{\link{ineg}}, \code{\link{ivec}})
#' @export
cmp_neg_vec <- function(x) {cmp_vec(x) & neg_vec(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete negative-valued
#'   whole-number array?
#'   (\code{\link{icmp}}, \code{\link{ingw}}, \code{\link{iarr}})
#' @export
cmp_ngw_arr <- function(x) {cmp_arr(x) & ngw_arr(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete negative-valued
#'   whole-number generic?
#'   (\code{\link{icmp}}, \code{\link{ingw}}, \code{\link{igen}})
#' @export
cmp_ngw_gen <- function(x) {cmp_gen(x) & ngw_gen(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete negative-valued
#'   whole-number tabular?
#'   (\code{\link{icmp}}, \code{\link{ingw}}, \code{\link{itab}})
#' @export
cmp_ngw_tab <- function(x) {cmp_tab(x) & ngw_tab(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete negative-valued
#'   whole-number vlist?
#'   (\code{\link{icmp}}, \code{\link{ingw}}, \code{\link{ivls}})
#' @export
cmp_ngw_vls <- function(x) {cmp_vls(x) & ngw_vls(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete negative-valued
#'   whole-number vtype?
#'   (\code{\link{icmp}}, \code{\link{ingw}}, \code{\link{ivtp}})
#' @export
cmp_ngw_vtp <- function(x) {cmp_vtp(x) & ngw_vtp(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete negative-valued
#'   whole-number matrix?
#'   (\code{\link{icmp}}, \code{\link{ingw}}, \code{\link{imat}})
#' @export
cmp_ngw_mat <- function(x) {cmp_mat(x) & ngw_mat(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete negative-valued
#'   whole-number mvector?
#'   (\code{\link{icmp}}, \code{\link{ingw}}, \code{\link{imvc}})
#' @export
cmp_ngw_mvc <- function(x) {cmp_mvc(x) & ngw_mvc(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete negative-valued
#'   whole-number scalar?
#'   (\code{\link{icmp}}, \code{\link{ingw}}, \code{\link{iscl}})
#' @export
cmp_ngw_scl <- function(x) {cmp_scl(x) & ngw_scl(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete negative-valued
#'   whole-number vector?
#'   (\code{\link{icmp}}, \code{\link{ingw}}, \code{\link{ivec}})
#' @export
cmp_ngw_vec <- function(x) {cmp_vec(x) & ngw_vec(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-negative-valued
#'   numeric array?
#'   (\code{\link{icmp}}, \code{\link{inng}}, \code{\link{iarr}})
#' @export
cmp_nng_arr <- function(x) {cmp_arr(x) & nng_arr(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-negative-valued
#'   numeric generic?
#'   (\code{\link{icmp}}, \code{\link{inng}}, \code{\link{igen}})
#' @export
cmp_nng_gen <- function(x) {cmp_gen(x) & nng_gen(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-negative-valued
#'   numeric tabular?
#'   (\code{\link{icmp}}, \code{\link{inng}}, \code{\link{itab}})
#' @export
cmp_nng_tab <- function(x) {cmp_tab(x) & nng_tab(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-negative-valued
#'   numeric vlist?
#'   (\code{\link{icmp}}, \code{\link{inng}}, \code{\link{ivls}})
#' @export
cmp_nng_vls <- function(x) {cmp_vls(x) & nng_vls(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-negative-valued
#'   numeric vtype?
#'   (\code{\link{icmp}}, \code{\link{inng}}, \code{\link{ivtp}})
#' @export
cmp_nng_vtp <- function(x) {cmp_vtp(x) & nng_vtp(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-negative-valued
#'   numeric matrix?
#'   (\code{\link{icmp}}, \code{\link{inng}}, \code{\link{imat}})
#' @export
cmp_nng_mat <- function(x) {cmp_mat(x) & nng_mat(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-negative-valued
#'   numeric mvector?
#'   (\code{\link{icmp}}, \code{\link{inng}}, \code{\link{imvc}})
#' @export
cmp_nng_mvc <- function(x) {cmp_mvc(x) & nng_mvc(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-negative-valued
#'   numeric scalar?
#'   (\code{\link{icmp}}, \code{\link{inng}}, \code{\link{iscl}})
#' @export
cmp_nng_scl <- function(x) {cmp_scl(x) & nng_scl(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-negative-valued
#'   numeric vector?
#'   (\code{\link{icmp}}, \code{\link{inng}}, \code{\link{ivec}})
#' @export
cmp_nng_vec <- function(x) {cmp_vec(x) & nng_vec(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-negative-valued
#'   whole-number array?
#'   (\code{\link{icmp}}, \code{\link{innw}}, \code{\link{iarr}})
#' @export
cmp_nnw_arr <- function(x) {cmp_arr(x) & nnw_arr(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-negative-valued
#'   whole-number generic?
#'   (\code{\link{icmp}}, \code{\link{innw}}, \code{\link{igen}})
#' @export
cmp_nnw_gen <- function(x) {cmp_gen(x) & nnw_gen(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-negative-valued
#'   whole-number tabular?
#'   (\code{\link{icmp}}, \code{\link{innw}}, \code{\link{itab}})
#' @export
cmp_nnw_tab <- function(x) {cmp_tab(x) & nnw_tab(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-negative-valued
#'   whole-number vlist?
#'   (\code{\link{icmp}}, \code{\link{innw}}, \code{\link{ivls}})
#' @export
cmp_nnw_vls <- function(x) {cmp_vls(x) & nnw_vls(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-negative-valued
#'   whole-number vtype?
#'   (\code{\link{icmp}}, \code{\link{innw}}, \code{\link{ivtp}})
#' @export
cmp_nnw_vtp <- function(x) {cmp_vtp(x) & nnw_vtp(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-negative-valued
#'   whole-number matrix?
#'   (\code{\link{icmp}}, \code{\link{innw}}, \code{\link{imat}})
#' @export
cmp_nnw_mat <- function(x) {cmp_mat(x) & nnw_mat(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-negative-valued
#'   whole-number mvector?
#'   (\code{\link{icmp}}, \code{\link{innw}}, \code{\link{imvc}})
#' @export
cmp_nnw_mvc <- function(x) {cmp_mvc(x) & nnw_mvc(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-negative-valued
#'   whole-number scalar?
#'   (\code{\link{icmp}}, \code{\link{innw}}, \code{\link{iscl}})
#' @export
cmp_nnw_scl <- function(x) {cmp_scl(x) & nnw_scl(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-negative-valued
#'   whole-number vector?
#'   (\code{\link{icmp}}, \code{\link{innw}}, \code{\link{ivec}})
#' @export
cmp_nnw_vec <- function(x) {cmp_vec(x) & nnw_vec(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-positive-valued
#'   numeric array?
#'   (\code{\link{icmp}}, \code{\link{inps}}, \code{\link{iarr}})
#' @export
cmp_nps_arr <- function(x) {cmp_arr(x) & nps_arr(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-positive-valued
#'   numeric generic?
#'   (\code{\link{icmp}}, \code{\link{inps}}, \code{\link{igen}})
#' @export
cmp_nps_gen <- function(x) {cmp_gen(x) & nps_gen(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-positive-valued
#'   numeric tabular?
#'   (\code{\link{icmp}}, \code{\link{inps}}, \code{\link{itab}})
#' @export
cmp_nps_tab <- function(x) {cmp_tab(x) & nps_tab(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-positive-valued
#'   numeric vlist?
#'   (\code{\link{icmp}}, \code{\link{inps}}, \code{\link{ivls}})
#' @export
cmp_nps_vls <- function(x) {cmp_vls(x) & nps_vls(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-positive-valued
#'   numeric vtype?
#'   (\code{\link{icmp}}, \code{\link{inps}}, \code{\link{ivtp}})
#' @export
cmp_nps_vtp <- function(x) {cmp_vtp(x) & nps_vtp(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-positive-valued
#'   numeric matrix?
#'   (\code{\link{icmp}}, \code{\link{inps}}, \code{\link{imat}})
#' @export
cmp_nps_mat <- function(x) {cmp_mat(x) & nps_mat(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-positive-valued
#'   numeric mvector?
#'   (\code{\link{icmp}}, \code{\link{inps}}, \code{\link{imvc}})
#' @export
cmp_nps_mvc <- function(x) {cmp_mvc(x) & nps_mvc(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-positive-valued
#'   numeric scalar?
#'   (\code{\link{icmp}}, \code{\link{inps}}, \code{\link{iscl}})
#' @export
cmp_nps_scl <- function(x) {cmp_scl(x) & nps_scl(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-positive-valued
#'   numeric vector?
#'   (\code{\link{icmp}}, \code{\link{inps}}, \code{\link{ivec}})
#' @export
cmp_nps_vec <- function(x) {cmp_vec(x) & nps_vec(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-positive-valued
#'   whole-number array?
#'   (\code{\link{icmp}}, \code{\link{inpw}}, \code{\link{iarr}})
#' @export
cmp_npw_arr <- function(x) {cmp_arr(x) & npw_arr(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-positive-valued
#'   whole-number generic?
#'   (\code{\link{icmp}}, \code{\link{inpw}}, \code{\link{igen}})
#' @export
cmp_npw_gen <- function(x) {cmp_gen(x) & npw_gen(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-positive-valued
#'   whole-number tabular?
#'   (\code{\link{icmp}}, \code{\link{inpw}}, \code{\link{itab}})
#' @export
cmp_npw_tab <- function(x) {cmp_tab(x) & npw_tab(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-positive-valued
#'   whole-number vlist?
#'   (\code{\link{icmp}}, \code{\link{inpw}}, \code{\link{ivls}})
#' @export
cmp_npw_vls <- function(x) {cmp_vls(x) & npw_vls(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-positive-valued
#'   whole-number vtype?
#'   (\code{\link{icmp}}, \code{\link{inpw}}, \code{\link{ivtp}})
#' @export
cmp_npw_vtp <- function(x) {cmp_vtp(x) & npw_vtp(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-positive-valued
#'   whole-number matrix?
#'   (\code{\link{icmp}}, \code{\link{inpw}}, \code{\link{imat}})
#' @export
cmp_npw_mat <- function(x) {cmp_mat(x) & npw_mat(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-positive-valued
#'   whole-number mvector?
#'   (\code{\link{icmp}}, \code{\link{inpw}}, \code{\link{imvc}})
#' @export
cmp_npw_mvc <- function(x) {cmp_mvc(x) & npw_mvc(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-positive-valued
#'   whole-number scalar?
#'   (\code{\link{icmp}}, \code{\link{inpw}}, \code{\link{iscl}})
#' @export
cmp_npw_scl <- function(x) {cmp_scl(x) & npw_scl(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-positive-valued
#'   whole-number vector?
#'   (\code{\link{icmp}}, \code{\link{inpw}}, \code{\link{ivec}})
#' @export
cmp_npw_vec <- function(x) {cmp_vec(x) & npw_vec(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-sortable
#'   array?
#'   (\code{\link{icmp}}, \code{\link{inst}}, \code{\link{iarr}})
#' @export
cmp_nst_arr <- function(x) {cmp_arr(x) & nst_arr(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-sortable
#'   generic?
#'   (\code{\link{icmp}}, \code{\link{inst}}, \code{\link{igen}})
#' @export
cmp_nst_gen <- function(x) {cmp_gen(x) & nst_gen(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-sortable
#'   tabular?
#'   (\code{\link{icmp}}, \code{\link{inst}}, \code{\link{itab}})
#' @export
cmp_nst_tab <- function(x) {cmp_tab(x) & nst_tab(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-sortable
#'   vlist?
#'   (\code{\link{icmp}}, \code{\link{inst}}, \code{\link{ivls}})
#' @export
cmp_nst_vls <- function(x) {cmp_vls(x) & nst_vls(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-sortable
#'   vtype?
#'   (\code{\link{icmp}}, \code{\link{inst}}, \code{\link{ivtp}})
#' @export
cmp_nst_vtp <- function(x) {cmp_vtp(x) & nst_vtp(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-sortable
#'   matrix?
#'   (\code{\link{icmp}}, \code{\link{inst}}, \code{\link{imat}})
#' @export
cmp_nst_mat <- function(x) {cmp_mat(x) & nst_mat(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-sortable
#'   mvector?
#'   (\code{\link{icmp}}, \code{\link{inst}}, \code{\link{imvc}})
#' @export
cmp_nst_mvc <- function(x) {cmp_mvc(x) & nst_mvc(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-sortable
#'   scalar?
#'   (\code{\link{icmp}}, \code{\link{inst}}, \code{\link{iscl}})
#' @export
cmp_nst_scl <- function(x) {cmp_scl(x) & nst_scl(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete non-sortable
#'   vector?
#'   (\code{\link{icmp}}, \code{\link{inst}}, \code{\link{ivec}})
#' @export
cmp_nst_vec <- function(x) {cmp_vec(x) & nst_vec(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete numeric
#'   array?
#'   (\code{\link{icmp}}, \code{\link{inum}}, \code{\link{iarr}})
#' @export
cmp_num_arr <- function(x) {cmp_arr(x) & num_arr(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete numeric
#'   generic?
#'   (\code{\link{icmp}}, \code{\link{inum}}, \code{\link{igen}})
#' @export
cmp_num_gen <- function(x) {cmp_gen(x) & num_gen(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete numeric
#'   tabular?
#'   (\code{\link{icmp}}, \code{\link{inum}}, \code{\link{itab}})
#' @export
cmp_num_tab <- function(x) {cmp_tab(x) & num_tab(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete numeric
#'   vlist?
#'   (\code{\link{icmp}}, \code{\link{inum}}, \code{\link{ivls}})
#' @export
cmp_num_vls <- function(x) {cmp_vls(x) & num_vls(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete numeric
#'   vtype?
#'   (\code{\link{icmp}}, \code{\link{inum}}, \code{\link{ivtp}})
#' @export
cmp_num_vtp <- function(x) {cmp_vtp(x) & num_vtp(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete numeric
#'   matrix?
#'   (\code{\link{icmp}}, \code{\link{inum}}, \code{\link{imat}})
#' @export
cmp_num_mat <- function(x) {cmp_mat(x) & num_mat(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete numeric
#'   mvector?
#'   (\code{\link{icmp}}, \code{\link{inum}}, \code{\link{imvc}})
#' @export
cmp_num_mvc <- function(x) {cmp_mvc(x) & num_mvc(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete numeric
#'   scalar?
#'   (\code{\link{icmp}}, \code{\link{inum}}, \code{\link{iscl}})
#' @export
cmp_num_scl <- function(x) {cmp_scl(x) & num_scl(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete numeric
#'   vector?
#'   (\code{\link{icmp}}, \code{\link{inum}}, \code{\link{ivec}})
#' @export
cmp_num_vec <- function(x) {cmp_vec(x) & num_vec(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete odd-valued
#'   whole-number array?
#'   (\code{\link{icmp}}, \code{\link{iodd}}, \code{\link{iarr}})
#' @export
cmp_odd_arr <- function(x) {cmp_arr(x) & odd_arr(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete odd-valued
#'   whole-number generic?
#'   (\code{\link{icmp}}, \code{\link{iodd}}, \code{\link{igen}})
#' @export
cmp_odd_gen <- function(x) {cmp_gen(x) & odd_gen(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete odd-valued
#'   whole-number tabular?
#'   (\code{\link{icmp}}, \code{\link{iodd}}, \code{\link{itab}})
#' @export
cmp_odd_tab <- function(x) {cmp_tab(x) & odd_tab(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete odd-valued
#'   whole-number vlist?
#'   (\code{\link{icmp}}, \code{\link{iodd}}, \code{\link{ivls}})
#' @export
cmp_odd_vls <- function(x) {cmp_vls(x) & odd_vls(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete odd-valued
#'   whole-number vtype?
#'   (\code{\link{icmp}}, \code{\link{iodd}}, \code{\link{ivtp}})
#' @export
cmp_odd_vtp <- function(x) {cmp_vtp(x) & odd_vtp(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete odd-valued
#'   whole-number matrix?
#'   (\code{\link{icmp}}, \code{\link{iodd}}, \code{\link{imat}})
#' @export
cmp_odd_mat <- function(x) {cmp_mat(x) & odd_mat(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete odd-valued
#'   whole-number mvector?
#'   (\code{\link{icmp}}, \code{\link{iodd}}, \code{\link{imvc}})
#' @export
cmp_odd_mvc <- function(x) {cmp_mvc(x) & odd_mvc(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete odd-valued
#'   whole-number scalar?
#'   (\code{\link{icmp}}, \code{\link{iodd}}, \code{\link{iscl}})
#' @export
cmp_odd_scl <- function(x) {cmp_scl(x) & odd_scl(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete odd-valued
#'   whole-number vector?
#'   (\code{\link{icmp}}, \code{\link{iodd}}, \code{\link{ivec}})
#' @export
cmp_odd_vec <- function(x) {cmp_vec(x) & odd_vec(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete ordered-factor
#'   array?
#'   (\code{\link{icmp}}, \code{\link{iord}}, \code{\link{iarr}})
#' @export
cmp_ord_arr <- function(x) {cmp_arr(x) & ord_arr(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete ordered-factor
#'   generic?
#'   (\code{\link{icmp}}, \code{\link{iord}}, \code{\link{igen}})
#' @export
cmp_ord_gen <- function(x) {cmp_gen(x) & ord_gen(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete ordered-factor
#'   tabular?
#'   (\code{\link{icmp}}, \code{\link{iord}}, \code{\link{itab}})
#' @export
cmp_ord_tab <- function(x) {cmp_tab(x) & ord_tab(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete ordered-factor
#'   vlist?
#'   (\code{\link{icmp}}, \code{\link{iord}}, \code{\link{ivls}})
#' @export
cmp_ord_vls <- function(x) {cmp_vls(x) & ord_vls(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete ordered-factor
#'   vtype?
#'   (\code{\link{icmp}}, \code{\link{iord}}, \code{\link{ivtp}})
#' @export
cmp_ord_vtp <- function(x) {cmp_vtp(x) & ord_vtp(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete ordered-factor
#'   matrix?
#'   (\code{\link{icmp}}, \code{\link{iord}}, \code{\link{imat}})
#' @export
cmp_ord_mat <- function(x) {cmp_mat(x) & ord_mat(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete ordered-factor
#'   mvector?
#'   (\code{\link{icmp}}, \code{\link{iord}}, \code{\link{imvc}})
#' @export
cmp_ord_mvc <- function(x) {cmp_mvc(x) & ord_mvc(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete ordered-factor
#'   scalar?
#'   (\code{\link{icmp}}, \code{\link{iord}}, \code{\link{iscl}})
#' @export
cmp_ord_scl <- function(x) {cmp_scl(x) & ord_scl(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete ordered-factor
#'   vector?
#'   (\code{\link{icmp}}, \code{\link{iord}}, \code{\link{ivec}})
#' @export
cmp_ord_vec <- function(x) {cmp_vec(x) & ord_vec(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete percent-valued
#'   (0-100) numeric array?
#'   (\code{\link{icmp}}, \code{\link{ipct}}, \code{\link{iarr}})
#' @export
cmp_pct_arr <- function(x) {cmp_arr(x) & pct_arr(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete percent-valued
#'   (0-100) numeric generic?
#'   (\code{\link{icmp}}, \code{\link{ipct}}, \code{\link{igen}})
#' @export
cmp_pct_gen <- function(x) {cmp_gen(x) & pct_gen(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete percent-valued
#'   (0-100) numeric tabular?
#'   (\code{\link{icmp}}, \code{\link{ipct}}, \code{\link{itab}})
#' @export
cmp_pct_tab <- function(x) {cmp_tab(x) & pct_tab(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete percent-valued
#'   (0-100) numeric vlist?
#'   (\code{\link{icmp}}, \code{\link{ipct}}, \code{\link{ivls}})
#' @export
cmp_pct_vls <- function(x) {cmp_vls(x) & pct_vls(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete percent-valued
#'   (0-100) numeric vtype?
#'   (\code{\link{icmp}}, \code{\link{ipct}}, \code{\link{ivtp}})
#' @export
cmp_pct_vtp <- function(x) {cmp_vtp(x) & pct_vtp(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete percent-valued
#'   (0-100) numeric matrix?
#'   (\code{\link{icmp}}, \code{\link{ipct}}, \code{\link{imat}})
#' @export
cmp_pct_mat <- function(x) {cmp_mat(x) & pct_mat(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete percent-valued
#'   (0-100) numeric mvector?
#'   (\code{\link{icmp}}, \code{\link{ipct}}, \code{\link{imvc}})
#' @export
cmp_pct_mvc <- function(x) {cmp_mvc(x) & pct_mvc(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete percent-valued
#'   (0-100) numeric scalar?
#'   (\code{\link{icmp}}, \code{\link{ipct}}, \code{\link{iscl}})
#' @export
cmp_pct_scl <- function(x) {cmp_scl(x) & pct_scl(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete percent-valued
#'   (0-100) numeric vector?
#'   (\code{\link{icmp}}, \code{\link{ipct}}, \code{\link{ivec}})
#' @export
cmp_pct_vec <- function(x) {cmp_vec(x) & pct_vec(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete positive-valued
#'   numeric array?
#'   (\code{\link{icmp}}, \code{\link{ipos}}, \code{\link{iarr}})
#' @export
cmp_pos_arr <- function(x) {cmp_arr(x) & pos_arr(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete positive-valued
#'   numeric generic?
#'   (\code{\link{icmp}}, \code{\link{ipos}}, \code{\link{igen}})
#' @export
cmp_pos_gen <- function(x) {cmp_gen(x) & pos_gen(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete positive-valued
#'   numeric tabular?
#'   (\code{\link{icmp}}, \code{\link{ipos}}, \code{\link{itab}})
#' @export
cmp_pos_tab <- function(x) {cmp_tab(x) & pos_tab(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete positive-valued
#'   numeric vlist?
#'   (\code{\link{icmp}}, \code{\link{ipos}}, \code{\link{ivls}})
#' @export
cmp_pos_vls <- function(x) {cmp_vls(x) & pos_vls(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete positive-valued
#'   numeric vtype?
#'   (\code{\link{icmp}}, \code{\link{ipos}}, \code{\link{ivtp}})
#' @export
cmp_pos_vtp <- function(x) {cmp_vtp(x) & pos_vtp(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete positive-valued
#'   numeric matrix?
#'   (\code{\link{icmp}}, \code{\link{ipos}}, \code{\link{imat}})
#' @export
cmp_pos_mat <- function(x) {cmp_mat(x) & pos_mat(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete positive-valued
#'   numeric mvector?
#'   (\code{\link{icmp}}, \code{\link{ipos}}, \code{\link{imvc}})
#' @export
cmp_pos_mvc <- function(x) {cmp_mvc(x) & pos_mvc(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete positive-valued
#'   numeric scalar?
#'   (\code{\link{icmp}}, \code{\link{ipos}}, \code{\link{iscl}})
#' @export
cmp_pos_scl <- function(x) {cmp_scl(x) & pos_scl(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete positive-valued
#'   numeric vector?
#'   (\code{\link{icmp}}, \code{\link{ipos}}, \code{\link{ivec}})
#' @export
cmp_pos_vec <- function(x) {cmp_vec(x) & pos_vec(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete proportion-valued
#'   (0-1) numeric array?
#'   (\code{\link{icmp}}, \code{\link{ippn}}, \code{\link{iarr}})
#' @export
cmp_ppn_arr <- function(x) {cmp_arr(x) & ppn_arr(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete proportion-valued
#'   (0-1) numeric generic?
#'   (\code{\link{icmp}}, \code{\link{ippn}}, \code{\link{igen}})
#' @export
cmp_ppn_gen <- function(x) {cmp_gen(x) & ppn_gen(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete proportion-valued
#'   (0-1) numeric tabular?
#'   (\code{\link{icmp}}, \code{\link{ippn}}, \code{\link{itab}})
#' @export
cmp_ppn_tab <- function(x) {cmp_tab(x) & ppn_tab(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete proportion-valued
#'   (0-1) numeric vlist?
#'   (\code{\link{icmp}}, \code{\link{ippn}}, \code{\link{ivls}})
#' @export
cmp_ppn_vls <- function(x) {cmp_vls(x) & ppn_vls(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete proportion-valued
#'   (0-1) numeric vtype?
#'   (\code{\link{icmp}}, \code{\link{ippn}}, \code{\link{ivtp}})
#' @export
cmp_ppn_vtp <- function(x) {cmp_vtp(x) & ppn_vtp(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete proportion-valued
#'   (0-1) numeric matrix?
#'   (\code{\link{icmp}}, \code{\link{ippn}}, \code{\link{imat}})
#' @export
cmp_ppn_mat <- function(x) {cmp_mat(x) & ppn_mat(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete proportion-valued
#'   (0-1) numeric mvector?
#'   (\code{\link{icmp}}, \code{\link{ippn}}, \code{\link{imvc}})
#' @export
cmp_ppn_mvc <- function(x) {cmp_mvc(x) & ppn_mvc(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete proportion-valued
#'   (0-1) numeric scalar?
#'   (\code{\link{icmp}}, \code{\link{ippn}}, \code{\link{iscl}})
#' @export
cmp_ppn_scl <- function(x) {cmp_scl(x) & ppn_scl(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete proportion-valued
#'   (0-1) numeric vector?
#'   (\code{\link{icmp}}, \code{\link{ippn}}, \code{\link{ivec}})
#' @export
cmp_ppn_vec <- function(x) {cmp_vec(x) & ppn_vec(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete positive-valued
#'   whole-number array?
#'   (\code{\link{icmp}}, \code{\link{ipsw}}, \code{\link{iarr}})
#' @export
cmp_psw_arr <- function(x) {cmp_arr(x) & psw_arr(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete positive-valued
#'   whole-number generic?
#'   (\code{\link{icmp}}, \code{\link{ipsw}}, \code{\link{igen}})
#' @export
cmp_psw_gen <- function(x) {cmp_gen(x) & psw_gen(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete positive-valued
#'   whole-number tabular?
#'   (\code{\link{icmp}}, \code{\link{ipsw}}, \code{\link{itab}})
#' @export
cmp_psw_tab <- function(x) {cmp_tab(x) & psw_tab(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete positive-valued
#'   whole-number vlist?
#'   (\code{\link{icmp}}, \code{\link{ipsw}}, \code{\link{ivls}})
#' @export
cmp_psw_vls <- function(x) {cmp_vls(x) & psw_vls(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete positive-valued
#'   whole-number vtype?
#'   (\code{\link{icmp}}, \code{\link{ipsw}}, \code{\link{ivtp}})
#' @export
cmp_psw_vtp <- function(x) {cmp_vtp(x) & psw_vtp(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete positive-valued
#'   whole-number matrix?
#'   (\code{\link{icmp}}, \code{\link{ipsw}}, \code{\link{imat}})
#' @export
cmp_psw_mat <- function(x) {cmp_mat(x) & psw_mat(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete positive-valued
#'   whole-number mvector?
#'   (\code{\link{icmp}}, \code{\link{ipsw}}, \code{\link{imvc}})
#' @export
cmp_psw_mvc <- function(x) {cmp_mvc(x) & psw_mvc(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete positive-valued
#'   whole-number scalar?
#'   (\code{\link{icmp}}, \code{\link{ipsw}}, \code{\link{iscl}})
#' @export
cmp_psw_scl <- function(x) {cmp_scl(x) & psw_scl(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete positive-valued
#'   whole-number vector?
#'   (\code{\link{icmp}}, \code{\link{ipsw}}, \code{\link{ivec}})
#' @export
cmp_psw_vec <- function(x) {cmp_vec(x) & psw_vec(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete sortable
#'   array?
#'   (\code{\link{icmp}}, \code{\link{isrt}}, \code{\link{iarr}})
#' @export
cmp_srt_arr <- function(x) {cmp_arr(x) & srt_arr(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete sortable
#'   generic?
#'   (\code{\link{icmp}}, \code{\link{isrt}}, \code{\link{igen}})
#' @export
cmp_srt_gen <- function(x) {cmp_gen(x) & srt_gen(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete sortable
#'   tabular?
#'   (\code{\link{icmp}}, \code{\link{isrt}}, \code{\link{itab}})
#' @export
cmp_srt_tab <- function(x) {cmp_tab(x) & srt_tab(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete sortable
#'   vlist?
#'   (\code{\link{icmp}}, \code{\link{isrt}}, \code{\link{ivls}})
#' @export
cmp_srt_vls <- function(x) {cmp_vls(x) & srt_vls(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete sortable
#'   vtype?
#'   (\code{\link{icmp}}, \code{\link{isrt}}, \code{\link{ivtp}})
#' @export
cmp_srt_vtp <- function(x) {cmp_vtp(x) & srt_vtp(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete sortable
#'   matrix?
#'   (\code{\link{icmp}}, \code{\link{isrt}}, \code{\link{imat}})
#' @export
cmp_srt_mat <- function(x) {cmp_mat(x) & srt_mat(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete sortable
#'   mvector?
#'   (\code{\link{icmp}}, \code{\link{isrt}}, \code{\link{imvc}})
#' @export
cmp_srt_mvc <- function(x) {cmp_mvc(x) & srt_mvc(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete sortable
#'   scalar?
#'   (\code{\link{icmp}}, \code{\link{isrt}}, \code{\link{iscl}})
#' @export
cmp_srt_scl <- function(x) {cmp_scl(x) & srt_scl(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete sortable
#'   vector?
#'   (\code{\link{icmp}}, \code{\link{isrt}}, \code{\link{ivec}})
#' @export
cmp_srt_vec <- function(x) {cmp_vec(x) & srt_vec(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete string
#'   array?
#'   (\code{\link{icmp}}, \code{\link{istr}}, \code{\link{iarr}})
#' @export
cmp_str_arr <- function(x) {cmp_arr(x) & str_arr(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete string
#'   generic?
#'   (\code{\link{icmp}}, \code{\link{istr}}, \code{\link{igen}})
#' @export
cmp_str_gen <- function(x) {cmp_gen(x) & str_gen(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete string
#'   tabular?
#'   (\code{\link{icmp}}, \code{\link{istr}}, \code{\link{itab}})
#' @export
cmp_str_tab <- function(x) {cmp_tab(x) & str_tab(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete string
#'   vlist?
#'   (\code{\link{icmp}}, \code{\link{istr}}, \code{\link{ivls}})
#' @export
cmp_str_vls <- function(x) {cmp_vls(x) & str_vls(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete string
#'   vtype?
#'   (\code{\link{icmp}}, \code{\link{istr}}, \code{\link{ivtp}})
#' @export
cmp_str_vtp <- function(x) {cmp_vtp(x) & str_vtp(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete string
#'   matrix?
#'   (\code{\link{icmp}}, \code{\link{istr}}, \code{\link{imat}})
#' @export
cmp_str_mat <- function(x) {cmp_mat(x) & str_mat(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete string
#'   mvector?
#'   (\code{\link{icmp}}, \code{\link{istr}}, \code{\link{imvc}})
#' @export
cmp_str_mvc <- function(x) {cmp_mvc(x) & str_mvc(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete string
#'   scalar?
#'   (\code{\link{icmp}}, \code{\link{istr}}, \code{\link{iscl}})
#' @export
cmp_str_scl <- function(x) {cmp_scl(x) & str_scl(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete string
#'   vector?
#'   (\code{\link{icmp}}, \code{\link{istr}}, \code{\link{ivec}})
#' @export
cmp_str_vec <- function(x) {cmp_vec(x) & str_vec(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete unordered-factor
#'   array?
#'   (\code{\link{icmp}}, \code{\link{iuno}}, \code{\link{iarr}})
#' @export
cmp_uno_arr <- function(x) {cmp_arr(x) & uno_arr(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete unordered-factor
#'   generic?
#'   (\code{\link{icmp}}, \code{\link{iuno}}, \code{\link{igen}})
#' @export
cmp_uno_gen <- function(x) {cmp_gen(x) & uno_gen(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete unordered-factor
#'   tabular?
#'   (\code{\link{icmp}}, \code{\link{iuno}}, \code{\link{itab}})
#' @export
cmp_uno_tab <- function(x) {cmp_tab(x) & uno_tab(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete unordered-factor
#'   vlist?
#'   (\code{\link{icmp}}, \code{\link{iuno}}, \code{\link{ivls}})
#' @export
cmp_uno_vls <- function(x) {cmp_vls(x) & uno_vls(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete unordered-factor
#'   vtype?
#'   (\code{\link{icmp}}, \code{\link{iuno}}, \code{\link{ivtp}})
#' @export
cmp_uno_vtp <- function(x) {cmp_vtp(x) & uno_vtp(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete unordered-factor
#'   matrix?
#'   (\code{\link{icmp}}, \code{\link{iuno}}, \code{\link{imat}})
#' @export
cmp_uno_mat <- function(x) {cmp_mat(x) & uno_mat(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete unordered-factor
#'   mvector?
#'   (\code{\link{icmp}}, \code{\link{iuno}}, \code{\link{imvc}})
#' @export
cmp_uno_mvc <- function(x) {cmp_mvc(x) & uno_mvc(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete unordered-factor
#'   scalar?
#'   (\code{\link{icmp}}, \code{\link{iuno}}, \code{\link{iscl}})
#' @export
cmp_uno_scl <- function(x) {cmp_scl(x) & uno_scl(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete unordered-factor
#'   vector?
#'   (\code{\link{icmp}}, \code{\link{iuno}}, \code{\link{ivec}})
#' @export
cmp_uno_vec <- function(x) {cmp_vec(x) & uno_vec(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete whole-number array?
#'   (\code{\link{icmp}}, \code{\link{iwhl}}, \code{\link{iarr}})
#' @export
cmp_whl_arr <- function(x) {cmp_arr(x) & whl_arr(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete whole-number generic?
#'   (\code{\link{icmp}}, \code{\link{iwhl}}, \code{\link{igen}})
#' @export
cmp_whl_gen <- function(x) {cmp_gen(x) & whl_gen(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete whole-number tabular?
#'   (\code{\link{icmp}}, \code{\link{iwhl}}, \code{\link{itab}})
#' @export
cmp_whl_tab <- function(x) {cmp_tab(x) & whl_tab(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete whole-number vlist?
#'   (\code{\link{icmp}}, \code{\link{iwhl}}, \code{\link{ivls}})
#' @export
cmp_whl_vls <- function(x) {cmp_vls(x) & whl_vls(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete whole-number vtype?
#'   (\code{\link{icmp}}, \code{\link{iwhl}}, \code{\link{ivtp}})
#' @export
cmp_whl_vtp <- function(x) {cmp_vtp(x) & whl_vtp(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete whole-number matrix?
#'   (\code{\link{icmp}}, \code{\link{iwhl}}, \code{\link{imat}})
#' @export
cmp_whl_mat <- function(x) {cmp_mat(x) & whl_mat(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete whole-number mvector?
#'   (\code{\link{icmp}}, \code{\link{iwhl}}, \code{\link{imvc}})
#' @export
cmp_whl_mvc <- function(x) {cmp_mvc(x) & whl_mvc(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete whole-number scalar?
#'   (\code{\link{icmp}}, \code{\link{iwhl}}, \code{\link{iscl}})
#' @export
cmp_whl_scl <- function(x) {cmp_scl(x) & whl_scl(x)}

#' @describeIn cmp_mmm_ccc_uj Is \code{x} a complete whole-number vector?
#'   (\code{\link{icmp}}, \code{\link{iwhl}}, \code{\link{ivec}})
#' @export
cmp_whl_vec <- function(x) {cmp_vec(x) & whl_vec(x)}
