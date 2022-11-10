#' @name mmm_ccc.
#' @family props
#' @title Extended mode plus extended class properties
#' @description Combinations of \link[mmm]{extended mode} and
#'   \link[ccc]{extended class}.
#' @param x An ℝ object.
#' @return A logical scalar.
#' @export
mmm_ccc. <- function() {help("mmm_ccc.", package = "uj")}

#' @describeIn mmm_ccc. Get a character vector of all possible extended mode
#'   plus extended class properties.
#' @export
mmm_ccc_vals <- function() {
  x <- expand.grid(mmm = mmm_vals(), ccc = ccc_vals())
  x <- av(apply(x, 1, paste0, collapse = "_"))
  sort(x)
}

#' @describeIn mmm_ccc. Is \code{x} a \link[ich1]{onechar} \link[iarr]{array}?
#' @export
ch1_arr <- function(x) {atm_arr(x, 'ch1')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ich1]{onechar} \link[igen]{generic}?
#' @export
ch1_gen <- function(x) {atm_gen(x, 'ch1')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ich1]{onechar} \link[idtf]{dtf}?
#' @export
ch1_dtf <- function(x) {atm_dtf(x, 'ch1')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ich1]{onechar} \link[ivls]{vlist}?
#' @export
ch1_vls <- function(x) {atm_vls(x, 'ch1')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ich1]{onechar} \link[ivtp]{vtype}?
#' @export
ch1_vtp <- function(x) {atm_vtp(x, 'ch1')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ich1]{onechar} \link[imat]{matrix}?
#' @export
ch1_mat <- function(x) {atm_mat(x, 'ch1')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ich1]{onechar}
#'   \link[imvc]{multivec}?
#' @export
ch1_mvc <- function(x) {atm_mvc(x, 'ch1')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ich1]{onechar} \link[iscl]{scalar}?
#' @export
ch1_scl <- function(x) {atm_scl(x, 'ch1')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ich1]{onechar} \link[ivec]{vec}?
#' @export
ch1_vec <- function(x) {atm_vec(x, 'ch1')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ichr]{character} \link[iarr]{array}?
#' @export
chr_arr <- function(x) {atm_arr(x, 'chr')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ichr]{character}
#'   \link[igen]{generic}?
#' @export
chr_gen <- function(x) {atm_gen(x, 'chr')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ichr]{character} \link[idtf]{dtf}?
#' @export
chr_dtf <- function(x) {atm_dtf(x, 'chr')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ichr]{character}
#'   \link[ivls]{vlist}?
#' @export
chr_vls <- function(x) {atm_vls(x, 'chr')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ichr]{character} \link[ivtp]{vtype}?
#' @export
chr_vtp <- function(x) {atm_vtp(x, 'chr')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ichr]{character}
#'   \link[imat]{matrix}?
#' @export
chr_mat <- function(x) {atm_mat(x, 'chr')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ichr]{character}
#'   \link[imvc]{multivec}?
#' @export
chr_mvc <- function(x) {atm_mvc(x, 'chr')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ichr]{character}
#'   \link[iscl]{scalar}?
#' @export
chr_scl <- function(x) {atm_scl(x, 'chr')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ichr]{character} \link[ivec]{vec}?
#' @export
chr_vec <- function(x) {atm_vec(x, 'chr')}

#' @describeIn mmm_ccc. Is \code{x} a \link[iclr]{color} \link[iarr]{array}?
#' @export
clr_arr <- function(x) {atm_arr(x, 'clr')}

#' @describeIn mmm_ccc. Is \code{x} a \link[iclr]{color} \link[igen]{generic}?
#' @export
clr_gen <- function(x) {atm_gen(x, 'clr')}

#' @describeIn mmm_ccc. Is \code{x} a \link[iclr]{color} \link[idtf]{dtf}?
#' @export
clr_dtf <- function(x) {atm_dtf(x, 'clr')}

#' @describeIn mmm_ccc. Is \code{x} a \link[iclr]{color} \link[ivls]{vlist}?
#' @export
clr_vls <- function(x) {atm_vls(x, 'clr')}

#' @describeIn mmm_ccc. Is \code{x} a \link[iclr]{color} \link[ivtp]{vtype}?
#' @export
clr_vtp <- function(x) {atm_vtp(x, 'clr')}

#' @describeIn mmm_ccc. Is \code{x} a \link[iclr]{color} \link[imat]{matrix}?
#' @export
clr_mat <- function(x) {atm_mat(x, 'clr')}

#' @describeIn mmm_ccc. Is \code{x} a \link[iclr]{color} \link[imvc]{multivec}?
#' @export
clr_mvc <- function(x) {atm_mvc(x, 'clr')}

#' @describeIn mmm_ccc. Is \code{x} a \link[iclr]{color} \link[iscl]{scalar}?
#' @export
clr_scl <- function(x) {atm_scl(x, 'clr')}

#' @describeIn mmm_ccc. Is \code{x} a \link[iclr]{color} \link[ivec]{vec}?
#' @export
clr_vec <- function(x) {atm_vec(x, 'clr')}

#' @describeIn mmm_ccc. Is \code{x} an \link[ievn]{even-valued whole-number}
#'   \link[iarr]{array}?
#' @export
evn_arr <- function(x) {atm_arr(x, 'evn')}

#' @describeIn mmm_ccc. Is \code{x} an \link[ievn]{even-valued whole-number}
#'   \link[igen]{generic}?
#' @export
evn_gen <- function(x) {atm_gen(x, 'evn')}

#' @describeIn mmm_ccc. Is \code{x} an \link[ievn]{even-valued whole-number}
#'   \link[idtf]{dtf}?
#' @export
evn_dtf <- function(x) {atm_dtf(x, 'evn')}

#' @describeIn mmm_ccc. Is \code{x} an \link[ievn]{even-valued whole-number}
#'   \link[ivls]{vlist}?
#' @export
evn_vls <- function(x) {atm_vls(x, 'evn')}

#' @describeIn mmm_ccc. Is \code{x} an \link[ievn]{even-valued whole-number}
#'   \link[ivtp]{vtype}?
#' @export
evn_vtp <- function(x) {atm_vtp(x, 'evn')}

#' @describeIn mmm_ccc. Is \code{x} an \link[ievn]{even-valued whole-number}
#'   \link[imat]{matrix}?
#' @export
evn_mat <- function(x) {atm_mat(x, 'evn')}

#' @describeIn mmm_ccc. Is \code{x} an \link[ievn]{even-valued whole-number}
#'   \link[imvc]{multivec}?
#' @export
evn_mvc <- function(x) {atm_mvc(x, 'evn')}

#' @describeIn mmm_ccc. Is \code{x} an \link[ievn]{even-valued whole-number}
#'   \link[iscl]{scalar}?
#' @export
evn_scl <- function(x) {atm_scl(x, 'evn')}

#' @describeIn mmm_ccc. Is \code{x} an \link[ievn]{even-valued whole-number}
#'   \link[ivec]{vec}?
#' @export
evn_vec <- function(x) {atm_vec(x, 'evn')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ifac]{factor} \link[iarr]{array}?
#' @export
fac_arr <- function(x) {atm_arr(x, 'fac')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ifac]{factor} \link[igen]{generic}?
#' @export
fac_gen <- function(x) {atm_gen(x, 'fac')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ifac]{factor} \link[idtf]{dtf}?
#' @export
fac_dtf <- function(x) {atm_dtf(x, 'fac')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ifac]{factor} \link[ivls]{vlist}?
#' @export
fac_vls <- function(x) {atm_vls(x, 'fac')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ifac]{factor} \link[ivtp]{vtype}?
#' @export
fac_vtp <- function(x) {atm_vtp(x, 'fac')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ifac]{factor} \link[imat]{matrix}?
#' @export
fac_mat <- function(x) {atm_mat(x, 'fac')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ifac]{factor} \link[imvc]{multivec}?
#' @export
fac_mvc <- function(x) {atm_mvc(x, 'fac')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ifac]{factor} \link[iscl]{scalar}?
#' @export
fac_scl <- function(x) {atm_scl(x, 'fac')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ifac]{factor} \link[ivec]{vec}?
#' @export
fac_vec <- function(x) {atm_vec(x, 'fac')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ifrc]{fractional-valued numeric}
#'   \link[iarr]{array}?
#' @export
frc_arr <- function(x) {atm_arr(x, 'frc')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ifrc]{fractional-valued numeric}
#'   \link[igen]{generic}?
#' @export
frc_gen <- function(x) {atm_gen(x, 'frc')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ifrc]{fractional-valued numeric}
#'   \link[idtf]{dtf}?
#' @export
frc_dtf <- function(x) {atm_dtf(x, 'frc')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ifrc]{fractional-valued numeric}
#'   \link[ivls]{vlist}?
#' @export
frc_vls <- function(x) {atm_vls(x, 'frc')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ifrc]{fractional-valued numeric}
#'   \link[ivtp]{vtype}?
#' @export
frc_vtp <- function(x) {atm_vtp(x, 'frc')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ifrc]{fractional-valued numeric}
#'   \link[imat]{matrix}?
#' @export
frc_mat <- function(x) {atm_mat(x, 'frc')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ifrc]{fractional-valued numeric}
#'   \link[imvc]{multivec}?
#' @export
frc_mvc <- function(x) {atm_mvc(x, 'frc')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ifrc]{fractional-valued numeric}
#'   \link[iscl]{scalar}?
#' @export
frc_scl <- function(x) {atm_scl(x, 'frc')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ifrc]{fractional-valued numeric}
#'   \link[ivec]{vec}?
#' @export
frc_vec <- function(x) {atm_vec(x, 'frc')}

#' @describeIn mmm_ccc. Is \code{x} an \link[iind]{indexer} \link[iarr]{array}?
#' @export
ind_arr <- function(x) {atm_arr(x, 'ind')}

#' @describeIn mmm_ccc. Is \code{x} an \link[iind]{indexer}
#'   \link[igen]{generic}?
#' @export
ind_gen <- function(x) {atm_gen(x, 'ind')}

#' @describeIn mmm_ccc. Is \code{x} an \link[iind]{indexer} \link[idtf]{dtf}?
#' @export
ind_dtf <- function(x) {atm_dtf(x, 'ind')}

#' @describeIn mmm_ccc. Is \code{x} an \link[iind]{indexer} \link[ivls]{vlist}?
#' @export
ind_vls <- function(x) {atm_vls(x, 'ind')}

#' @describeIn mmm_ccc. Is \code{x} an \link[iind]{indexer} \link[ivtp]{vtype}?
#' @export
ind_vtp <- function(x) {atm_vtp(x, 'ind')}

#' @describeIn mmm_ccc. Is \code{x} an \link[iind]{indexer} \link[imat]{matrix}?
#' @export
ind_mat <- function(x) {atm_mat(x, 'ind')}

#' @describeIn mmm_ccc. Is \code{x} an \link[iind]{indexer}
#'   \link[imvc]{multivec}?
#' @export
ind_mvc <- function(x) {atm_mvc(x, 'ind')}

#' @describeIn mmm_ccc. Is \code{x} an \link[iind]{indexer}
#'   \link[iscl]{scalar}?
#' @export
ind_scl <- function(x) {atm_scl(x, 'ind')}

#' @describeIn mmm_ccc. Is \code{x} an \link[iind]{indexer} \link[ivec]{vec}?
#' @export
ind_vec <- function(x) {atm_vec(x, 'ind')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ilgl]{logical} \link[iarr]{array}?
#' @export
lgl_arr <- function(x) {atm_arr(x, 'lgl')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ilgl]{logical} \link[igen]{generic}?
#' @export
lgl_gen <- function(x) {atm_gen(x, 'lgl')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ilgl]{logical} \link[idtf]{dtf}?
#' @export
lgl_dtf <- function(x) {atm_dtf(x, 'lgl')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ilgl]{logical} \link[ivls]{vlist}?
#' @export
lgl_vls <- function(x) {atm_vls(x, 'lgl')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ilgl]{logical} \link[ivtp]{vtype}?
#' @export
lgl_vtp <- function(x) {atm_vtp(x, 'lgl')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ilgl]{logical} \link[imat]{matrix}?
#' @export
lgl_mat <- function(x) {atm_mat(x, 'lgl')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ilgl]{logical}
#'   \link[imvc]{multivec}?
#' @export
lgl_mvc <- function(x) {atm_mvc(x, 'lgl')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ilgl]{logical} \link[iscl]{scalar}?
#' @export
lgl_scl <- function(x) {atm_scl(x, 'lgl')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ilgl]{logical} \link[ivec]{vec}?
#' @export
lgl_vec <- function(x) {atm_vec(x, 'lgl')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ineg]{negative-valued numeric}
#'   \link[iarr]{array}?
#' @export
neg_arr <- function(x) {atm_arr(x, 'neg')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ineg]{negative-valued numeric}
#'   \link[igen]{generic}?
#' @export
neg_gen <- function(x) {atm_gen(x, 'neg')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ineg]{negative-valued numeric}
#'    \link[idtf]{dtf}?
#' @export
neg_dtf <- function(x) {atm_dtf(x, 'neg')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ineg]{negative-valued numeric}
#'    \link[ivls]{vlist}?
#' @export
neg_vls <- function(x) {atm_vls(x, 'neg')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ineg]{negative-valued numeric}
#'   \link[ivtp]{vtype}?
#' @export
neg_vtp <- function(x) {atm_vtp(x, 'neg')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ineg]{negative-valued numeric}
#'   \link[imat]{matrix}?
#' @export
neg_mat <- function(x) {atm_mat(x, 'neg')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ineg]{negative-valued numeric}
#'   \link[imvc]{multivec}?
#' @export
neg_mvc <- function(x) {atm_mvc(x, 'neg')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ineg]{negative-valued numeric}
#'    \link[iscl]{scalar}?
#' @export
neg_scl <- function(x) {atm_scl(x, 'neg')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ineg]{negative-valued numeric}
#'   \link[ivec]{vec}?
#' @export
neg_vec <- function(x) {atm_vec(x, 'neg')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ngw]{negative-valued whole-number}
#'    \link[iarr]{array}?
#' @export
ngw_arr <- function(x) {atm_arr(x, 'ngw')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ngw]{negative-valued whole-number}
#'   \link[igen]{generic}?
#' @export
ngw_gen <- function(x) {atm_gen(x, 'ngw')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ngw]{negative-valued whole-number}
#'   \link[idtf]{dtf}?
#' @export
ngw_dtf <- function(x) {atm_dtf(x, 'ngw')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ngw]{negative-valued whole-number}
#'   \link[ivls]{vlist}?
#' @export
ngw_vls <- function(x) {atm_vls(x, 'ngw')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ngw]{negative-valued whole-number}
#'   \link[ivtp]{vtype}?
#' @export
ngw_vtp <- function(x) {atm_vtp(x, 'ngw')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ngw]{negative-valued whole-number}
#'   \link[imat]{matrix}?
#' @export
ngw_mat <- function(x) {atm_mat(x, 'ngw')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ngw]{negative-valued whole-number}
#'   \link[imvc]{multivec}?
#' @export
ngw_mvc <- function(x) {atm_mvc(x, 'ngw')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ngw]{negative-valued whole-number}
#'   \link[iscl]{scalar}?
#' @export
ngw_scl <- function(x) {atm_scl(x, 'ngw')}

#' @describeIn mmm_ccc. Is \code{x} a \link[ngw]{negative-valued whole-number}
#'   \link[ivec]{vec}?
#' @export
ngw_vec <- function(x) {atm_vec(x, 'ngw')}

#' @describeIn mmm_ccc. Is \code{x} a \link[nng]{non-negative-valued numeric}
#'   \link[iarr]{array}?
#' @export
nng_arr <- function(x) {atm_arr(x, 'nng')}

#' @describeIn mmm_ccc. Is \code{x} a \link[nng]{non-negative-valued numeric}
#'   \link[igen]{generic}?
#' @export
nng_gen <- function(x) {atm_gen(x, 'nng')}

#' @describeIn mmm_ccc. Is \code{x} a \link[nng]{non-negative-valued numeric}
#'   \link[idtf]{dtf}?
#' @export
nng_dtf <- function(x) {atm_dtf(x, 'nng')}

#' @describeIn mmm_ccc. Is \code{x} a \link[nng]{non-negative-valued numeric}
#'   \link[ivls]{vlist}?
#' @export
nng_vls <- function(x) {atm_vls(x, 'nng')}

#' @describeIn mmm_ccc. Is \code{x} a \link[nng]{non-negative-valued numeric}
#'   \link[ivtp]{vtype}?
#' @export
nng_vtp <- function(x) {atm_vtp(x, 'nng')}

#' @describeIn mmm_ccc. Is \code{x} a \link[nng]{non-negative-valued numeric}
#'   \link[imat]{matrix}?
#' @export
nng_mat <- function(x) {atm_mat(x, 'nng')}

#' @describeIn mmm_ccc. Is \code{x} a \link[nng]{non-negative-valued numeric}
#'   \link[imvc]{multivec}?
#' @export
nng_mvc <- function(x) {atm_mvc(x, 'nng')}

#' @describeIn mmm_ccc. Is \code{x} a \link[nng]{non-negative-valued numeric}
#'   \link[iscl]{scalar}?
#' @export
nng_scl <- function(x) {atm_scl(x, 'nng')}

#' @describeIn mmm_ccc. Is \code{x} a \link[nng]{non-negative-valued numeric}
#'   \link[ivec]{vec}?
#' @export
nng_vec <- function(x) {atm_vec(x, 'nng')}

#' @describeIn mmm_ccc. Is \code{x} a
#'   \link[nnw]{non-negative-valued whole-number} \link[iarr]{array}?
#' @export
nnw_arr <- function(x) {atm_arr(x, 'nnw')}

#' @describeIn mmm_ccc. Is \code{x} a
#'   \link[nnw]{non-negative-valued whole-number} \link[igen]{generic}?
#' @export
nnw_gen <- function(x) {atm_gen(x, 'nnw')}

#' @describeIn mmm_ccc. Is \code{x} a
#'   \link[nnw]{non-negative-valued whole-number} \link[idtf]{dtf}?
#' @export
nnw_dtf <- function(x) {atm_dtf(x, 'nnw')}

#' @describeIn mmm_ccc. Is \code{x} a
#'   \link[nnw]{non-negative-valued whole-number} \link[ivls]{vlist}?
#' @export
nnw_vls <- function(x) {atm_vls(x, 'nnw')}

#' @describeIn mmm_ccc. Is \code{x} a
#'   \link[nnw]{non-negative-valued whole-number} \link[ivtp]{vtype}?
#' @export
nnw_vtp <- function(x) {atm_vtp(x, 'nnw')}

#' @describeIn mmm_ccc. Is \code{x} a
#'   \link[nnw]{non-negative-valued whole-number} \link[imat]{matrix}?
#' @export
nnw_mat <- function(x) {atm_mat(x, 'nnw')}

#' @describeIn mmm_ccc. Is \code{x} a
#'   \link[nnw]{non-negative-valued whole-number} \link[imvc]{multivec}?
#' @export
nnw_mvc <- function(x) {atm_mvc(x, 'nnw')}

#' @describeIn mmm_ccc. Is \code{x} a
#'   \link[nnw]{non-negative-valued whole-number} \link[iscl]{scalar}?
#' @export
nnw_scl <- function(x) {atm_scl(x, 'nnw')}

#' @describeIn mmm_ccc. Is \code{x} a
#'   \link[nnw]{non-negative-valued whole-number} \link[ivec]{vec}?
#' @export
nnw_vec <- function(x) {atm_vec(x, 'nnw')}

#' @describeIn mmm_ccc. Is \code{x} a \link[nps]{non-positive-valued numeric}
#'   \link[iarr]{array}?
#' @export
nps_arr <- function(x) {atm_arr(x, 'nps')}

#' @describeIn mmm_ccc. Is \code{x} a \link[nps]{non-positive-valued numeric}
#'   \link[igen]{generic}?
#' @export
nps_gen <- function(x) {atm_gen(x, 'nps')}

#' @describeIn mmm_ccc. Is \code{x} a \link[nps]{non-positive-valued numeric}
#'   \link[idtf]{dtf}?
#' @export
nps_dtf <- function(x) {atm_dtf(x, 'nps')}

#' @describeIn mmm_ccc. Is \code{x} a \link[nps]{non-positive-valued numeric}
#'   \link[ivls]{vlist}?
#' @export
nps_vls <- function(x) {atm_vls(x, 'nps')}

#' @describeIn mmm_ccc. Is \code{x} a \link[nps]{non-positive-valued numeric}
#'   \link[ivtp]{vtype}?
#' @export
nps_vtp <- function(x) {atm_vtp(x, 'nps')}

#' @describeIn mmm_ccc. Is \code{x} a \link[nps]{non-positive-valued numeric}
#'   \link[imat]{matrix}?
#' @export
nps_mat <- function(x) {atm_mat(x, 'nps')}

#' @describeIn mmm_ccc. Is \code{x} a \link[nps]{non-positive-valued numeric}
#'   \link[imvc]{multivec}?
#' @export
nps_mvc <- function(x) {atm_mvc(x, 'nps')}

#' @describeIn mmm_ccc. Is \code{x} a \link[nps]{non-positive-valued numeric}
#'   \link[iscl]{scalar}?
#' @export
nps_scl <- function(x) {atm_scl(x, 'nps')}

#' @describeIn mmm_ccc. Is \code{x} a \link[nps]{non-positive-valued numeric}
#'   \link[ivec]{vec}?
#' @export
nps_vec <- function(x) {atm_vec(x, 'nps')}

#' @describeIn mmm_ccc. Is \code{x} a
#'   \link[npw]{non-positive-valued whole-number} \link[iarr]{array}?
#' @export
npw_arr <- function(x) {atm_arr(x, 'npw')}

#' @describeIn mmm_ccc. Is \code{x} a
#'   \link[npw]{non-positive-valued whole-number} \link[igen]{generic}?
#' @export
npw_gen <- function(x) {atm_gen(x, 'npw')}

#' @describeIn mmm_ccc. Is \code{x} a
#'   \link[npw]{non-positive-valued whole-number} \link[idtf]{dtf}?
#' @export
npw_dtf <- function(x) {atm_dtf(x, 'npw')}

#' @describeIn mmm_ccc. Is \code{x} a
#'   \link[npw]{non-positive-valued whole-number} \link[ivls]{vlist}?
#' @export
npw_vls <- function(x) {atm_vls(x, 'npw')}

#' @describeIn mmm_ccc. Is \code{x} a
#'   \link[npw]{non-positive-valued whole-number} \link[ivtp]{vtype}?
#' @export
npw_vtp <- function(x) {atm_vtp(x, 'npw')}

#' @describeIn mmm_ccc. Is \code{x} a
#'   \link[npw]{non-positive-valued whole-number} \link[imat]{matrix}?
#' @export
npw_mat <- function(x) {atm_mat(x, 'npw')}

#' @describeIn mmm_ccc. Is \code{x} a
#'   \link[npw]{non-positive-valued whole-number} \link[imvc]{multivec}?
#' @export
npw_mvc <- function(x) {atm_mvc(x, 'npw')}

#' @describeIn mmm_ccc. Is \code{x} a
#'   \link[npw]{non-positive-valued whole-number} \link[iscl]{scalar}?
#' @export
npw_scl <- function(x) {atm_scl(x, 'npw')}

#' @describeIn mmm_ccc. Is \code{x} a
#'   \link[npw]{non-positive-valued whole-number} \link[ivec]{vec}?
#' @export
npw_vec <- function(x) {atm_vec(x, 'npw')}

#' @describeIn mmm_ccc. Is \code{x} a \link[nst]{non-sortable}
#'   \link[iarr]{array}?
#' @export
nst_arr <- function(x) {atm_arr(x, 'nst')}

#' @describeIn mmm_ccc. Is \code{x} a \link[nst]{non-sortable}
#'   \link[igen]{generic}?
#' @export
nst_gen <- function(x) {atm_gen(x, 'nst')}

#' @describeIn mmm_ccc. Is \code{x} a \link[nst]{non-sortable}
#'   \link[idtf]{dtf}?
#' @export
nst_dtf <- function(x) {atm_dtf(x, 'nst')}

#' @describeIn mmm_ccc. Is \code{x} a \link[nst]{non-sortable}
#'   \link[ivls]{vlist}?
#' @export
nst_vls <- function(x) {atm_vls(x, 'nst')}

#' @describeIn mmm_ccc. Is \code{x} a \link[nst]{non-sortable}
#'   \link[ivtp]{vtype}?
#' @export
nst_vtp <- function(x) {atm_vtp(x, 'nst')}

#' @describeIn mmm_ccc. Is \code{x} a \link[nst]{non-sortable}
#'   \link[imat]{matrix}?
#' @export
nst_mat <- function(x) {atm_mat(x, 'nst')}

#' @describeIn mmm_ccc. Is \code{x} a \link[nst]{non-sortable}
#'   \link[imvc]{multivec}?
#' @export
nst_mvc <- function(x) {atm_mvc(x, 'nst')}

#' @describeIn mmm_ccc. Is \code{x} a \link[nst]{non-sortable}
#'   \link[iscl]{scalar}?
#' @export
nst_scl <- function(x) {atm_scl(x, 'nst')}

#' @describeIn mmm_ccc. Is \code{x} a \link[nst]{non-sortable} \link[ivec]{vec}?
#' @export
nst_vec <- function(x) {atm_vec(x, 'nst')}

#' @describeIn mmm_ccc. Is \code{x} a \link[num]{numeric} \link[iarr]{array}?
#' @export
num_arr <- function(x) {atm_arr(x, 'num')}

#' @describeIn mmm_ccc. Is \code{x} a \link[num]{numeric} \link[igen]{generic}?
#' @export
num_gen <- function(x) {atm_gen(x, 'num')}

#' @describeIn mmm_ccc. Is \code{x} a \link[num]{numeric} \link[idtf]{dtf}?
#' @export
num_dtf <- function(x) {atm_dtf(x, 'num')}

#' @describeIn mmm_ccc. Is \code{x} a \link[num]{numeric} \link[ivls]{vlist}?
#' @export
num_vls <- function(x) {atm_vls(x, 'num')}

#' @describeIn mmm_ccc. Is \code{x} a \link[num]{numeric} \link[ivtp]{vtype}?
#' @export
num_vtp <- function(x) {atm_vtp(x, 'num')}

#' @describeIn mmm_ccc. Is \code{x} a \link[num]{numeric} \link[imat]{matrix}?
#' @export
num_mat <- function(x) {atm_mat(x, 'num')}

#' @describeIn mmm_ccc. Is \code{x} a \link[num]{numeric} \link[imvc]{multivec}?
#' @export
num_mvc <- function(x) {atm_mvc(x, 'num')}

#' @describeIn mmm_ccc. Is \code{x} a \link[num]{numeric} \link[iscl]{scalar}?
#' @export
num_scl <- function(x) {atm_scl(x, 'num')}

#' @describeIn mmm_ccc. Is \code{x} a \link[num]{numeric} \link[ivec]{vec}?
#' @export
num_vec <- function(x) {atm_vec(x, 'num')}

#' @describeIn mmm_ccc. Is \code{x} an \link[odd]{odd-valued whole-number}
#'   \link[iarr]{array}?
#' @export
odd_arr <- function(x) {atm_arr(x, 'odd')}

#' @describeIn mmm_ccc. Is \code{x} an \link[odd]{odd-valued whole-number}
#'   \link[igen]{generic}?
#' @export
odd_gen <- function(x) {atm_gen(x, 'odd')}

#' @describeIn mmm_ccc. Is \code{x} an \link[odd]{odd-valued whole-number}
#'   \link[idtf]{dtf}?
#' @export
odd_dtf <- function(x) {atm_dtf(x, 'odd')}

#' @describeIn mmm_ccc. Is \code{x} an \link[odd]{odd-valued whole-number}
#'   \link[ivls]{vlist}?
#' @export
odd_vls <- function(x) {atm_vls(x, 'odd')}

#' @describeIn mmm_ccc. Is \code{x} an \link[odd]{odd-valued whole-number}
#'   \link[ivtp]{vtype}?
#' @export
odd_vtp <- function(x) {atm_vtp(x, 'odd')}

#' @describeIn mmm_ccc. Is \code{x} an \link[odd]{odd-valued whole-number}
#'   \link[imat]{matrix}?
#' @export
odd_mat <- function(x) {atm_mat(x, 'odd')}

#' @describeIn mmm_ccc. Is \code{x} an \link[odd]{odd-valued whole-number}
#'   \link[imvc]{multivec}?
#' @export
odd_mvc <- function(x) {atm_mvc(x, 'odd')}

#' @describeIn mmm_ccc. Is \code{x} an \link[odd]{odd-valued whole-number}
#'   \link[iscl]{scalar}?
#' @export
odd_scl <- function(x) {atm_scl(x, 'odd')}

#' @describeIn mmm_ccc. Is \code{x} an \link[odd]{odd-valued whole-number}
#'   \link[ivec]{vec}?
#' @export
odd_vec <- function(x) {atm_vec(x, 'odd')}

#' @describeIn mmm_ccc. Is \code{x} an \link[ord]{ordered-factor}
#'   \link[iarr]{array}?
#' @export
ord_arr <- function(x) {atm_arr(x, 'ord')}

#' @describeIn mmm_ccc. Is \code{x} an \link[ord]{ordered-factor}
#'   \link[igen]{generic}?
#' @export
ord_gen <- function(x) {atm_gen(x, 'ord')}

#' @describeIn mmm_ccc. Is \code{x} an \link[ord]{ordered-factor}
#'   \link[idtf]{dtf}?
#' @export
ord_dtf <- function(x) {atm_dtf(x, 'ord')}

#' @describeIn mmm_ccc. Is \code{x} an \link[ord]{ordered-factor}
#'   \link[ivls]{vlist}?
#' @export
ord_vls <- function(x) {atm_vls(x, 'ord')}

#' @describeIn mmm_ccc. Is \code{x} an \link[ord]{ordered-factor}
#'   \link[ivtp]{vtype}?
#' @export
ord_vtp <- function(x) {atm_vtp(x, 'ord')}

#' @describeIn mmm_ccc. Is \code{x} an \link[ord]{ordered-factor}
#'   \link[imat]{matrix}?
#' @export
ord_mat <- function(x) {atm_mat(x, 'ord')}

#' @describeIn mmm_ccc. Is \code{x} an \link[ord]{ordered-factor}
#'   \link[imvc]{multivec}?
#' @export
ord_mvc <- function(x) {atm_mvc(x, 'ord')}

#' @describeIn mmm_ccc. Is \code{x} an \link[ord]{ordered-factor}
#'   \link[iscl]{scalar}?
#' @export
ord_scl <- function(x) {atm_scl(x, 'ord')}

#' @describeIn mmm_ccc. Is \code{x} an \link[ord]{ordered-factor}
#'   \link[ivec]{vec}?
#' @export
ord_vec <- function(x) {atm_vec(x, 'ord')}

#' @describeIn mmm_ccc. Is \code{x} a \link[pct]{percent-valued (0-100) numeric}
#'   \link[iarr]{array}?
#' @export
pct_arr <- function(x) {atm_arr(x, 'pct')}

#' @describeIn mmm_ccc. Is \code{x} a \link[pct]{percent-valued (0-100) numeric}
#'   \link[igen]{generic}?
#' @export
pct_gen <- function(x) {atm_gen(x, 'pct')}

#' @describeIn mmm_ccc. Is \code{x} a \link[pct]{percent-valued (0-100) numeric}
#'   \link[idtf]{dtf}?
#' @export
pct_dtf <- function(x) {atm_dtf(x, 'pct')}

#' @describeIn mmm_ccc. Is \code{x} a \link[pct]{percent-valued (0-100) numeric}
#'   \link[ivls]{vlist}?
#' @export
pct_vls <- function(x) {atm_vls(x, 'pct')}

#' @describeIn mmm_ccc. Is \code{x} a \link[pct]{percent-valued (0-100) numeric}
#'   \link[ivtp]{vtype}?
#' @export
pct_vtp <- function(x) {atm_vtp(x, 'pct')}

#' @describeIn mmm_ccc. Is \code{x} a \link[pct]{percent-valued (0-100) numeric}
#'   \link[imat]{matrix}?
#' @export
pct_mat <- function(x) {atm_mat(x, 'pct')}

#' @describeIn mmm_ccc. Is \code{x} a \link[pct]{percent-valued (0-100) numeric}
#'   \link[imvc]{multivec}?
#' @export
pct_mvc <- function(x) {atm_mvc(x, 'pct')}

#' @describeIn mmm_ccc. Is \code{x} a \link[pct]{percent-valued (0-100) numeric}
#'   \link[iscl]{scalar}?
#' @export
pct_scl <- function(x) {atm_scl(x, 'pct')}

#' @describeIn mmm_ccc. Is \code{x} a \link[pct]{percent-valued (0-100) numeric}
#'   \link[ivec]{vec}?
#' @export
pct_vec <- function(x) {atm_vec(x, 'pct')}

#' @describeIn mmm_ccc. Is \code{x} a \link[pos]{positive-valued numeric}
#'   \link[iarr]{array}?
#' @export
pos_arr <- function(x) {atm_arr(x, 'pos')}

#' @describeIn mmm_ccc. Is \code{x} a \link[pos]{positive-valued numeric}
#'   \link[igen]{generic}?
#' @export
pos_gen <- function(x) {atm_gen(x, 'pos')}

#' @describeIn mmm_ccc. Is \code{x} a \link[pos]{positive-valued numeric}
#'   \link[idtf]{dtf}?
#' @export
pos_dtf <- function(x) {atm_dtf(x, 'pos')}

#' @describeIn mmm_ccc. Is \code{x} a \link[pos]{positive-valued numeric}
#'   \link[ivls]{vlist}?
#' @export
pos_vls <- function(x) {atm_vls(x, 'pos')}

#' @describeIn mmm_ccc. Is \code{x} a \link[pos]{positive-valued numeric}
#'   \link[ivtp]{vtype}?
#' @export
pos_vtp <- function(x) {atm_vtp(x, 'pos')}

#' @describeIn mmm_ccc. Is \code{x} a \link[pos]{positive-valued numeric}
#'   \link[imat]{matrix}?
#' @export
pos_mat <- function(x) {atm_mat(x, 'pos')}

#' @describeIn mmm_ccc. Is \code{x} a \link[pos]{positive-valued numeric}
#'   \link[imvc]{multivec}?
#' @export
pos_mvc <- function(x) {atm_mvc(x, 'pos')}

#' @describeIn mmm_ccc. Is \code{x} a \link[pos]{positive-valued numeric}
#'   \link[iscl]{scalar}?
#' @export
pos_scl <- function(x) {atm_scl(x, 'pos')}

#' @describeIn mmm_ccc. Is \code{x} a \link[pos]{positive-valued numeric}
#'   \link[ivec]{vec}?
#' @export
pos_vec <- function(x) {atm_vec(x, 'pos')}

#' @describeIn mmm_ccc. Is \code{x} a
#'   \link[ppn]{proportion-valued (0-1) numeric} \link[iarr]{array}?
#' @export
ppn_arr <- function(x) {atm_arr(x, 'ppn')}

#' @describeIn mmm_ccc. Is \code{x} a
#'   \link[ppn]{proportion-valued (0-1) numeric} \link[igen]{generic}?
#' @export
ppn_gen <- function(x) {atm_gen(x, 'ppn')}

#' @describeIn mmm_ccc. Is \code{x} a
#'   \link[ppn]{proportion-valued (0-1) numeric} \link[idtf]{dtf}?
#' @export
ppn_dtf <- function(x) {atm_dtf(x, 'ppn')}

#' @describeIn mmm_ccc. Is \code{x} a
#'   \link[ppn]{proportion-valued (0-1) numeric} \link[ivls]{vlist}?
#' @export
ppn_vls <- function(x) {atm_vls(x, 'ppn')}

#' @describeIn mmm_ccc. Is \code{x} a
#'   \link[ppn]{proportion-valued (0-1) numeric} \link[ivtp]{vtype}?
#' @export
ppn_vtp <- function(x) {atm_vtp(x, 'ppn')}

#' @describeIn mmm_ccc. Is \code{x} a
#'   \link[ppn]{proportion-valued (0-1) numeric} \link[imat]{matrix}?
#' @export
ppn_mat <- function(x) {atm_mat(x, 'ppn')}

#' @describeIn mmm_ccc. Is \code{x} a
#'   \link[ppn]{proportion-valued (0-1) numeric} \link[imvc]{multivec}?
#' @export
ppn_mvc <- function(x) {atm_mvc(x, 'ppn')}

#' @describeIn mmm_ccc. Is \code{x} a
#'   \link[ppn]{proportion-valued (0-1) numeric} \link[iscl]{scalar}?
#' @export
ppn_scl <- function(x) {atm_scl(x, 'ppn')}

#' @describeIn mmm_ccc. Is \code{x} a
#'   \link[ppn]{proportion-valued (0-1) numeric} \link[ivec]{vec}?
#' @export
ppn_vec <- function(x) {atm_vec(x, 'ppn')}

#' @describeIn mmm_ccc. Is \code{x} a \link[pos]{positive-valued whole-number}
#'   \link[iarr]{array}?
#' @export
psw_arr <- function(x) {atm_arr(x, 'psw')}

#' @describeIn mmm_ccc. Is \code{x} a \link[pos]{positive-valued whole-number}
#'   \link[igen]{generic}?
#' @export
psw_gen <- function(x) {atm_gen(x, 'psw')}

#' @describeIn mmm_ccc. Is \code{x} a \link[pos]{positive-valued whole-number}
#'   \link[idtf]{dtf}?
#' @export
psw_dtf <- function(x) {atm_dtf(x, 'psw')}

#' @describeIn mmm_ccc. Is \code{x} a \link[pos]{positive-valued whole-number}
#'   \link[ivls]{vlist}?
#' @export
psw_vls <- function(x) {atm_vls(x, 'psw')}

#' @describeIn mmm_ccc. Is \code{x} a \link[pos]{positive-valued whole-number}
#'   \link[ivtp]{vtype}?
#' @export
psw_vtp <- function(x) {atm_vtp(x, 'psw')}

#' @describeIn mmm_ccc. Is \code{x} a \link[pos]{positive-valued whole-number}
#'   \link[imat]{matrix}?
#' @export
psw_mat <- function(x) {atm_mat(x, 'psw')}

#' @describeIn mmm_ccc. Is \code{x} a \link[pos]{positive-valued whole-number}
#'   \link[imvc]{multivec}?
#' @export
psw_mvc <- function(x) {atm_mvc(x, 'psw')}

#' @describeIn mmm_ccc. Is \code{x} a \link[pos]{positive-valued whole-number}
#'   \link[iscl]{scalar}?
#' @export
psw_scl <- function(x) {atm_scl(x, 'psw')}

#' @describeIn mmm_ccc. Is \code{x} a \link[pos]{positive-valued whole-number}
#'   \link[ivec]{vec}?
#' @export
psw_vec <- function(x) {atm_vec(x, 'psw')}

#' @describeIn mmm_ccc. Is \code{x} a \link[srt]{sortable} \link[iarr]{array}?
#' @export
srt_arr <- function(x) {atm_arr(x, 'srt')}

#' @describeIn mmm_ccc. Is \code{x} a \link[srt]{sortable} \link[igen]{generic}?
#' @export
srt_gen <- function(x) {atm_gen(x, 'srt')}

#' @describeIn mmm_ccc. Is \code{x} a \link[srt]{sortable} \link[idtf]{dtf}?
#' @export
srt_dtf <- function(x) {atm_dtf(x, 'srt')}

#' @describeIn mmm_ccc. Is \code{x} a \link[srt]{sortable} \link[ivls]{vlist}?
#' @export
srt_vls <- function(x) {atm_vls(x, 'srt')}

#' @describeIn mmm_ccc. Is \code{x} a \link[srt]{sortable} \link[ivtp]{vtype}?
#' @export
srt_vtp <- function(x) {atm_vtp(x, 'srt')}

#' @describeIn mmm_ccc. Is \code{x} a \link[srt]{sortable} \link[imat]{matrix}?
#' @export
srt_mat <- function(x) {atm_mat(x, 'srt')}

#' @describeIn mmm_ccc. Is \code{x} a \link[srt]{sortable}
#'   \link[imvc]{multivec}?
#' @export
srt_mvc <- function(x) {atm_mvc(x, 'srt')}

#' @describeIn mmm_ccc. Is \code{x} a \link[srt]{sortable} \link[iscl]{scalar}?
#' @export
srt_scl <- function(x) {atm_scl(x, 'srt')}

#' @describeIn mmm_ccc. Is \code{x} a \link[srt]{sortable} \link[ivec]{vec}?
#' @export
srt_vec <- function(x) {atm_vec(x, 'srt')}

#' @describeIn mmm_ccc. Is \code{x} a \link[str]{string} \link[iarr]{array}?
#' @export
str_arr <- function(x) {atm_arr(x, 'str')}

#' @describeIn mmm_ccc. Is \code{x} a \link[str]{string} \link[igen]{generic}?
#' @export
str_gen <- function(x) {atm_gen(x, 'str')}

#' @describeIn mmm_ccc. Is \code{x} a \link[str]{string} \link[idtf]{dtf}?
#' @export
str_dtf <- function(x) {atm_dtf(x, 'str')}

#' @describeIn mmm_ccc. Is \code{x} a \link[str]{string} \link[ivls]{vlist}?
#' @export
str_vls <- function(x) {atm_vls(x, 'str')}

#' @describeIn mmm_ccc. Is \code{x} a \link[str]{string} \link[ivtp]{vtype}?
#' @export
str_vtp <- function(x) {atm_vtp(x, 'str')}

#' @describeIn mmm_ccc. Is \code{x} a \link[str]{string} \link[imat]{matrix}?
#' @export
str_mat <- function(x) {atm_mat(x, 'str')}

#' @describeIn mmm_ccc. Is \code{x} a \link[str]{string} \link[imvc]{multivec}?
#' @export
str_mvc <- function(x) {atm_mvc(x, 'str')}

#' @describeIn mmm_ccc. Is \code{x} a \link[str]{string} \link[iscl]{scalar}?
#' @export
str_scl <- function(x) {atm_scl(x, 'str')}

#' @describeIn mmm_ccc. Is \code{x} a \link[str]{string} \link[ivec]{vec}?
#' @export
str_vec <- function(x) {atm_vec(x, 'str')}

#' @describeIn mmm_ccc. Is \code{x} an \link[uno]{unordered-factor}
#'   \link[iarr]{array}?
#' @export
uno_arr <- function(x) {atm_arr(x, 'uno')}

#' @describeIn mmm_ccc. Is \code{x} an \link[uno]{unordered-factor}
#'   \link[igen]{generic}?
#' @export
uno_gen <- function(x) {atm_gen(x, 'uno')}

#' @describeIn mmm_ccc. Is \code{x} an \link[uno]{unordered-factor}
#'   \link[idtf]{dtf}?
#' @export
uno_dtf <- function(x) {atm_dtf(x, 'uno')}

#' @describeIn mmm_ccc. Is \code{x} an \link[uno]{unordered-factor}
#'   \link[ivls]{vlist}?
#' @export
uno_vls <- function(x) {atm_vls(x, 'uno')}

#' @describeIn mmm_ccc. Is \code{x} an \link[uno]{unordered-factor}
#'   \link[ivtp]{vtype}?
#' @export
uno_vtp <- function(x) {atm_vtp(x, 'uno')}

#' @describeIn mmm_ccc. Is \code{x} an \link[uno]{unordered-factor}
#'   \link[imat]{matrix}?
#' @export
uno_mat <- function(x) {atm_mat(x, 'uno')}

#' @describeIn mmm_ccc. Is \code{x} an \link[uno]{unordered-factor}
#'   \link[imvc]{multivec}?
#' @export
uno_mvc <- function(x) {atm_mvc(x, 'uno')}

#' @describeIn mmm_ccc. Is \code{x} an \link[uno]{unordered-factor}
#'   \link[iscl]{scalar}?
#' @export
uno_scl <- function(x) {atm_scl(x, 'uno')}

#' @describeIn mmm_ccc. Is \code{x} an \link[uno]{unordered-factor}
#'   \link[ivec]{vec}?
#' @export
uno_vec <- function(x) {atm_vec(x, 'uno')}

#' @describeIn mmm_ccc. Is \code{x} a \link[whl]{whole-number}
#'   \link[iarr]{array}?
#' @export
whl_arr <- function(x) {atm_arr(x, 'whl')}

#' @describeIn mmm_ccc. Is \code{x} a \link[whl]{whole-number}
#'   \link[igen]{generic}?
#' @export
whl_gen <- function(x) {atm_gen(x, 'whl')}

#' @describeIn mmm_ccc. Is \code{x} a \link[whl]{whole-number}
#'   \link[idtf]{dtf}?
#' @export
whl_dtf <- function(x) {atm_dtf(x, 'whl')}

#' @describeIn mmm_ccc. Is \code{x} a \link[whl]{whole-number}
#'   \link[ivls]{vlist}?
#' @export
whl_vls <- function(x) {atm_vls(x, 'whl')}

#' @describeIn mmm_ccc. Is \code{x} a \link[whl]{whole-number}
#'   \link[ivtp]{vtype}?
#' @export
whl_vtp <- function(x) {atm_vtp(x, 'whl')}

#' @describeIn mmm_ccc. Is \code{x} a \link[whl]{whole-number}
#'   \link[imat]{matrix}?
#' @export
whl_mat <- function(x) {atm_mat(x, 'whl')}

#' @describeIn mmm_ccc. Is \code{x} a \link[whl]{whole-number}
#'   \link[imvc]{multivec}?
#' @export
whl_mvc <- function(x) {atm_mvc(x, 'whl')}

#' @describeIn mmm_ccc. Is \code{x} a \link[whl]{whole-number}
#'   \link[iscl]{scalar}?
#' @export
whl_scl <- function(x) {atm_scl(x, 'whl')}

#' @describeIn mmm_ccc. Is \code{x} a \link[whl]{whole-number} \link[ivec]{vec}?
#' @export
whl_vec <- function(x) {atm_vec(x, 'whl')}
