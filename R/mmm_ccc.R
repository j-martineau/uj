#' @name mmm_ccc.
#' @family props
#' @title Extended mode + extended class properties
#' @description Evaluate whether an object is of a specific combination of
#'   extended mode (\code{\link{mmm}}) and extended class (\code{\link{ccc}}).
#' @param x An object.
#' @return A logical scalar.
#' @export
mmm_ccc. <- function() {help("mmm_ccc.", package = "uj")}

#' @describeIn mmm_ccc. Get a character vector of all possible extended mode +
#'   extended class properties.
#' @export
mmm_ccc_vals <- function() {
  x <- expand.grid(mmm = mmm_vals(), ccc = ccc_vals())
  x <- av(apply(x, 1, paste0, collapse = "_"))
  sort(x)
}

#' @describeIn mmm_ccc. Is \code{x} a onechar array? (\code{\link{ich1}},
#'   \code{\link{iarr}})
#' @export
ch1_arr <- function(x) {atm_arr(x, 'ch1')}

#' @describeIn mmm_ccc. Is \code{x} a onechar generic? (\code{\link{ich1}},
#'   \code{\link{iagn}})
#' @export
ch1_gen <- function(x) {atm_gen(x, 'ch1')}

#' @describeIn mmm_ccc. Is \code{x} a onechar dtf? (\code{\link{ich1}},
#'   \code{\link{idtf}})
#' @export
ch1_dtf <- function(x) {atm_dtf(x, 'ch1')}

#' @describeIn mmm_ccc. Is \code{x} a onechar vlist? (\code{\link{ich1}},
#'   \code{\link{iavl}})
#' @export
ch1_vls <- function(x) {atm_vls(x, 'ch1')}

#' @describeIn mmm_ccc. Is \code{x} a onechar vtype? (\code{\link{ich1}},
#'   \code{\link{iavt}})
#' @export
ch1_vtp <- function(x) {atm_vtp(x, 'ch1')}

#' @describeIn mmm_ccc. Is \code{x} a onechar matrix? (\code{\link{ich1}},
#'   \code{\link{imat}})
#' @export
ch1_mat <- function(x) {atm_mat(x, 'ch1')}

#' @describeIn mmm_ccc. Is \code{x} a onechar multivec? (\code{\link{ich1}},
#'   \code{\link{imvc}})
#' @export
ch1_mvc <- function(x) {atm_mvc(x, 'ch1')}

#' @describeIn mmm_ccc. Is \code{x} a onechar scalar? (\code{\link{ich1}},
#'   \code{\link{iscl}})
#' @export
ch1_scl <- function(x) {atm_scl(x, 'ch1')}

#' @describeIn mmm_ccc. Is \code{x} a onechar vector? (\code{\link{ich1}},
#'   \code{\link{ivec}})
#' @export
ch1_vec <- function(x) {atm_vec(x, 'ch1')}

#' @describeIn mmm_ccc. Is \code{x} a character array? (\code{\link{ichr}},
#'   \code{\link{iarr}})
#' @export
chr_arr <- function(x) {atm_arr(x, 'chr')}

#' @describeIn mmm_ccc. Is \code{x} a character generic? (\code{\link{ichr}},
#'   \code{\link{iagn}})
#' @export
chr_gen <- function(x) {atm_gen(x, 'chr')}

#' @describeIn mmm_ccc. Is \code{x} a character dtf? (\code{\link{ichr}},
#'   \code{\link{idtf}})
#' @export
chr_dtf <- function(x) {atm_dtf(x, 'chr')}

#' @describeIn mmm_ccc. Is \code{x} a character vlist? (\code{\link{ichr}},
#'   \code{\link{iavl}})
#' @export
chr_vls <- function(x) {atm_vls(x, 'chr')}

#' @describeIn mmm_ccc. Is \code{x} a character vtype? (\code{\link{ichr}},
#'   \code{\link{iavt}})
#' @export
chr_vtp <- function(x) {atm_vtp(x, 'chr')}

#' @describeIn mmm_ccc. Is \code{x} a character matrix? (\code{\link{ichr}},
#'   \code{\link{imat}})
#' @export
chr_mat <- function(x) {atm_mat(x, 'chr')}

#' @describeIn mmm_ccc. Is \code{x} a character multivec? (\code{\link{ichr}},
#'   \code{\link{imvc}})
#' @export
chr_mvc <- function(x) {atm_mvc(x, 'chr')}

#' @describeIn mmm_ccc. Is \code{x} a character scalar? (\code{\link{ichr}},
#'   \code{\link{iscl}})
#' @export
chr_scl <- function(x) {atm_scl(x, 'chr')}

#' @describeIn mmm_ccc. Is \code{x} a character vector? (\code{\link{ichr}},
#'   \code{\link{ivec}})
#' @export
chr_vec <- function(x) {atm_vec(x, 'chr')}

#' @describeIn mmm_ccc. Is \code{x} a color array? (\code{\link{iclr}},
#'   \code{\link{iarr}})
#' @export
clr_arr <- function(x) {atm_arr(x, 'clr')}

#' @describeIn mmm_ccc. Is \code{x} a color generic? (\code{\link{iclr}},
#'   \code{\link{igen}})
#' @export
clr_gen <- function(x) {atm_gen(x, 'clr')}

#' @describeIn mmm_ccc. Is \code{x} a color dtf? (\code{\link{iclr}},
#'   \code{\link{idtf}})
#' @export
clr_dtf <- function(x) {atm_dtf(x, 'clr')}

#' @describeIn mmm_ccc. Is \code{x} a color vlist? (\code{\link{iclr}},
#'   \code{\link{iavl}})
#' @export
clr_vls <- function(x) {atm_vls(x, 'clr')}

#' @describeIn mmm_ccc. Is \code{x} a color vtype? (\code{\link{iclr}},
#'   \code{\link{iavt}})
#' @export
clr_vtp <- function(x) {atm_vtp(x, 'clr')}

#' @describeIn mmm_ccc. Is \code{x} a color matrix? (\code{\link{iclr}},
#'   \code{\link{imat}})
#' @export
clr_mat <- function(x) {atm_mat(x, 'clr')}

#' @describeIn mmm_ccc. Is \code{x} a color multivec? (\code{\link{iclr}},
#'   \code{\link{imvc}})
#' @export
clr_mvc <- function(x) {atm_mvc(x, 'clr')}

#' @describeIn mmm_ccc. Is \code{x} a color scalar? (\code{\link{iclr}},
#'   \code{\link{iscl}})
#' @export
clr_scl <- function(x) {atm_scl(x, 'clr')}

#' @describeIn mmm_ccc. Is \code{x} a color vector? (\code{\link{iclr}},
#'   \code{\link{ivec}})
#' @export
clr_vec <- function(x) {atm_vec(x, 'clr')}

#' @describeIn mmm_ccc. Is \code{x} an even-valued whole-number array?
#'   (\code{\link{ievn}}, \code{\link{iarr}})
#' @export
evn_arr <- function(x) {atm_arr(x, 'evn')}

#' @describeIn mmm_ccc. Is \code{x} an even-valued whole-number generic?
#'   (\code{\link{ievn}}, \code{\link{iagn}})
#' @export
evn_gen <- function(x) {atm_gen(x, 'evn')}

#' @describeIn mmm_ccc. Is \code{x} an even-valued whole-number dtf?
#'   (\code{\link{ievn}}, \code{\link{idtf}})
#' @export
evn_dtf <- function(x) {atm_dtf(x, 'evn')}

#' @describeIn mmm_ccc. Is \code{x} an even-valued whole-number vlist?
#'   (\code{\link{ievn}}, \code{\link{iavl}})
#' @export
evn_vls <- function(x) {atm_vls(x, 'evn')}

#' @describeIn mmm_ccc. Is \code{x} an even-valued
#'   whole-number vtype?
#'   (\code{\link{ievn}}, \code{\link{iavt}})
#' @export
evn_vtp <- function(x) {atm_vtp(x, 'evn')}

#' @describeIn mmm_ccc. Is \code{x} an even-valued
#'   whole-number matrix?
#'   (\code{\link{ievn}}, \code{\link{imat}})
#' @export
evn_mat <- function(x) {atm_mat(x, 'evn')}

#' @describeIn mmm_ccc. Is \code{x} an even-valued
#'   whole-number multivec?
#'   (\code{\link{ievn}}, \code{\link{imvc}})
#' @export
evn_mvc <- function(x) {atm_mvc(x, 'evn')}

#' @describeIn mmm_ccc. Is \code{x} an even-valued whole-number scalar?
#'   (\code{\link{ievn}}, \code{\link{iscl}})
#' @export
evn_scl <- function(x) {atm_scl(x, 'evn')}

#' @describeIn mmm_ccc. Is \code{x} an even-valued whole-number vector?
#'   (\code{\link{ievn}}, \code{\link{ivec}})
#' @export
evn_vec <- function(x) {atm_vec(x, 'evn')}

#' @describeIn mmm_ccc. Is \code{x} a factor array? (\code{\link{ifac}},
#'   \code{\link{iarr}})
#' @export
fac_arr <- function(x) {atm_arr(x, 'fac')}

#' @describeIn mmm_ccc. Is \code{x} a factor generic? (\code{\link{ifac}},
#'   \code{\link{iagn}})
#' @export
fac_gen <- function(x) {atm_gen(x, 'fac')}

#' @describeIn mmm_ccc. Is \code{x} a factor dtf? (\code{\link{ifac}},
#'   \code{\link{idtf}})
#' @export
fac_dtf <- function(x) {atm_dtf(x, 'fac')}

#' @describeIn mmm_ccc. Is \code{x} a factor vlist? (\code{\link{ifac}},
#'   \code{\link{iavl}})
#' @export
fac_vls <- function(x) {atm_vls(x, 'fac')}

#' @describeIn mmm_ccc. Is \code{x} a factor vtype? (\code{\link{ifac}},
#'   \code{\link{iavt}})
#' @export
fac_vtp <- function(x) {atm_vtp(x, 'fac')}

#' @describeIn mmm_ccc. Is \code{x} a factor matrix? (\code{\link{ifac}},
#'   \code{\link{imat}})
#' @export
fac_mat <- function(x) {atm_mat(x, 'fac')}

#' @describeIn mmm_ccc. Is \code{x} a factor multivec? (\code{\link{ifac}},
#'   \code{\link{imvc}})
#' @export
fac_mvc <- function(x) {atm_mvc(x, 'fac')}

#' @describeIn mmm_ccc. Is \code{x} a factor scalar? (\code{\link{ifac}},
#'   \code{\link{iscl}})
#' @export
fac_scl <- function(x) {atm_scl(x, 'fac')}

#' @describeIn mmm_ccc. Is \code{x} a factor vector? (\code{\link{ifac}},
#'   \code{\link{ivec}})
#' @export
fac_vec <- function(x) {atm_vec(x, 'fac')}

#' @describeIn mmm_ccc. Is \code{x} a fractional-valued numeric array?
#'   (\code{\link{ifrc}}, \code{\link{iarr}})
#' @export
frc_arr <- function(x) {atm_arr(x, 'frc')}

#' @describeIn mmm_ccc. Is \code{x} a fractional-valued numeric generic?
#'   (\code{\link{ifrc}}, \code{\link{iagn}})
#' @export
frc_gen <- function(x) {atm_gen(x, 'frc')}

#' @describeIn mmm_ccc. Is \code{x} a fractional-valued numeric dtf?
#'   (\code{\link{ifrc}}, \code{\link{idtf}})
#' @export
frc_dtf <- function(x) {atm_dtf(x, 'frc')}

#' @describeIn mmm_ccc. Is \code{x} a fractional-valued numeric vlist?
#'   (\code{\link{ifrc}}, \code{\link{iavl}})
#' @export
frc_vls <- function(x) {atm_vls(x, 'frc')}

#' @describeIn mmm_ccc. Is \code{x} a fractional-valued numeric vtype?
#'   (\code{\link{ifrc}}, \code{\link{iavt}})
#' @export
frc_vtp <- function(x) {atm_vtp(x, 'frc')}

#' @describeIn mmm_ccc. Is \code{x} a fractional-valued
#'   numeric matrix?
#'   (\code{\link{ifrc}}, \code{\link{imat}})
#' @export
frc_mat <- function(x) {atm_mat(x, 'frc')}

#' @describeIn mmm_ccc. Is \code{x} a fractional-valued
#'   numeric multivec?
#'   (\code{\link{ifrc}}, \code{\link{imvc}})
#' @export
frc_mvc <- function(x) {atm_mvc(x, 'frc')}

#' @describeIn mmm_ccc. Is \code{x} a fractional-valued
#'   numeric scalar?
#'   (\code{\link{ifrc}}, \code{\link{iscl}})
#' @export
frc_scl <- function(x) {atm_scl(x, 'frc')}

#' @describeIn mmm_ccc. Is \code{x} a fractional-valued
#'   numeric vector?
#'   (\code{\link{ifrc}}, \code{\link{ivec}})
#' @export
frc_vec <- function(x) {atm_vec(x, 'frc')}

#' @describeIn mmm_ccc. Is \code{x} an indexer
#'   array?
#'   (\code{\link{iind}}, \code{\link{iarr}})
#' @export
ind_arr <- function(x) {atm_arr(x, 'ind')}

#' @describeIn mmm_ccc. Is \code{x} an indexer
#'   generic?
#'   (\code{\link{iind}}, \code{\link{iagn}})
#' @export
ind_gen <- function(x) {atm_gen(x, 'ind')}

#' @describeIn mmm_ccc. Is \code{x} an indexer
#'   dtf?
#'   (\code{\link{iind}}, \code{\link{idtf}})
#' @export
ind_dtf <- function(x) {atm_dtf(x, 'ind')}

#' @describeIn mmm_ccc. Is \code{x} an indexer
#'   vlist?
#'   (\code{\link{iind}}, \code{\link{iavl}})
#' @export
ind_vls <- function(x) {atm_vls(x, 'ind')}

#' @describeIn mmm_ccc. Is \code{x} an indexer
#'   vtype?
#'   (\code{\link{iind}}, \code{\link{iavt}})
#' @export
ind_vtp <- function(x) {atm_vtp(x, 'ind')}

#' @describeIn mmm_ccc. Is \code{x} an indexer
#'   matrix?
#'   (\code{\link{iind}}, \code{\link{imat}})
#' @export
ind_mat <- function(x) {atm_mat(x, 'ind')}

#' @describeIn mmm_ccc. Is \code{x} an indexer
#'   multivec?
#'   (\code{\link{iind}}, \code{\link{imvc}})
#' @export
ind_mvc <- function(x) {atm_mvc(x, 'ind')}

#' @describeIn mmm_ccc. Is \code{x} an indexer
#'   scalar?
#'   (\code{\link{iind}}, \code{\link{iscl}})
#' @export
ind_scl <- function(x) {atm_scl(x, 'ind')}

#' @describeIn mmm_ccc. Is \code{x} an indexer
#'   vector?
#'   (\code{\link{iind}}, \code{\link{ivec}})
#' @export
ind_vec <- function(x) {atm_vec(x, 'ind')}

#' @describeIn mmm_ccc. Is \code{x} a logical
#'   array?
#'   (\code{\link{ilgl}}, \code{\link{iarr}})
#' @export
lgl_arr <- function(x) {atm_arr(x, 'lgl')}

#' @describeIn mmm_ccc. Is \code{x} a logical
#'   generic?
#'   (\code{\link{ilgl}}, \code{\link{iagn}})
#' @export
lgl_gen <- function(x) {atm_gen(x, 'lgl')}

#' @describeIn mmm_ccc. Is \code{x} a logical
#'   dtf?
#'   (\code{\link{ilgl}}, \code{\link{idtf}})
#' @export
lgl_dtf <- function(x) {atm_dtf(x, 'lgl')}

#' @describeIn mmm_ccc. Is \code{x} a logical
#'   vlist?
#'   (\code{\link{ilgl}}, \code{\link{iavl}})
#' @export
lgl_vls <- function(x) {atm_vls(x, 'lgl')}

#' @describeIn mmm_ccc. Is \code{x} a logical
#'   vtype?
#'   (\code{\link{ilgl}}, \code{\link{iavt}})
#' @export
lgl_vtp <- function(x) {atm_vtp(x, 'lgl')}

#' @describeIn mmm_ccc. Is \code{x} a logical
#'   matrix?
#'   (\code{\link{ilgl}}, \code{\link{imat}})
#' @export
lgl_mat <- function(x) {atm_mat(x, 'lgl')}

#' @describeIn mmm_ccc. Is \code{x} a logical
#'   multivec?
#'   (\code{\link{ilgl}}, \code{\link{imvc}})
#' @export
lgl_mvc <- function(x) {atm_mvc(x, 'lgl')}

#' @describeIn mmm_ccc. Is \code{x} a logical
#'   scalar?
#'   (\code{\link{ilgl}}, \code{\link{iscl}})
#' @export
lgl_scl <- function(x) {atm_scl(x, 'lgl')}

#' @describeIn mmm_ccc. Is \code{x} a logical
#'   vector?
#'   (\code{\link{ilgl}}, \code{\link{ivec}})
#' @export
lgl_vec <- function(x) {atm_vec(x, 'lgl')}

#' @describeIn mmm_ccc. Is \code{x} a negative-valued
#'   numeric array?
#'   (\code{\link{ineg}}, \code{\link{iarr}})
#' @export
neg_arr <- function(x) {atm_arr(x, 'neg')}

#' @describeIn mmm_ccc. Is \code{x} a negative-valued
#'   numeric generic?
#'   (\code{\link{ineg}}, \code{\link{iagn}})
#' @export
neg_gen <- function(x) {atm_gen(x, 'neg')}

#' @describeIn mmm_ccc. Is \code{x} a negative-valued
#'   numeric dtf?
#'   (\code{\link{ineg}}, \code{\link{idtf}})
#' @export
neg_dtf <- function(x) {atm_dtf(x, 'neg')}

#' @describeIn mmm_ccc. Is \code{x} a negative-valued
#'   numeric vlist?
#'   (\code{\link{ineg}}, \code{\link{iavl}})
#' @export
neg_vls <- function(x) {atm_vls(x, 'neg')}

#' @describeIn mmm_ccc. Is \code{x} a negative-valued
#'   numeric vtype?
#'   (\code{\link{ineg}}, \code{\link{iavt}})
#' @export
neg_vtp <- function(x) {atm_vtp(x, 'neg')}

#' @describeIn mmm_ccc. Is \code{x} a negative-valued
#'   numeric matrix?
#'   (\code{\link{ineg}}, \code{\link{imat}})
#' @export
neg_mat <- function(x) {atm_mat(x, 'neg')}

#' @describeIn mmm_ccc. Is \code{x} a negative-valued
#'   numeric multivec?
#'   (\code{\link{ineg}}, \code{\link{imvc}})
#' @export
neg_mvc <- function(x) {atm_mvc(x, 'neg')}

#' @describeIn mmm_ccc. Is \code{x} a negative-valued
#'   numeric scalar?
#'   (\code{\link{ineg}}, \code{\link{iscl}})
#' @export
neg_scl <- function(x) {atm_scl(x, 'neg')}

#' @describeIn mmm_ccc. Is \code{x} a negative-valued
#'   numeric vector?
#'   (\code{\link{ineg}}, \code{\link{ivec}})
#' @export
neg_vec <- function(x) {atm_vec(x, 'neg')}

#' @describeIn mmm_ccc. Is \code{x} a negative-valued
#'   whole-number array?
#'   (\code{\link{ingw}}, \code{\link{iarr}})
#' @export
ngw_arr <- function(x) {atm_arr(x, 'ngw')}

#' @describeIn mmm_ccc. Is \code{x} a negative-valued
#'   whole-number generic?
#'   (\code{\link{ingw}}, \code{\link{iagn}})
#' @export
ngw_gen <- function(x) {atm_gen(x, 'ngw')}

#' @describeIn mmm_ccc. Is \code{x} a negative-valued
#'   whole-number dtf?
#'   (\code{\link{ingw}}, \code{\link{idtf}})
#' @export
ngw_dtf <- function(x) {atm_dtf(x, 'ngw')}

#' @describeIn mmm_ccc. Is \code{x} a negative-valued
#'   whole-number vlist?
#'   (\code{\link{ingw}}, \code{\link{iavl}})
#' @export
ngw_vls <- function(x) {atm_vls(x, 'ngw')}

#' @describeIn mmm_ccc. Is \code{x} a negative-valued
#'   whole-number vtype?
#'   (\code{\link{ingw}}, \code{\link{iavt}})
#' @export
ngw_vtp <- function(x) {atm_vtp(x, 'ngw')}

#' @describeIn mmm_ccc. Is \code{x} a negative-valued
#'   whole-number matrix?
#'   (\code{\link{ingw}}, \code{\link{imat}})
#' @export
ngw_mat <- function(x) {atm_mat(x, 'ngw')}

#' @describeIn mmm_ccc. Is \code{x} a negative-valued
#'   whole-number multivec?
#'   (\code{\link{ingw}}, \code{\link{imvc}})
#' @export
ngw_mvc <- function(x) {atm_mvc(x, 'ngw')}

#' @describeIn mmm_ccc. Is \code{x} a negative-valued
#'   whole-number scalar?
#'   (\code{\link{ingw}}, \code{\link{iscl}})
#' @export
ngw_scl <- function(x) {atm_scl(x, 'ngw')}

#' @describeIn mmm_ccc. Is \code{x} a negative-valued
#'   whole-number vector?
#'   (\code{\link{ingw}}, \code{\link{ivec}})
#' @export
ngw_vec <- function(x) {atm_vec(x, 'ngw')}

#' @describeIn mmm_ccc. Is \code{x} a non-negative-valued
#'   numeric array?
#'   (\code{\link{inng}}, \code{\link{iarr}})
#' @export
nng_arr <- function(x) {atm_arr(x, 'nng')}

#' @describeIn mmm_ccc. Is \code{x} a non-negative-valued
#'   numeric generic?
#'   (\code{\link{inng}}, \code{\link{iagn}})
#' @export
nng_gen <- function(x) {atm_gen(x, 'nng')}

#' @describeIn mmm_ccc. Is \code{x} a non-negative-valued
#'   numeric dtf?
#'   (\code{\link{inng}}, \code{\link{idtf}})
#' @export
nng_dtf <- function(x) {atm_dtf(x, 'nng')}

#' @describeIn mmm_ccc. Is \code{x} a non-negative-valued
#'   numeric vlist?
#'   (\code{\link{inng}}, \code{\link{iavl}})
#' @export
nng_vls <- function(x) {atm_vls(x, 'nng')}

#' @describeIn mmm_ccc. Is \code{x} a non-negative-valued
#'   numeric vtype?
#'   (\code{\link{inng}}, \code{\link{iavt}})
#' @export
nng_vtp <- function(x) {atm_vtp(x, 'nng')}

#' @describeIn mmm_ccc. Is \code{x} a non-negative-valued
#'   numeric matrix?
#'   (\code{\link{inng}}, \code{\link{imat}})
#' @export
nng_mat <- function(x) {atm_mat(x, 'nng')}

#' @describeIn mmm_ccc. Is \code{x} a non-negative-valued
#'   numeric multivec?
#'   (\code{\link{inng}}, \code{\link{imvc}})
#' @export
nng_mvc <- function(x) {atm_mvc(x, 'nng')}

#' @describeIn mmm_ccc. Is \code{x} a non-negative-valued
#'   numeric scalar?
#'   (\code{\link{inng}}, \code{\link{iscl}})
#' @export
nng_scl <- function(x) {atm_scl(x, 'nng')}

#' @describeIn mmm_ccc. Is \code{x} a non-negative-valued
#'   numeric vector?
#'   (\code{\link{inng}}, \code{\link{ivec}})
#' @export
nng_vec <- function(x) {atm_vec(x, 'nng')}

#' @describeIn mmm_ccc. Is \code{x} a non-negative-valued
#'   whole-number array?
#'   (\code{\link{innw}}, \code{\link{iarr}})
#' @export
nnw_arr <- function(x) {atm_arr(x, 'nnw')}

#' @describeIn mmm_ccc. Is \code{x} a non-negative-valued
#'   whole-number generic?
#'   (\code{\link{innw}}, \code{\link{iagn}})
#' @export
nnw_gen <- function(x) {atm_gen(x, 'nnw')}

#' @describeIn mmm_ccc. Is \code{x} a non-negative-valued
#'   whole-number dtf?
#'   (\code{\link{innw}}, \code{\link{idtf}})
#' @export
nnw_dtf <- function(x) {atm_dtf(x, 'nnw')}

#' @describeIn mmm_ccc. Is \code{x} a non-negative-valued
#'   whole-number vlist?
#'   (\code{\link{innw}}, \code{\link{iavl}})
#' @export
nnw_vls <- function(x) {atm_vls(x, 'nnw')}

#' @describeIn mmm_ccc. Is \code{x} a non-negative-valued
#'   whole-number vtype?
#'   (\code{\link{innw}}, \code{\link{iavt}})
#' @export
nnw_vtp <- function(x) {atm_vtp(x, 'nnw')}

#' @describeIn mmm_ccc. Is \code{x} a non-negative-valued
#'   whole-number matrix?
#'   (\code{\link{innw}}, \code{\link{imat}})
#' @export
nnw_mat <- function(x) {atm_mat(x, 'nnw')}

#' @describeIn mmm_ccc. Is \code{x} a non-negative-valued
#'   whole-number multivec?
#'   (\code{\link{innw}}, \code{\link{imvc}})
#' @export
nnw_mvc <- function(x) {atm_mvc(x, 'nnw')}

#' @describeIn mmm_ccc. Is \code{x} a non-negative-valued
#'   whole-number scalar?
#'   (\code{\link{innw}}, \code{\link{iscl}})
#' @export
nnw_scl <- function(x) {atm_scl(x, 'nnw')}

#' @describeIn mmm_ccc. Is \code{x} a non-negative-valued
#'   whole-number vector?
#'   (\code{\link{innw}}, \code{\link{ivec}})
#' @export
nnw_vec <- function(x) {atm_vec(x, 'nnw')}

#' @describeIn mmm_ccc. Is \code{x} a non-positive-valued
#'   numeric array?
#'   (\code{\link{inps}}, \code{\link{iarr}})
#' @export
nps_arr <- function(x) {atm_arr(x, 'nps')}

#' @describeIn mmm_ccc. Is \code{x} a non-positive-valued
#'   numeric generic?
#'   (\code{\link{inps}}, \code{\link{iagn}})
#' @export
nps_gen <- function(x) {atm_gen(x, 'nps')}

#' @describeIn mmm_ccc. Is \code{x} a non-positive-valued
#'   numeric dtf?
#'   (\code{\link{inps}}, \code{\link{idtf}})
#' @export
nps_dtf <- function(x) {atm_dtf(x, 'nps')}

#' @describeIn mmm_ccc. Is \code{x} a non-positive-valued
#'   numeric vlist?
#'   (\code{\link{inps}}, \code{\link{iavl}})
#' @export
nps_vls <- function(x) {atm_vls(x, 'nps')}

#' @describeIn mmm_ccc. Is \code{x} a non-positive-valued
#'   numeric vtype?
#'   (\code{\link{inps}}, \code{\link{iavt}})
#' @export
nps_vtp <- function(x) {atm_vtp(x, 'nps')}

#' @describeIn mmm_ccc. Is \code{x} a non-positive-valued
#'   numeric matrix?
#'   (\code{\link{inps}}, \code{\link{imat}})
#' @export
nps_mat <- function(x) {atm_mat(x, 'nps')}

#' @describeIn mmm_ccc. Is \code{x} a non-positive-valued
#'   numeric multivec?
#'   (\code{\link{inps}}, \code{\link{imvc}})
#' @export
nps_mvc <- function(x) {atm_mvc(x, 'nps')}

#' @describeIn mmm_ccc. Is \code{x} a non-positive-valued
#'   numeric scalar?
#'   (\code{\link{inps}}, \code{\link{iscl}})
#' @export
nps_scl <- function(x) {atm_scl(x, 'nps')}

#' @describeIn mmm_ccc. Is \code{x} a non-positive-valued
#'   numeric vector?
#'   (\code{\link{inps}}, \code{\link{ivec}})
#' @export
nps_vec <- function(x) {atm_vec(x, 'nps')}

#' @describeIn mmm_ccc. Is \code{x} a non-positive-valued
#'   whole-number array?
#'   (\code{\link{inpw}}, \code{\link{iarr}})
#' @export
npw_arr <- function(x) {atm_arr(x, 'npw')}

#' @describeIn mmm_ccc. Is \code{x} a non-positive-valued
#'   whole-number generic?
#'   (\code{\link{inpw}}, \code{\link{iagn}})
#' @export
npw_gen <- function(x) {atm_gen(x, 'npw')}

#' @describeIn mmm_ccc. Is \code{x} a non-positive-valued
#'   whole-number dtf?
#'   (\code{\link{inpw}}, \code{\link{idtf}})
#' @export
npw_dtf <- function(x) {atm_dtf(x, 'npw')}

#' @describeIn mmm_ccc. Is \code{x} a non-positive-valued
#'   whole-number vlist?
#'   (\code{\link{inpw}}, \code{\link{iavl}})
#' @export
npw_vls <- function(x) {atm_vls(x, 'npw')}

#' @describeIn mmm_ccc. Is \code{x} a non-positive-valued
#'   whole-number vtype?
#'   (\code{\link{inpw}}, \code{\link{iavt}})
#' @export
npw_vtp <- function(x) {atm_vtp(x, 'npw')}

#' @describeIn mmm_ccc. Is \code{x} a non-positive-valued
#'   whole-number matrix?
#'   (\code{\link{inpw}}, \code{\link{imat}})
#' @export
npw_mat <- function(x) {atm_mat(x, 'npw')}

#' @describeIn mmm_ccc. Is \code{x} a non-positive-valued
#'   whole-number multivec?
#'   (\code{\link{inpw}}, \code{\link{imvc}})
#' @export
npw_mvc <- function(x) {atm_mvc(x, 'npw')}

#' @describeIn mmm_ccc. Is \code{x} a non-positive-valued
#'   whole-number scalar?
#'   (\code{\link{inpw}}, \code{\link{iscl}})
#' @export
npw_scl <- function(x) {atm_scl(x, 'npw')}

#' @describeIn mmm_ccc. Is \code{x} a non-positive-valued
#'   whole-number vector?
#'   (\code{\link{inpw}}, \code{\link{ivec}})
#' @export
npw_vec <- function(x) {atm_vec(x, 'npw')}

#' @describeIn mmm_ccc. Is \code{x} a non-sortable
#'   array?
#'   (\code{\link{inst}}, \code{\link{iarr}})
#' @export
nst_arr <- function(x) {atm_arr(x, 'nst')}

#' @describeIn mmm_ccc. Is \code{x} a non-sortable
#'   generic?
#'   (\code{\link{inst}}, \code{\link{iagn}})
#' @export
nst_gen <- function(x) {atm_gen(x, 'nst')}

#' @describeIn mmm_ccc. Is \code{x} a non-sortable
#'   dtf?
#'   (\code{\link{inst}}, \code{\link{idtf}})
#' @export
nst_dtf <- function(x) {atm_dtf(x, 'nst')}

#' @describeIn mmm_ccc. Is \code{x} a non-sortable
#'   vlist?
#'   (\code{\link{inst}}, \code{\link{iavl}})
#' @export
nst_vls <- function(x) {atm_vls(x, 'nst')}

#' @describeIn mmm_ccc. Is \code{x} a non-sortable
#'   vtype?
#'   (\code{\link{inst}}, \code{\link{iavt}})
#' @export
nst_vtp <- function(x) {atm_vtp(x, 'nst')}

#' @describeIn mmm_ccc. Is \code{x} a non-sortable
#'   matrix?
#'   (\code{\link{inst}}, \code{\link{imat}})
#' @export
nst_mat <- function(x) {atm_mat(x, 'nst')}

#' @describeIn mmm_ccc. Is \code{x} a non-sortable
#'   multivec?
#'   (\code{\link{inst}}, \code{\link{imvc}})
#' @export
nst_mvc <- function(x) {atm_mvc(x, 'nst')}

#' @describeIn mmm_ccc. Is \code{x} a non-sortable
#'   scalar?
#'   (\code{\link{inst}}, \code{\link{iscl}})
#' @export
nst_scl <- function(x) {atm_scl(x, 'nst')}

#' @describeIn mmm_ccc. Is \code{x} a non-sortable
#'   vector?
#'   (\code{\link{inst}}, \code{\link{ivec}})
#' @export
nst_vec <- function(x) {atm_vec(x, 'nst')}

#' @describeIn mmm_ccc. Is \code{x} a numeric
#'   array?
#'   (\code{\link{inum}}, \code{\link{iarr}})
#' @export
num_arr <- function(x) {atm_arr(x, 'num')}

#' @describeIn mmm_ccc. Is \code{x} a numeric
#'   generic?
#'   (\code{\link{inum}}, \code{\link{iagn}})
#' @export
num_gen <- function(x) {atm_gen(x, 'num')}

#' @describeIn mmm_ccc. Is \code{x} a numeric
#'   dtf?
#'   (\code{\link{inum}}, \code{\link{idtf}})
#' @export
num_dtf <- function(x) {atm_dtf(x, 'num')}

#' @describeIn mmm_ccc. Is \code{x} a numeric
#'   vlist?
#'   (\code{\link{inum}}, \code{\link{iavl}})
#' @export
num_vls <- function(x) {atm_vls(x, 'num')}

#' @describeIn mmm_ccc. Is \code{x} a numeric
#'   vtype?
#'   (\code{\link{inum}}, \code{\link{iavt}})
#' @export
num_vtp <- function(x) {atm_vtp(x, 'num')}

#' @describeIn mmm_ccc. Is \code{x} a numeric
#'   matrix?
#'   (\code{\link{inum}}, \code{\link{imat}})
#' @export
num_mat <- function(x) {atm_mat(x, 'num')}

#' @describeIn mmm_ccc. Is \code{x} a numeric
#'   multivec?
#'   (\code{\link{inum}}, \code{\link{imvc}})
#' @export
num_mvc <- function(x) {atm_mvc(x, 'num')}

#' @describeIn mmm_ccc. Is \code{x} a numeric
#'   scalar?
#'   (\code{\link{inum}}, \code{\link{iscl}})
#' @export
num_scl <- function(x) {atm_scl(x, 'num')}

#' @describeIn mmm_ccc. Is \code{x} a numeric
#'   vector?
#'   (\code{\link{inum}}, \code{\link{ivec}})
#' @export
num_vec <- function(x) {atm_vec(x, 'num')}

#' @describeIn mmm_ccc. Is \code{x} an odd-valued
#'   whole-number array?
#'   (\code{\link{iodd}}, \code{\link{iarr}})
#' @export
odd_arr <- function(x) {atm_arr(x, 'odd')}

#' @describeIn mmm_ccc. Is \code{x} an odd-valued
#'   whole-number generic?
#'   (\code{\link{iodd}}, \code{\link{iagn}})
#' @export
odd_gen <- function(x) {atm_gen(x, 'odd')}

#' @describeIn mmm_ccc. Is \code{x} an odd-valued
#'   whole-number dtf?
#'   (\code{\link{iodd}}, \code{\link{idtf}})
#' @export
odd_dtf <- function(x) {atm_dtf(x, 'odd')}

#' @describeIn mmm_ccc. Is \code{x} an odd-valued
#'   whole-number vlist?
#'   (\code{\link{iodd}}, \code{\link{iavl}})
#' @export
odd_vls <- function(x) {atm_vls(x, 'odd')}

#' @describeIn mmm_ccc. Is \code{x} an odd-valued
#'   whole-number vtype?
#'   (\code{\link{iodd}}, \code{\link{iavt}})
#' @export
odd_vtp <- function(x) {atm_vtp(x, 'odd')}

#' @describeIn mmm_ccc. Is \code{x} an odd-valued
#'   whole-number matrix?
#'   (\code{\link{iodd}}, \code{\link{imat}})
#' @export
odd_mat <- function(x) {atm_mat(x, 'odd')}

#' @describeIn mmm_ccc. Is \code{x} an odd-valued
#'   whole-number multivec?
#'   (\code{\link{iodd}}, \code{\link{imvc}})
#' @export
odd_mvc <- function(x) {atm_mvc(x, 'odd')}

#' @describeIn mmm_ccc. Is \code{x} an odd-valued
#'   whole-number scalar?
#'   (\code{\link{iodd}}, \code{\link{iscl}})
#' @export
odd_scl <- function(x) {atm_scl(x, 'odd')}

#' @describeIn mmm_ccc. Is \code{x} an odd-valued
#'   whole-number vector?
#'   (\code{\link{iodd}}, \code{\link{ivec}})
#' @export
odd_vec <- function(x) {atm_vec(x, 'odd')}

#' @describeIn mmm_ccc. Is \code{x} an ordered-factor
#'   array?
#'   (\code{\link{iord}}, \code{\link{iarr}})
#' @export
ord_arr <- function(x) {atm_arr(x, 'ord')}

#' @describeIn mmm_ccc. Is \code{x} an ordered-factor
#'   generic?
#'   (\code{\link{iord}}, \code{\link{iagn}})
#' @export
ord_gen <- function(x) {atm_gen(x, 'ord')}

#' @describeIn mmm_ccc. Is \code{x} an ordered-factor
#'   dtf?
#'   (\code{\link{iord}}, \code{\link{idtf}})
#' @export
ord_dtf <- function(x) {atm_dtf(x, 'ord')}

#' @describeIn mmm_ccc. Is \code{x} an ordered-factor
#'   vlist?
#'   (\code{\link{iord}}, \code{\link{iavl}})
#' @export
ord_vls <- function(x) {atm_vls(x, 'ord')}

#' @describeIn mmm_ccc. Is \code{x} an ordered-factor
#'   vtype?
#'   (\code{\link{iord}}, \code{\link{iavt}})
#' @export
ord_vtp <- function(x) {atm_vtp(x, 'ord')}

#' @describeIn mmm_ccc. Is \code{x} an ordered-factor
#'   matrix?
#'   (\code{\link{iord}}, \code{\link{imat}})
#' @export
ord_mat <- function(x) {atm_mat(x, 'ord')}

#' @describeIn mmm_ccc. Is \code{x} an ordered-factor
#'   multivec?
#'   (\code{\link{iord}}, \code{\link{imvc}})
#' @export
ord_mvc <- function(x) {atm_mvc(x, 'ord')}

#' @describeIn mmm_ccc. Is \code{x} an ordered-factor
#'   scalar?
#'   (\code{\link{iord}}, \code{\link{iscl}})
#' @export
ord_scl <- function(x) {atm_scl(x, 'ord')}

#' @describeIn mmm_ccc. Is \code{x} an ordered-factor
#'   vector?
#'   (\code{\link{iord}}, \code{\link{ivec}})
#' @export
ord_vec <- function(x) {atm_vec(x, 'ord')}

#' @describeIn mmm_ccc. Is \code{x} a percent-valued
#'   (0-100) numeric array?
#'   (\code{\link{ipct}}, \code{\link{iarr}})
#' @export
pct_arr <- function(x) {atm_arr(x, 'pct')}

#' @describeIn mmm_ccc. Is \code{x} a percent-valued
#'   (0-100) numeric generic?
#'   (\code{\link{ipct}}, \code{\link{iagn}})
#' @export
pct_gen <- function(x) {atm_gen(x, 'pct')}

#' @describeIn mmm_ccc. Is \code{x} a percent-valued
#'   (0-100) numeric dtf?
#'   (\code{\link{ipct}}, \code{\link{idtf}})
#' @export
pct_dtf <- function(x) {atm_dtf(x, 'pct')}

#' @describeIn mmm_ccc. Is \code{x} a percent-valued
#'   (0-100) numeric vlist?
#'   (\code{\link{ipct}}, \code{\link{iavl}})
#' @export
pct_vls <- function(x) {atm_vls(x, 'pct')}

#' @describeIn mmm_ccc. Is \code{x} a percent-valued
#'   (0-100) numeric vtype?
#'   (\code{\link{ipct}}, \code{\link{iavt}})
#' @export
pct_vtp <- function(x) {atm_vtp(x, 'pct')}

#' @describeIn mmm_ccc. Is \code{x} a percent-valued
#'   (0-100) numeric matrix?
#'   (\code{\link{ipct}}, \code{\link{imat}})
#' @export
pct_mat <- function(x) {atm_mat(x, 'pct')}

#' @describeIn mmm_ccc. Is \code{x} a percent-valued
#'   (0-100) numeric multivec?
#'   (\code{\link{ipct}}, \code{\link{imvc}})
#' @export
pct_mvc <- function(x) {atm_mvc(x, 'pct')}

#' @describeIn mmm_ccc. Is \code{x} a percent-valued
#'   (0-100) numeric scalar?
#'   (\code{\link{ipct}}, \code{\link{iscl}})
#' @export
pct_scl <- function(x) {atm_scl(x, 'pct')}

#' @describeIn mmm_ccc. Is \code{x} a percent-valued
#'   (0-100) numeric vector?
#'   (\code{\link{ipct}}, \code{\link{ivec}})
#' @export
pct_vec <- function(x) {atm_vec(x, 'pct')}

#' @describeIn mmm_ccc. Is \code{x} a positive-valued
#'   numeric array?
#'   (\code{\link{ipos}}, \code{\link{iarr}})
#' @export
pos_arr <- function(x) {atm_arr(x, 'pos')}

#' @describeIn mmm_ccc. Is \code{x} a positive-valued
#'   numeric generic?
#'   (\code{\link{ipos}}, \code{\link{iagn}})
#' @export
pos_gen <- function(x) {atm_gen(x, 'pos')}

#' @describeIn mmm_ccc. Is \code{x} a positive-valued
#'   numeric dtf?
#'   (\code{\link{ipos}}, \code{\link{idtf}})
#' @export
pos_dtf <- function(x) {atm_dtf(x, 'pos')}

#' @describeIn mmm_ccc. Is \code{x} a positive-valued
#'   numeric vlist?
#'   (\code{\link{ipos}}, \code{\link{iavl}})
#' @export
pos_vls <- function(x) {atm_vls(x, 'pos')}

#' @describeIn mmm_ccc. Is \code{x} a positive-valued
#'   numeric vtype?
#'   (\code{\link{ipos}}, \code{\link{iavt}})
#' @export
pos_vtp <- function(x) {atm_vtp(x, 'pos')}

#' @describeIn mmm_ccc. Is \code{x} a positive-valued
#'   numeric matrix?
#'   (\code{\link{ipos}}, \code{\link{imat}})
#' @export
pos_mat <- function(x) {atm_mat(x, 'pos')}

#' @describeIn mmm_ccc. Is \code{x} a positive-valued
#'   numeric multivec?
#'   (\code{\link{ipos}}, \code{\link{imvc}})
#' @export
pos_mvc <- function(x) {atm_mvc(x, 'pos')}

#' @describeIn mmm_ccc. Is \code{x} a positive-valued
#'   numeric scalar?
#'   (\code{\link{ipos}}, \code{\link{iscl}})
#' @export
pos_scl <- function(x) {atm_scl(x, 'pos')}

#' @describeIn mmm_ccc. Is \code{x} a positive-valued
#'   numeric vector?
#'   (\code{\link{ipos}}, \code{\link{ivec}})
#' @export
pos_vec <- function(x) {atm_vec(x, 'pos')}

#' @describeIn mmm_ccc. Is \code{x} a proportion-valued
#'   (0-1) numeric array?
#'   (\code{\link{ippn}}, \code{\link{iarr}})
#' @export
ppn_arr <- function(x) {atm_arr(x, 'ppn')}

#' @describeIn mmm_ccc. Is \code{x} a proportion-valued
#'   (0-1) numeric generic?
#'   (\code{\link{ippn}}, \code{\link{iagn}})
#' @export
ppn_gen <- function(x) {atm_gen(x, 'ppn')}

#' @describeIn mmm_ccc. Is \code{x} a proportion-valued
#'   (0-1) numeric dtf?
#'   (\code{\link{ippn}}, \code{\link{idtf}})
#' @export
ppn_dtf <- function(x) {atm_dtf(x, 'ppn')}

#' @describeIn mmm_ccc. Is \code{x} a proportion-valued
#'   (0-1) numeric vlist?
#'   (\code{\link{ippn}}, \code{\link{iavl}})
#' @export
ppn_vls <- function(x) {atm_vls(x, 'ppn')}

#' @describeIn mmm_ccc. Is \code{x} a proportion-valued
#'   (0-1) numeric vtype?
#'   (\code{\link{ippn}}, \code{\link{iavt}})
#' @export
ppn_vtp <- function(x) {atm_vtp(x, 'ppn')}

#' @describeIn mmm_ccc. Is \code{x} a proportion-valued
#'   (0-1) numeric matrix?
#'   (\code{\link{ippn}}, \code{\link{imat}})
#' @export
ppn_mat <- function(x) {atm_mat(x, 'ppn')}

#' @describeIn mmm_ccc. Is \code{x} a proportion-valued
#'   (0-1) numeric multivec?
#'   (\code{\link{ippn}}, \code{\link{imvc}})
#' @export
ppn_mvc <- function(x) {atm_mvc(x, 'ppn')}

#' @describeIn mmm_ccc. Is \code{x} a proportion-valued
#'   (0-1) numeric scalar?
#'   (\code{\link{ippn}}, \code{\link{iscl}})
#' @export
ppn_scl <- function(x) {atm_scl(x, 'ppn')}

#' @describeIn mmm_ccc. Is \code{x} a proportion-valued
#'   (0-1) numeric vector?
#'   (\code{\link{ippn}}, \code{\link{ivec}})
#' @export
ppn_vec <- function(x) {atm_vec(x, 'ppn')}

#' @describeIn mmm_ccc. Is \code{x} a positive-valued
#'   whole-number array?
#'   (\code{\link{ipsw}}, \code{\link{iarr}})
#' @export
psw_arr <- function(x) {atm_arr(x, 'psw')}

#' @describeIn mmm_ccc. Is \code{x} a positive-valued
#'   whole-number generic?
#'   (\code{\link{ipsw}}, \code{\link{iagn}})
#' @export
psw_gen <- function(x) {atm_gen(x, 'psw')}

#' @describeIn mmm_ccc. Is \code{x} a positive-valued
#'   whole-number dtf?
#'   (\code{\link{ipsw}}, \code{\link{idtf}})
#' @export
psw_dtf <- function(x) {atm_dtf(x, 'psw')}

#' @describeIn mmm_ccc. Is \code{x} a positive-valued
#'   whole-number vlist?
#'   (\code{\link{ipsw}}, \code{\link{iavl}})
#' @export
psw_vls <- function(x) {atm_vls(x, 'psw')}

#' @describeIn mmm_ccc. Is \code{x} a positive-valued
#'   whole-number vtype?
#'   (\code{\link{ipsw}}, \code{\link{iavt}})
#' @export
psw_vtp <- function(x) {atm_vtp(x, 'psw')}

#' @describeIn mmm_ccc. Is \code{x} a positive-valued
#'   whole-number matrix?
#'   (\code{\link{ipsw}}, \code{\link{imat}})
#' @export
psw_mat <- function(x) {atm_mat(x, 'psw')}

#' @describeIn mmm_ccc. Is \code{x} a positive-valued
#'   whole-number multivec?
#'   (\code{\link{ipsw}}, \code{\link{imvc}})
#' @export
psw_mvc <- function(x) {atm_mvc(x, 'psw')}

#' @describeIn mmm_ccc. Is \code{x} a positive-valued
#'   whole-number scalar?
#'   (\code{\link{ipsw}}, \code{\link{iscl}})
#' @export
psw_scl <- function(x) {atm_scl(x, 'psw')}

#' @describeIn mmm_ccc. Is \code{x} a positive-valued
#'   whole-number vector?
#'   (\code{\link{ipsw}}, \code{\link{ivec}})
#' @export
psw_vec <- function(x) {atm_vec(x, 'psw')}

#' @describeIn mmm_ccc. Is \code{x} a sortable
#'   array?
#'   (\code{\link{isrt}}, \code{\link{iarr}})
#' @export
srt_arr <- function(x) {atm_arr(x, 'srt')}

#' @describeIn mmm_ccc. Is \code{x} a sortable
#'   generic?
#'   (\code{\link{isrt}}, \code{\link{iagn}})
#' @export
srt_gen <- function(x) {atm_gen(x, 'srt')}

#' @describeIn mmm_ccc. Is \code{x} a sortable
#'   dtf?
#'   (\code{\link{isrt}}, \code{\link{idtf}})
#' @export
srt_dtf <- function(x) {atm_dtf(x, 'srt')}

#' @describeIn mmm_ccc. Is \code{x} a sortable
#'   vlist?
#'   (\code{\link{isrt}}, \code{\link{iavl}})
#' @export
srt_vls <- function(x) {atm_vls(x, 'srt')}

#' @describeIn mmm_ccc. Is \code{x} a sortable
#'   vtype?
#'   (\code{\link{isrt}}, \code{\link{iavt}})
#' @export
srt_vtp <- function(x) {atm_vtp(x, 'srt')}

#' @describeIn mmm_ccc. Is \code{x} a sortable
#'   matrix?
#'   (\code{\link{isrt}}, \code{\link{imat}})
#' @export
srt_mat <- function(x) {atm_mat(x, 'srt')}

#' @describeIn mmm_ccc. Is \code{x} a sortable
#'   multivec?
#'   (\code{\link{isrt}}, \code{\link{imvc}})
#' @export
srt_mvc <- function(x) {atm_mvc(x, 'srt')}

#' @describeIn mmm_ccc. Is \code{x} a sortable
#'   scalar?
#'   (\code{\link{isrt}}, \code{\link{iscl}})
#' @export
srt_scl <- function(x) {atm_scl(x, 'srt')}

#' @describeIn mmm_ccc. Is \code{x} a sortable
#'   vector?
#'   (\code{\link{isrt}}, \code{\link{ivec}})
#' @export
srt_vec <- function(x) {atm_vec(x, 'srt')}

#' @describeIn mmm_ccc. Is \code{x} a string
#'   array?
#'   (\code{\link{istr}}, \code{\link{iarr}})
#' @export
str_arr <- function(x) {atm_arr(x, 'str')}

#' @describeIn mmm_ccc. Is \code{x} a string
#'   generic?
#'   (\code{\link{istr}}, \code{\link{iagn}})
#' @export
str_gen <- function(x) {atm_gen(x, 'str')}

#' @describeIn mmm_ccc. Is \code{x} a string
#'   dtf?
#'   (\code{\link{istr}}, \code{\link{idtf}})
#' @export
str_dtf <- function(x) {atm_dtf(x, 'str')}

#' @describeIn mmm_ccc. Is \code{x} a string
#'   vlist?
#'   (\code{\link{istr}}, \code{\link{iavl}})
#' @export
str_vls <- function(x) {atm_vls(x, 'str')}

#' @describeIn mmm_ccc. Is \code{x} a string
#'   vtype?
#'   (\code{\link{istr}}, \code{\link{iavt}})
#' @export
str_vtp <- function(x) {atm_vtp(x, 'str')}

#' @describeIn mmm_ccc. Is \code{x} a string
#'   matrix?
#'   (\code{\link{istr}}, \code{\link{imat}})
#' @export
str_mat <- function(x) {atm_mat(x, 'str')}

#' @describeIn mmm_ccc. Is \code{x} a string
#'   multivec?
#'   (\code{\link{istr}}, \code{\link{imvc}})
#' @export
str_mvc <- function(x) {atm_mvc(x, 'str')}

#' @describeIn mmm_ccc. Is \code{x} a string
#'   scalar?
#'   (\code{\link{istr}}, \code{\link{iscl}})
#' @export
str_scl <- function(x) {atm_scl(x, 'str')}

#' @describeIn mmm_ccc. Is \code{x} a string
#'   vector?
#'   (\code{\link{istr}}, \code{\link{ivec}})
#' @export
str_vec <- function(x) {atm_vec(x, 'str')}

#' @describeIn mmm_ccc. Is \code{x} an unordered-factor
#'   array?
#'   (\code{\link{iuno}}, \code{\link{iarr}})
#' @export
uno_arr <- function(x) {atm_arr(x, 'uno')}

#' @describeIn mmm_ccc. Is \code{x} an unordered-factor
#'   generic?
#'   (\code{\link{iuno}}, \code{\link{iagn}})
#' @export
uno_gen <- function(x) {atm_gen(x, 'uno')}

#' @describeIn mmm_ccc. Is \code{x} an unordered-factor
#'   dtf?
#'   (\code{\link{iuno}}, \code{\link{idtf}})
#' @export
uno_dtf <- function(x) {atm_dtf(x, 'uno')}

#' @describeIn mmm_ccc. Is \code{x} an unordered-factor
#'   vlist?
#'   (\code{\link{iuno}}, \code{\link{iavl}})
#' @export
uno_vls <- function(x) {atm_vls(x, 'uno')}

#' @describeIn mmm_ccc. Is \code{x} an unordered-factor
#'   vtype?
#'   (\code{\link{iuno}}, \code{\link{iavt}})
#' @export
uno_vtp <- function(x) {atm_vtp(x, 'uno')}

#' @describeIn mmm_ccc. Is \code{x} an unordered-factor
#'   matrix?
#'   (\code{\link{iuno}}, \code{\link{imat}})
#' @export
uno_mat <- function(x) {atm_mat(x, 'uno')}

#' @describeIn mmm_ccc. Is \code{x} an unordered-factor
#'   multivec?
#'   (\code{\link{iuno}}, \code{\link{imvc}})
#' @export
uno_mvc <- function(x) {atm_mvc(x, 'uno')}

#' @describeIn mmm_ccc. Is \code{x} an unordered-factor
#'   scalar?
#'   (\code{\link{iuno}}, \code{\link{iscl}})
#' @export
uno_scl <- function(x) {atm_scl(x, 'uno')}

#' @describeIn mmm_ccc. Is \code{x} an unordered-factor
#'   vector?
#'   (\code{\link{iuno}}, \code{\link{ivec}})
#' @export
uno_vec <- function(x) {atm_vec(x, 'uno')}

#' @describeIn mmm_ccc. Is \code{x} a whole-number
#'   array?
#'   (\code{\link{iwhl}}, \code{\link{iarr}})
#' @export
whl_arr <- function(x) {atm_arr(x, 'whl')}

#' @describeIn mmm_ccc. Is \code{x} a whole-number
#'   generic?
#'   (\code{\link{iwhl}}, \code{\link{iagn}})
#' @export
whl_gen <- function(x) {atm_gen(x, 'whl')}

#' @describeIn mmm_ccc. Is \code{x} a whole-number
#'   dtf?
#'   (\code{\link{iwhl}}, \code{\link{idtf}})
#' @export
whl_dtf <- function(x) {atm_dtf(x, 'whl')}

#' @describeIn mmm_ccc. Is \code{x} a whole-number
#'   vlist?
#'   (\code{\link{iwhl}}, \code{\link{iavl}})
#' @export
whl_vls <- function(x) {atm_vls(x, 'whl')}

#' @describeIn mmm_ccc. Is \code{x} a whole-number
#'   vtype?
#'   (\code{\link{iwhl}}, \code{\link{iavt}})
#' @export
whl_vtp <- function(x) {atm_vtp(x, 'whl')}

#' @describeIn mmm_ccc. Is \code{x} a whole-number
#'   matrix?
#'   (\code{\link{iwhl}}, \code{\link{imat}})
#' @export
whl_mat <- function(x) {atm_mat(x, 'whl')}

#' @describeIn mmm_ccc. Is \code{x} a whole-number
#'   multivec?
#'   (\code{\link{iwhl}}, \code{\link{imvc}})
#' @export
whl_mvc <- function(x) {atm_mvc(x, 'whl')}

#' @describeIn mmm_ccc. Is \code{x} a whole-number
#'   scalar?
#'   (\code{\link{iwhl}}, \code{\link{iscl}})
#' @export
whl_scl <- function(x) {atm_scl(x, 'whl')}

#' @describeIn mmm_ccc. Is \code{x} a whole-number
#'   vector?
#'   (\code{\link{iwhl}}, \code{\link{ivec}})
#' @export
whl_vec <- function(x) {atm_vec(x, 'whl')}
