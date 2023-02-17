#' @encoding UTF-8
#' @family props
#' @title Completeness + xmode + xclass combination properties
#' @description Check for combinations of \link[=CMP]{completeness}, \link[=mmm]{xmode}, and \link[=ccc]{xclass}.
#' @details
#' \tabular{ll}{  `cmp_mmm_ccc_funs`   \tab What complete + xmode + xclass combination property functions are there?                                                                                                                                                          \cr   \tab  }
#' \tabular{ll}{  `cmp_{mmm}_{ccc}`    \tab Is `x` both complete and a match to the single xmode property `'{mmm}'` and single xclass property `'{ccc}'` where `{mmm}` and `{ccc}` are placeholders for any given xmode property and any given xclass property, respectively. \cr   \tab  }
#' \tabular{ll}{  `cmp_mmm_ccc`        \tab Is `x` both complete and a match to the single xmode and xclass properties in `MMM` and `ccc`, respectively?                                                                                                                                  }
#' @param x An R object.
#' @param MMM A character scalar single xmode property from `mmm_props()`.
#' @param ccc A character scalar single xclass property from `ccc_props()`.
#' @param prop A character scalar.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return **A character scalar** \cr `cmp_mmm_ccc_funs`
#' \cr\cr  **A logical scalar**   \cr `cmp_mmm_ccc, cmp_{mmm}_{ccc}`
#' @examples
#' cmp_mmm_cccFUNS()
#' cmp_mmm_ccc(letters, "ch1", "vec")
#' cmp_mmm_ccc(letters, "str", "scl")
#' cmp_ch1_vec(letters)
#' cmp_str_scl("a")
#' @export
cmp_mmm_ccc <- function(x, mmm, ccc, ...) {
  cfun <- function(cx) {uj::run("uj::i", base::toupper(ccc), "(cx)")}
  mfun <- function(mx) {uj::f0(uj::notATM(mx), F, uj::f0(uj::N0(mx), F, uj::f0(uj::anyNAS(mx), F, uj::run("uj::", base::toupper(mmm), "(mx)"))))}
  dfun <- function(dx) {base::all(base::apply(dx, 2, mfun))}
  vfun <- function(vx) {base::all(base::sapply(vx, mfun))}
  if (uj::isCHR(mmm)) {mmm <- base::tolower(mmm)}
  if (uj::isCHR(ccc)) {ccc <- base::tolower(ccc)}
  errs <- base::c(uj:::.meets_errs(x, ...),
                  uj::f0(uj::isIN(mmm, uj::mmm_props()), NULL, '[mmm] is not a scalar value from mmm_props().'),
                  uj::f0(uj::isIN(ccc, uj::ccc_props()), NULL, '[ccc] is not a scalar value from ccc_props().'))
  uj::errs_if_pop(errs, PKG = "uj")
  uj::f0(!uj::meets(x, ...), F, uj::f0(!cfun(x), F, uj::f0(ccc == "dtf", dfun(x), uj::f0(ccc == "vls", vfun(x), mfun(x)))))
}

#' @rdname cmp_mmm_ccc
#' @export
cmp_mmm_ccc_funs <- function() {uj::SUV(uj::p0('cmp', base::sort(uj::av(base::apply(base::expand.grid(mmm = base::toupper(uj:::.mmm), ccc = uj:::.ccc), 1, base::paste0, collapse = "_")))))}

#' @rdname cmp_mmm_ccc
#' @export
cmp_atm_arr <- function(x, ...) {uj::cmp_mmm_ccc(x, 'atm', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_atm_dtf <- function(x, ...) {uj::cmp_mmm_ccc(x, 'atm', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_atm_gen <- function(x, ...) {uj::cmp_mmm_ccc(x, 'atm', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_atm_mat <- function(x, ...) {uj::cmp_mmm_ccc(x, 'atm', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_atm_mvc <- function(x, ...) {uj::cmp_mmm_ccc(x, 'atm', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_atm_scl <- function(x, ...) {uj::cmp_mmm_ccc(x, 'atm', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_atm_vec <- function(x, ...) {uj::cmp_mmm_ccc(x, 'atm', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_atm_vls <- function(x, ...) {uj::cmp_mmm_ccc(x, 'atm', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ch1_arr <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ch1', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ch1_dtf <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ch1', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ch1_gen <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ch1', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ch1_mat <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ch1', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ch1_mvc <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ch1', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ch1_vec <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ch1', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ch1_vec <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ch1', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ch1_vls <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ch1', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ch3_arr <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ch3', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ch3_dtf <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ch3', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ch3_gen <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ch3', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ch3_mat <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ch3', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ch3_mvc <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ch3', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ch3_scl <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ch3', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ch3_vec <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ch3', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ch3_vls <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ch3', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_chr_arr <- function(x, ...) {uj::cmp_mmm_ccc(x, 'chr', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_chr_dtf <- function(x, ...) {uj::cmp_mmm_ccc(x, 'chr', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_chr_gen <- function(x, ...) {uj::cmp_mmm_ccc(x, 'chr', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_chr_mat <- function(x, ...) {uj::cmp_mmm_ccc(x, 'chr', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_chr_mvc <- function(x, ...) {uj::cmp_mmm_ccc(x, 'chr', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_chr_scl <- function(x, ...) {uj::cmp_mmm_ccc(x, 'chr', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_chr_vec <- function(x, ...) {uj::cmp_mmm_ccc(x, 'chr', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_chr_vls <- function(x, ...) {uj::cmp_mmm_ccc(x, 'chr', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_clr_arr <- function(x, ...) {uj::cmp_mmm_ccc(x, 'clr', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_clr_dtf <- function(x, ...) {uj::cmp_mmm_ccc(x, 'clr', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_clr_gen <- function(x, ...) {uj::cmp_mmm_ccc(x, 'clr', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_clr_mat <- function(x, ...) {uj::cmp_mmm_ccc(x, 'clr', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_clr_mvc <- function(x, ...) {uj::cmp_mmm_ccc(x, 'clr', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_clr_scl <- function(x, ...) {uj::cmp_mmm_ccc(x, 'clr', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_clr_vec <- function(x, ...) {uj::cmp_mmm_ccc(x, 'clr', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_clr_vls <- function(x, ...) {uj::cmp_mmm_ccc(x, 'clr', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_evn_arr <- function(x, ...) {uj::cmp_mmm_ccc(x, 'evn', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_evn_dtf <- function(x, ...) {uj::cmp_mmm_ccc(x, 'evn', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_evn_gen <- function(x, ...) {uj::cmp_mmm_ccc(x, 'evn', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_evn_mat <- function(x, ...) {uj::cmp_mmm_ccc(x, 'evn', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_evn_mvc <- function(x, ...) {uj::cmp_mmm_ccc(x, 'evn', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_evn_scl <- function(x, ...) {uj::cmp_mmm_ccc(x, 'evn', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_evn_vec <- function(x, ...) {uj::cmp_mmm_ccc(x, 'evn', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_evn_vls <- function(x, ...) {uj::cmp_mmm_ccc(x, 'evn', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_fac_arr <- function(x, ...) {uj::cmp_mmm_ccc(x, 'fac', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_fac_dtf <- function(x, ...) {uj::cmp_mmm_ccc(x, 'fac', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_fac_gen <- function(x, ...) {uj::cmp_mmm_ccc(x, 'fac', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_fac_mat <- function(x, ...) {uj::cmp_mmm_ccc(x, 'fac', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_fac_mvc <- function(x, ...) {uj::cmp_mmm_ccc(x, 'fac', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_fac_scl <- function(x, ...) {uj::cmp_mmm_ccc(x, 'fac', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_fac_vec <- function(x, ...) {uj::cmp_mmm_ccc(x, 'fac', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_fac_vls <- function(x, ...) {uj::cmp_mmm_ccc(x, 'fac', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_frc_arr <- function(x, ...) {uj::cmp_mmm_ccc(x, 'frc', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_frc_dtf <- function(x, ...) {uj::cmp_mmm_ccc(x, 'frc', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_frc_gen <- function(x, ...) {uj::cmp_mmm_ccc(x, 'frc', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_frc_mat <- function(x, ...) {uj::cmp_mmm_ccc(x, 'frc', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_frc_mvc <- function(x, ...) {uj::cmp_mmm_ccc(x, 'frc', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_frc_scl <- function(x, ...) {uj::cmp_mmm_ccc(x, 'frc', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_frc_vec <- function(x, ...) {uj::cmp_mmm_ccc(x, 'frc', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_frc_vls <- function(x, ...) {uj::cmp_mmm_ccc(x, 'frc', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ind_arr <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ind', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ind_dtf <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ind', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ind_gen <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ind', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ind_mat <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ind', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ind_mvc <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ind', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ind_scl <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ind', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ind_vec <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ind', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ind_vls <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ind', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_lgl_arr <- function(x, ...) {uj::cmp_mmm_ccc(x, 'lgl', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_lgl_dtf <- function(x, ...) {uj::cmp_mmm_ccc(x, 'lgl', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_lgl_gen <- function(x, ...) {uj::cmp_mmm_ccc(x, 'lgl', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_lgl_mat <- function(x, ...) {uj::cmp_mmm_ccc(x, 'lgl', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_lgl_mvc <- function(x, ...) {uj::cmp_mmm_ccc(x, 'lgl', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_lgl_scl <- function(x, ...) {uj::cmp_mmm_ccc(x, 'lgl', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_lgl_vec <- function(x, ...) {uj::cmp_mmm_ccc(x, 'lgl', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_lgl_vls <- function(x, ...) {uj::cmp_mmm_ccc(x, 'lgl', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_neg_arr <- function(x, ...) {uj::cmp_mmm_ccc(x, 'neg', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_neg_dtf <- function(x, ...) {uj::cmp_mmm_ccc(x, 'neg', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_neg_gen <- function(x, ...) {uj::cmp_mmm_ccc(x, 'neg', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_neg_mat <- function(x, ...) {uj::cmp_mmm_ccc(x, 'neg', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_neg_mvc <- function(x, ...) {uj::cmp_mmm_ccc(x, 'neg', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_neg_scl <- function(x, ...) {uj::cmp_mmm_ccc(x, 'neg', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_neg_vec <- function(x, ...) {uj::cmp_mmm_ccc(x, 'neg', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_neg_vls <- function(x, ...) {uj::cmp_mmm_ccc(x, 'neg', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ngw_arr <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ngw', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ngw_dtf <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ngw', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ngw_gen <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ngw', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ngw_mat <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ngw', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ngw_mvc <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ngw', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ngw_scl <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ngw', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ngw_vec <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ngw', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ngw_vls <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ngw', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nng_arr <- function(x, ...) {uj::cmp_mmm_ccc(x, 'nng', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nng_dtf <- function(x, ...) {uj::cmp_mmm_ccc(x, 'nng', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nng_gen <- function(x, ...) {uj::cmp_mmm_ccc(x, 'nng', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nng_mat <- function(x, ...) {uj::cmp_mmm_ccc(x, 'nng', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nng_mvc <- function(x, ...) {uj::cmp_mmm_ccc(x, 'nng', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nng_scl <- function(x, ...) {uj::cmp_mmm_ccc(x, 'nng', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nng_vec <- function(x, ...) {uj::cmp_mmm_ccc(x, 'nng', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nng_vls <- function(x, ...) {uj::cmp_mmm_ccc(x, 'nng', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nnw_arr <- function(x, ...) {uj::cmp_mmm_ccc(x, 'nnw', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nnw_dtf <- function(x, ...) {uj::cmp_mmm_ccc(x, 'nnw', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nnw_gen <- function(x, ...) {uj::cmp_mmm_ccc(x, 'nnw', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nnw_mat <- function(x, ...) {uj::cmp_mmm_ccc(x, 'nnw', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nnw_mvc <- function(x, ...) {uj::cmp_mmm_ccc(x, 'nnw', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nnw_scl <- function(x, ...) {uj::cmp_mmm_ccc(x, 'nnw', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nnw_vec <- function(x, ...) {uj::cmp_mmm_ccc(x, 'nnw', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nnw_vls <- function(x, ...) {uj::cmp_mmm_ccc(x, 'nnw', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nps_arr <- function(x, ...) {uj::cmp_mmm_ccc(x, 'nps', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nps_dtf <- function(x, ...) {uj::cmp_mmm_ccc(x, 'nps', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nps_gen <- function(x, ...) {uj::cmp_mmm_ccc(x, 'nps', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nps_mat <- function(x, ...) {uj::cmp_mmm_ccc(x, 'nps', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nps_mvc <- function(x, ...) {uj::cmp_mmm_ccc(x, 'nps', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nps_scl <- function(x, ...) {uj::cmp_mmm_ccc(x, 'nps', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nps_vec <- function(x, ...) {uj::cmp_mmm_ccc(x, 'nps', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nps_vls <- function(x, ...) {uj::cmp_mmm_ccc(x, 'nps', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_npw_arr <- function(x, ...) {uj::cmp_mmm_ccc(x, 'npw', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_npw_dtf <- function(x, ...) {uj::cmp_mmm_ccc(x, 'npw', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_npw_gen <- function(x, ...) {uj::cmp_mmm_ccc(x, 'npw', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_npw_mat <- function(x, ...) {uj::cmp_mmm_ccc(x, 'npw', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_npw_mvc <- function(x, ...) {uj::cmp_mmm_ccc(x, 'npw', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_npw_scl <- function(x, ...) {uj::cmp_mmm_ccc(x, 'npw', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_npw_vec <- function(x, ...) {uj::cmp_mmm_ccc(x, 'npw', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_npw_vls <- function(x, ...) {uj::cmp_mmm_ccc(x, 'npw', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nst_arr <- function(x, ...) {uj::cmp_mmm_ccc(x, 'nst', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nst_dtf <- function(x, ...) {uj::cmp_mmm_ccc(x, 'nst', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nst_gen <- function(x, ...) {uj::cmp_mmm_ccc(x, 'nst', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nst_mat <- function(x, ...) {uj::cmp_mmm_ccc(x, 'nst', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nst_mvc <- function(x, ...) {uj::cmp_mmm_ccc(x, 'nst', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nst_scl <- function(x, ...) {uj::cmp_mmm_ccc(x, 'nst', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nst_vec <- function(x, ...) {uj::cmp_mmm_ccc(x, 'nst', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nst_vls <- function(x, ...) {uj::cmp_mmm_ccc(x, 'nst', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_num_arr <- function(x, ...) {uj::cmp_mmm_ccc(x, 'num', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_num_dtf <- function(x, ...) {uj::cmp_mmm_ccc(x, 'num', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_num_gen <- function(x, ...) {uj::cmp_mmm_ccc(x, 'num', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_num_mat <- function(x, ...) {uj::cmp_mmm_ccc(x, 'num', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_num_mvc <- function(x, ...) {uj::cmp_mmm_ccc(x, 'num', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_num_scl <- function(x, ...) {uj::cmp_mmm_ccc(x, 'num', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_num_vec <- function(x, ...) {uj::cmp_mmm_ccc(x, 'num', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_num_vls <- function(x, ...) {uj::cmp_mmm_ccc(x, 'num', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_odd_arr <- function(x, ...) {uj::cmp_mmm_ccc(x, 'odd', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_odd_dtf <- function(x, ...) {uj::cmp_mmm_ccc(x, 'odd', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_odd_gen <- function(x, ...) {uj::cmp_mmm_ccc(x, 'odd', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_odd_mat <- function(x, ...) {uj::cmp_mmm_ccc(x, 'odd', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_odd_mvc <- function(x, ...) {uj::cmp_mmm_ccc(x, 'odd', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_odd_scl <- function(x, ...) {uj::cmp_mmm_ccc(x, 'odd', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_odd_vec <- function(x, ...) {uj::cmp_mmm_ccc(x, 'odd', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_odd_vls <- function(x, ...) {uj::cmp_mmm_ccc(x, 'odd', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ord_arr <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ord', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ord_dtf <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ord', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ord_gen <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ord', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ord_mat <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ord', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ord_mvc <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ord', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ord_scl <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ord', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ord_vec <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ord', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ord_vls <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ord', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_pct_arr <- function(x, ...) {uj::cmp_mmm_ccc(x, 'pct', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_pct_dtf <- function(x, ...) {uj::cmp_mmm_ccc(x, 'pct', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_pct_gen <- function(x, ...) {uj::cmp_mmm_ccc(x, 'pct', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_pct_mat <- function(x, ...) {uj::cmp_mmm_ccc(x, 'pct', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_pct_mvc <- function(x, ...) {uj::cmp_mmm_ccc(x, 'pct', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_pct_scl <- function(x, ...) {uj::cmp_mmm_ccc(x, 'pct', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_pct_vec <- function(x, ...) {uj::cmp_mmm_ccc(x, 'pct', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_pct_vls <- function(x, ...) {uj::cmp_mmm_ccc(x, 'pct', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_pos_arr <- function(x, ...) {uj::cmp_mmm_ccc(x, 'pos', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_pos_dtf <- function(x, ...) {uj::cmp_mmm_ccc(x, 'pos', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_pos_gen <- function(x, ...) {uj::cmp_mmm_ccc(x, 'pos', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_pos_mat <- function(x, ...) {uj::cmp_mmm_ccc(x, 'pos', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_pos_mvc <- function(x, ...) {uj::cmp_mmm_ccc(x, 'pos', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_pos_scl <- function(x, ...) {uj::cmp_mmm_ccc(x, 'pos', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_pos_vec <- function(x, ...) {uj::cmp_mmm_ccc(x, 'pos', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_pos_vls <- function(x, ...) {uj::cmp_mmm_ccc(x, 'pos', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ppn_arr <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ppn', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ppn_dtf <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ppn', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ppn_gen <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ppn', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ppn_mat <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ppn', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ppn_mvc <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ppn', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ppn_scl <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ppn', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ppn_vec <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ppn', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ppn_vls <- function(x, ...) {uj::cmp_mmm_ccc(x, 'ppn', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_psw_arr <- function(x, ...) {uj::cmp_mmm_ccc(x, 'psw', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_psw_dtf <- function(x, ...) {uj::cmp_mmm_ccc(x, 'psw', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_psw_gen <- function(x, ...) {uj::cmp_mmm_ccc(x, 'psw', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_psw_mat <- function(x, ...) {uj::cmp_mmm_ccc(x, 'psw', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_psw_mvc <- function(x, ...) {uj::cmp_mmm_ccc(x, 'psw', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_psw_scl <- function(x, ...) {uj::cmp_mmm_ccc(x, 'psw', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_psw_vec <- function(x, ...) {uj::cmp_mmm_ccc(x, 'psw', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_psw_vls <- function(x, ...) {uj::cmp_mmm_ccc(x, 'psw', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmpSRTarr <- function(x, ...) {uj::cmp_mmm_ccc(x, 'srt', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmpSRTdtf <- function(x, ...) {uj::cmp_mmm_ccc(x, 'srt', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmpSRTgen <- function(x, ...) {uj::cmp_mmm_ccc(x, 'srt', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmpSRTmat <- function(x, ...) {uj::cmp_mmm_ccc(x, 'srt', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmpSRTmvc <- function(x, ...) {uj::cmp_mmm_ccc(x, 'srt', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_srt_scl <- function(x, ...) {uj::cmp_mmm_ccc(x, 'srt', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmpSRTvec <- function(x, ...) {uj::cmp_mmm_ccc(x, 'srt', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmpSRTvls <- function(x, ...) {uj::cmp_mmm_ccc(x, 'srt', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_str_arr <- function(x, ...) {uj::cmp_mmm_ccc(x, 'str', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_str_dtf <- function(x, ...) {uj::cmp_mmm_ccc(x, 'str', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_str_gen <- function(x, ...) {uj::cmp_mmm_ccc(x, 'str', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_str_mat <- function(x, ...) {uj::cmp_mmm_ccc(x, 'str', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_str_mvc <- function(x, ...) {uj::cmp_mmm_ccc(x, 'str', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_str_scl <- function(x, ...) {uj::cmp_mmm_ccc(x, 'str', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_str_vec <- function(x, ...) {uj::cmp_mmm_ccc(x, 'str', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_str_vls <- function(x, ...) {uj::cmp_mmm_ccc(x, 'str', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_uno_arr <- function(x, ...) {uj::cmp_mmm_ccc(x, 'uno', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_uno_dtf <- function(x, ...) {uj::cmp_mmm_ccc(x, 'uno', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_uno_gen <- function(x, ...) {uj::cmp_mmm_ccc(x, 'uno', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_uno_mat <- function(x, ...) {uj::cmp_mmm_ccc(x, 'uno', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_uno_mvc <- function(x, ...) {uj::cmp_mmm_ccc(x, 'uno', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_uno_scl <- function(x, ...) {uj::cmp_mmm_ccc(x, 'uno', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_uno_vec <- function(x, ...) {uj::cmp_mmm_ccc(x, 'uno', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_uno_vls <- function(x, ...) {uj::cmp_mmm_ccc(x, 'uno', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_whl_arr <- function(x, ...) {uj::cmp_mmm_ccc(x, 'whl', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_whl_dtf <- function(x, ...) {uj::cmp_mmm_ccc(x, 'whl', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_whl_gen <- function(x, ...) {uj::cmp_mmm_ccc(x, 'whl', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_whl_mat <- function(x, ...) {uj::cmp_mmm_ccc(x, 'whl', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_whl_mvc <- function(x, ...) {uj::cmp_mmm_ccc(x, 'whl', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_whl_scl <- function(x, ...) {uj::cmp_mmm_ccc(x, 'whl', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_whl_vec <- function(x, ...) {uj::cmp_mmm_ccc(x, 'whl', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_whl_vls <- function(x, ...) {uj::cmp_mmm_ccc(x, 'whl', 'vls', ...)}
