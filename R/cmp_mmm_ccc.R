#' @encoding UTF-8
#' @family props
#' @title Completeness + xmode + xclass combination properties
#' @description Check for combinations of \link[=CMP]{completeness}, \link[=mmm]{xmode}, and \link[=ccc]{xclass}.
#' @details
#' \tabular{ll}{  `cmp_mmm_ccc_funs`   \tab What complete + xmode + xclass combination property functions are there?                                                                                                                                                          \cr   \tab   \cr
#'                `cmp_{mmm}_{ccc}`    \tab Is `X` both complete and a match to the single xmode property `'{mmm}'` and single xclass property `'{ccc}'` where `{mmm}` and `{ccc}` are placeholders for any given xmode property and any given xclass property, respectively. \cr   \tab   \cr
#'                `cmp_mmm_ccc`        \tab Is `X` both complete and a match to the single xmode and xclass properties in `MMM` and `CCC`, respectively?                                                                                                                                     }
#' @param X An R object.
#' @param mmm A character scalar single xmode property from `mmm_props()`.
#' @param CCC A character scalar single xclass property from `ccc_props()`.
#' @param prop A character scalar.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return **A character scalar** \cr\cr `cmp_mmm_ccc_funs`
#' \cr\cr  **A logical scalar**   \cr\cr `cmp_{mmm}_{ccc}, cmp_mmm_ccc`
#' @examples
#' cmp_mmm_cccFUNS()
#' cmp_mmm_ccc(letters, "ch1", "vec")
#' cmp_mmm_ccc(letters, "str", "scl")
#' cmp_ch1_vec(letters)
#' cmp_str_scl("a")
#' @export
cmp_mmm_ccc <- function(X, MMM, CCC, ...) {
  cfun <- function(cx) {uj::run("uj:::.", base::toupper(CCC), "(cx)")}
  mfun <- function(mx) {
    if      (!base::is.atomic(mx)      ) {F}
    else if (base::length(mx) == 0     ) {F}
    else if (base::any(base::is.na(mx))) {F}
    else {uj::run("uj:::.", base::toupper(MMM), "(mx)")}
  }
  dfun <- function(dx) {
    for (i in 1:base::NCOL(dx)) {if (!mfun(dx[[i]])) {return(F)}}
    T
  }
  vfun <- function(vx) {base::all(base::sapply(vx, mfun))}
  if (base::is.character(MMM)) {MMM <- base::tolower(MMM)}
  if (base::is.character(CCC)) {CCC <- base::tolower(CCC)}
  Errors <- uj:::.meets_errs(X, ...)
  ErrMMM <- "[MMM] is not a scalar value from mmm_props()."
  ErrCCC <- "[CCC] is not a scalar value from ccc_props()."
  if (base::length(MMM) != 1) {Errors <- base::c(Errors, ErrMMM)} else if (!(MMM %in% uj::v(mmm))) {Errors <- base::c(Errors, ErrMMM)}
  if (base::length(CCC) != 1) {Errors <- base::c(Errors, ErrCCC)} else if (!(CCC %in% uj::v(ccc))) {Errors <- base::c(Errors, ErrCCC)}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  if (!uj::meets(X, ...)) {F} else if (!cfun(X)) {F} else if (CCC == "dtf" ) {dfun(X)} else if (CCC == "vls" ) {vfun(X)} else {mfun(X)}
}

#' @rdname cmp_mmm_ccc
#' @export
cmp_mmm_ccc_funs <- function() {
  X <- base::expand.grid(mmm = uj::v(mmm), ccc = uj::v(ccc))
  X <- base::apply(X, 1, base::paste0, collapse = "_")
  X <- base::paste0("cmp_", X)
  base::sort(base::unique(uj::av(X)))
}

#' @rdname cmp_mmm_ccc
#' @export
cmp_atm_arr <- function(X, ...) {uj::cmp_mmm_ccc(X, 'atm', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_atm_dtf <- function(X, ...) {uj::cmp_mmm_ccc(X, 'atm', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_atm_gen <- function(X, ...) {uj::cmp_mmm_ccc(X, 'atm', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_atm_mat <- function(X, ...) {uj::cmp_mmm_ccc(X, 'atm', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_atm_mvc <- function(X, ...) {uj::cmp_mmm_ccc(X, 'atm', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_atm_scl <- function(X, ...) {uj::cmp_mmm_ccc(X, 'atm', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_atm_vec <- function(X, ...) {uj::cmp_mmm_ccc(X, 'atm', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_atm_vls <- function(X, ...) {uj::cmp_mmm_ccc(X, 'atm', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ch1_arr <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ch1', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ch1_dtf <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ch1', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ch1_gen <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ch1', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ch1_mat <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ch1', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ch1_mvc <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ch1', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ch1_scl <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ch1', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ch1_vec <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ch1', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ch1_vls <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ch1', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ch3_arr <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ch3', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ch3_dtf <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ch3', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ch3_gen <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ch3', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ch3_mat <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ch3', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ch3_mvc <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ch3', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ch3_scl <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ch3', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ch3_vec <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ch3', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ch3_vls <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ch3', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_chr_arr <- function(X, ...) {uj::cmp_mmm_ccc(X, 'chr', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_chr_dtf <- function(X, ...) {uj::cmp_mmm_ccc(X, 'chr', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_chr_gen <- function(X, ...) {uj::cmp_mmm_ccc(X, 'chr', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_chr_mat <- function(X, ...) {uj::cmp_mmm_ccc(X, 'chr', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_chr_mvc <- function(X, ...) {uj::cmp_mmm_ccc(X, 'chr', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_chr_scl <- function(X, ...) {uj::cmp_mmm_ccc(X, 'chr', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_chr_vec <- function(X, ...) {uj::cmp_mmm_ccc(X, 'chr', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_chr_vls <- function(X, ...) {uj::cmp_mmm_ccc(X, 'chr', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_clr_arr <- function(X, ...) {uj::cmp_mmm_ccc(X, 'clr', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_clr_dtf <- function(X, ...) {uj::cmp_mmm_ccc(X, 'clr', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_clr_gen <- function(X, ...) {uj::cmp_mmm_ccc(X, 'clr', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_clr_mat <- function(X, ...) {uj::cmp_mmm_ccc(X, 'clr', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_clr_mvc <- function(X, ...) {uj::cmp_mmm_ccc(X, 'clr', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_clr_scl <- function(X, ...) {uj::cmp_mmm_ccc(X, 'clr', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_clr_vec <- function(X, ...) {uj::cmp_mmm_ccc(X, 'clr', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_clr_vls <- function(X, ...) {uj::cmp_mmm_ccc(X, 'clr', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_evn_arr <- function(X, ...) {uj::cmp_mmm_ccc(X, 'evn', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_evn_dtf <- function(X, ...) {uj::cmp_mmm_ccc(X, 'evn', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_evn_gen <- function(X, ...) {uj::cmp_mmm_ccc(X, 'evn', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_evn_mat <- function(X, ...) {uj::cmp_mmm_ccc(X, 'evn', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_evn_mvc <- function(X, ...) {uj::cmp_mmm_ccc(X, 'evn', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_evn_scl <- function(X, ...) {uj::cmp_mmm_ccc(X, 'evn', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_evn_vec <- function(X, ...) {uj::cmp_mmm_ccc(X, 'evn', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_evn_vls <- function(X, ...) {uj::cmp_mmm_ccc(X, 'evn', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_fac_arr <- function(X, ...) {uj::cmp_mmm_ccc(X, 'fac', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_fac_dtf <- function(X, ...) {uj::cmp_mmm_ccc(X, 'fac', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_fac_gen <- function(X, ...) {uj::cmp_mmm_ccc(X, 'fac', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_fac_mat <- function(X, ...) {uj::cmp_mmm_ccc(X, 'fac', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_fac_mvc <- function(X, ...) {uj::cmp_mmm_ccc(X, 'fac', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_fac_scl <- function(X, ...) {uj::cmp_mmm_ccc(X, 'fac', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_fac_vec <- function(X, ...) {uj::cmp_mmm_ccc(X, 'fac', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_fac_vls <- function(X, ...) {uj::cmp_mmm_ccc(X, 'fac', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_frc_arr <- function(X, ...) {uj::cmp_mmm_ccc(X, 'frc', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_frc_dtf <- function(X, ...) {uj::cmp_mmm_ccc(X, 'frc', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_frc_gen <- function(X, ...) {uj::cmp_mmm_ccc(X, 'frc', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_frc_mat <- function(X, ...) {uj::cmp_mmm_ccc(X, 'frc', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_frc_mvc <- function(X, ...) {uj::cmp_mmm_ccc(X, 'frc', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_frc_scl <- function(X, ...) {uj::cmp_mmm_ccc(X, 'frc', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_frc_vec <- function(X, ...) {uj::cmp_mmm_ccc(X, 'frc', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_frc_vls <- function(X, ...) {uj::cmp_mmm_ccc(X, 'frc', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ind_arr <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ind', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ind_dtf <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ind', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ind_gen <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ind', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ind_mat <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ind', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ind_mvc <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ind', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ind_scl <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ind', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ind_vec <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ind', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ind_vls <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ind', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_lgl_arr <- function(X, ...) {uj::cmp_mmm_ccc(X, 'lgl', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_lgl_dtf <- function(X, ...) {uj::cmp_mmm_ccc(X, 'lgl', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_lgl_gen <- function(X, ...) {uj::cmp_mmm_ccc(X, 'lgl', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_lgl_mat <- function(X, ...) {uj::cmp_mmm_ccc(X, 'lgl', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_lgl_mvc <- function(X, ...) {uj::cmp_mmm_ccc(X, 'lgl', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_lgl_scl <- function(X, ...) {uj::cmp_mmm_ccc(X, 'lgl', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_lgl_vec <- function(X, ...) {uj::cmp_mmm_ccc(X, 'lgl', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_lgl_vls <- function(X, ...) {uj::cmp_mmm_ccc(X, 'lgl', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_neg_arr <- function(X, ...) {uj::cmp_mmm_ccc(X, 'neg', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_neg_dtf <- function(X, ...) {uj::cmp_mmm_ccc(X, 'neg', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_neg_gen <- function(X, ...) {uj::cmp_mmm_ccc(X, 'neg', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_neg_mat <- function(X, ...) {uj::cmp_mmm_ccc(X, 'neg', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_neg_mvc <- function(X, ...) {uj::cmp_mmm_ccc(X, 'neg', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_neg_scl <- function(X, ...) {uj::cmp_mmm_ccc(X, 'neg', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_neg_vec <- function(X, ...) {uj::cmp_mmm_ccc(X, 'neg', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_neg_vls <- function(X, ...) {uj::cmp_mmm_ccc(X, 'neg', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ngw_arr <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ngw', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ngw_dtf <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ngw', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ngw_gen <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ngw', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ngw_mat <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ngw', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ngw_mvc <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ngw', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ngw_scl <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ngw', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ngw_vec <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ngw', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ngw_vls <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ngw', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nng_arr <- function(X, ...) {uj::cmp_mmm_ccc(X, 'nng', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nng_dtf <- function(X, ...) {uj::cmp_mmm_ccc(X, 'nng', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nng_gen <- function(X, ...) {uj::cmp_mmm_ccc(X, 'nng', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nng_mat <- function(X, ...) {uj::cmp_mmm_ccc(X, 'nng', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nng_mvc <- function(X, ...) {uj::cmp_mmm_ccc(X, 'nng', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nng_scl <- function(X, ...) {uj::cmp_mmm_ccc(X, 'nng', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nng_vec <- function(X, ...) {uj::cmp_mmm_ccc(X, 'nng', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nng_vls <- function(X, ...) {uj::cmp_mmm_ccc(X, 'nng', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nnw_arr <- function(X, ...) {uj::cmp_mmm_ccc(X, 'nnw', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nnw_dtf <- function(X, ...) {uj::cmp_mmm_ccc(X, 'nnw', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nnw_gen <- function(X, ...) {uj::cmp_mmm_ccc(X, 'nnw', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nnw_mat <- function(X, ...) {uj::cmp_mmm_ccc(X, 'nnw', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nnw_mvc <- function(X, ...) {uj::cmp_mmm_ccc(X, 'nnw', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nnw_scl <- function(X, ...) {uj::cmp_mmm_ccc(X, 'nnw', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nnw_vec <- function(X, ...) {uj::cmp_mmm_ccc(X, 'nnw', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nnw_vls <- function(X, ...) {uj::cmp_mmm_ccc(X, 'nnw', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nps_arr <- function(X, ...) {uj::cmp_mmm_ccc(X, 'nps', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nps_dtf <- function(X, ...) {uj::cmp_mmm_ccc(X, 'nps', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nps_gen <- function(X, ...) {uj::cmp_mmm_ccc(X, 'nps', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nps_mat <- function(X, ...) {uj::cmp_mmm_ccc(X, 'nps', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nps_mvc <- function(X, ...) {uj::cmp_mmm_ccc(X, 'nps', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nps_scl <- function(X, ...) {uj::cmp_mmm_ccc(X, 'nps', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nps_vec <- function(X, ...) {uj::cmp_mmm_ccc(X, 'nps', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nps_vls <- function(X, ...) {uj::cmp_mmm_ccc(X, 'nps', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_npw_arr <- function(X, ...) {uj::cmp_mmm_ccc(X, 'npw', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_npw_dtf <- function(X, ...) {uj::cmp_mmm_ccc(X, 'npw', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_npw_gen <- function(X, ...) {uj::cmp_mmm_ccc(X, 'npw', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_npw_mat <- function(X, ...) {uj::cmp_mmm_ccc(X, 'npw', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_npw_mvc <- function(X, ...) {uj::cmp_mmm_ccc(X, 'npw', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_npw_scl <- function(X, ...) {uj::cmp_mmm_ccc(X, 'npw', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_npw_vec <- function(X, ...) {uj::cmp_mmm_ccc(X, 'npw', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_npw_vls <- function(X, ...) {uj::cmp_mmm_ccc(X, 'npw', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nst_arr <- function(X, ...) {uj::cmp_mmm_ccc(X, 'nst', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nst_dtf <- function(X, ...) {uj::cmp_mmm_ccc(X, 'nst', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nst_gen <- function(X, ...) {uj::cmp_mmm_ccc(X, 'nst', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nst_mat <- function(X, ...) {uj::cmp_mmm_ccc(X, 'nst', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nst_mvc <- function(X, ...) {uj::cmp_mmm_ccc(X, 'nst', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nst_scl <- function(X, ...) {uj::cmp_mmm_ccc(X, 'nst', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nst_vec <- function(X, ...) {uj::cmp_mmm_ccc(X, 'nst', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_nst_vls <- function(X, ...) {uj::cmp_mmm_ccc(X, 'nst', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_num_arr <- function(X, ...) {uj::cmp_mmm_ccc(X, 'num', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_num_dtf <- function(X, ...) {uj::cmp_mmm_ccc(X, 'num', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_num_gen <- function(X, ...) {uj::cmp_mmm_ccc(X, 'num', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_num_mat <- function(X, ...) {uj::cmp_mmm_ccc(X, 'num', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_num_mvc <- function(X, ...) {uj::cmp_mmm_ccc(X, 'num', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_num_scl <- function(X, ...) {uj::cmp_mmm_ccc(X, 'num', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_num_vec <- function(X, ...) {uj::cmp_mmm_ccc(X, 'num', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_num_vls <- function(X, ...) {uj::cmp_mmm_ccc(X, 'num', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_odd_arr <- function(X, ...) {uj::cmp_mmm_ccc(X, 'odd', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_odd_dtf <- function(X, ...) {uj::cmp_mmm_ccc(X, 'odd', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_odd_gen <- function(X, ...) {uj::cmp_mmm_ccc(X, 'odd', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_odd_mat <- function(X, ...) {uj::cmp_mmm_ccc(X, 'odd', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_odd_mvc <- function(X, ...) {uj::cmp_mmm_ccc(X, 'odd', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_odd_scl <- function(X, ...) {uj::cmp_mmm_ccc(X, 'odd', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_odd_vec <- function(X, ...) {uj::cmp_mmm_ccc(X, 'odd', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_odd_vls <- function(X, ...) {uj::cmp_mmm_ccc(X, 'odd', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ord_arr <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ord', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ord_dtf <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ord', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ord_gen <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ord', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ord_mat <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ord', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ord_mvc <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ord', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ord_scl <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ord', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ord_vec <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ord', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ord_vls <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ord', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_pct_arr <- function(X, ...) {uj::cmp_mmm_ccc(X, 'pct', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_pct_dtf <- function(X, ...) {uj::cmp_mmm_ccc(X, 'pct', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_pct_gen <- function(X, ...) {uj::cmp_mmm_ccc(X, 'pct', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_pct_mat <- function(X, ...) {uj::cmp_mmm_ccc(X, 'pct', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_pct_mvc <- function(X, ...) {uj::cmp_mmm_ccc(X, 'pct', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_pct_scl <- function(X, ...) {uj::cmp_mmm_ccc(X, 'pct', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_pct_vec <- function(X, ...) {uj::cmp_mmm_ccc(X, 'pct', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_pct_vls <- function(X, ...) {uj::cmp_mmm_ccc(X, 'pct', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_pos_arr <- function(X, ...) {uj::cmp_mmm_ccc(X, 'pos', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_pos_dtf <- function(X, ...) {uj::cmp_mmm_ccc(X, 'pos', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_pos_gen <- function(X, ...) {uj::cmp_mmm_ccc(X, 'pos', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_pos_mat <- function(X, ...) {uj::cmp_mmm_ccc(X, 'pos', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_pos_mvc <- function(X, ...) {uj::cmp_mmm_ccc(X, 'pos', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_pos_scl <- function(X, ...) {uj::cmp_mmm_ccc(X, 'pos', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_pos_vec <- function(X, ...) {uj::cmp_mmm_ccc(X, 'pos', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_pos_vls <- function(X, ...) {uj::cmp_mmm_ccc(X, 'pos', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ppn_arr <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ppn', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ppn_dtf <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ppn', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ppn_gen <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ppn', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ppn_mat <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ppn', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ppn_mvc <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ppn', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ppn_scl <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ppn', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ppn_vec <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ppn', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_ppn_vls <- function(X, ...) {uj::cmp_mmm_ccc(X, 'ppn', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_psw_arr <- function(X, ...) {uj::cmp_mmm_ccc(X, 'psw', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_psw_dtf <- function(X, ...) {uj::cmp_mmm_ccc(X, 'psw', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_psw_gen <- function(X, ...) {uj::cmp_mmm_ccc(X, 'psw', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_psw_mat <- function(X, ...) {uj::cmp_mmm_ccc(X, 'psw', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_psw_mvc <- function(X, ...) {uj::cmp_mmm_ccc(X, 'psw', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_psw_scl <- function(X, ...) {uj::cmp_mmm_ccc(X, 'psw', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_psw_vec <- function(X, ...) {uj::cmp_mmm_ccc(X, 'psw', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_psw_vls <- function(X, ...) {uj::cmp_mmm_ccc(X, 'psw', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmpSRTarr <- function(X, ...) {uj::cmp_mmm_ccc(X, 'srt', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmpSRTdtf <- function(X, ...) {uj::cmp_mmm_ccc(X, 'srt', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmpSRTgen <- function(X, ...) {uj::cmp_mmm_ccc(X, 'srt', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmpSRTmat <- function(X, ...) {uj::cmp_mmm_ccc(X, 'srt', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmpSRTmvc <- function(X, ...) {uj::cmp_mmm_ccc(X, 'srt', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_srt_scl <- function(X, ...) {uj::cmp_mmm_ccc(X, 'srt', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmpSRTvec <- function(X, ...) {uj::cmp_mmm_ccc(X, 'srt', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmpSRTvls <- function(X, ...) {uj::cmp_mmm_ccc(X, 'srt', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_str_arr <- function(X, ...) {uj::cmp_mmm_ccc(X, 'str', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_str_dtf <- function(X, ...) {uj::cmp_mmm_ccc(X, 'str', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_str_gen <- function(X, ...) {uj::cmp_mmm_ccc(X, 'str', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_str_mat <- function(X, ...) {uj::cmp_mmm_ccc(X, 'str', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_str_mvc <- function(X, ...) {uj::cmp_mmm_ccc(X, 'str', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_str_scl <- function(X, ...) {uj::cmp_mmm_ccc(X, 'str', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_str_vec <- function(X, ...) {uj::cmp_mmm_ccc(X, 'str', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_str_vls <- function(X, ...) {uj::cmp_mmm_ccc(X, 'str', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_uno_arr <- function(X, ...) {uj::cmp_mmm_ccc(X, 'uno', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_uno_dtf <- function(X, ...) {uj::cmp_mmm_ccc(X, 'uno', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_uno_gen <- function(X, ...) {uj::cmp_mmm_ccc(X, 'uno', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_uno_mat <- function(X, ...) {uj::cmp_mmm_ccc(X, 'uno', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_uno_mvc <- function(X, ...) {uj::cmp_mmm_ccc(X, 'uno', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_uno_scl <- function(X, ...) {uj::cmp_mmm_ccc(X, 'uno', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_uno_vec <- function(X, ...) {uj::cmp_mmm_ccc(X, 'uno', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_uno_vls <- function(X, ...) {uj::cmp_mmm_ccc(X, 'uno', 'vls', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_whl_arr <- function(X, ...) {uj::cmp_mmm_ccc(X, 'whl', 'arr', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_whl_dtf <- function(X, ...) {uj::cmp_mmm_ccc(X, 'whl', 'dtf', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_whl_gen <- function(X, ...) {uj::cmp_mmm_ccc(X, 'whl', 'gen', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_whl_mat <- function(X, ...) {uj::cmp_mmm_ccc(X, 'whl', 'mat', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_whl_mvc <- function(X, ...) {uj::cmp_mmm_ccc(X, 'whl', 'mvc', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_whl_scl <- function(X, ...) {uj::cmp_mmm_ccc(X, 'whl', 'scl', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_whl_vec <- function(X, ...) {uj::cmp_mmm_ccc(X, 'whl', 'vec', ...)}

#' @rdname cmp_mmm_ccc
#' @export
cmp_whl_vls <- function(X, ...) {uj::cmp_mmm_ccc(X, 'whl', 'vls', ...)}
