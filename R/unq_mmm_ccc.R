#' @encoding UTF-8
#' @family props
#' @title unique + xmode + xclass combination properties
#' @description Functions for checking combinations of \link[=UNQ]{uniqueness}, \link[=mmm]{xmode}, and \link[=ccc]{xclass}.
#' @details
#' \tabular{ll}{  `unq_mmm_ccc_funs`   \tab What \link[=iunq]{unique} + \link[=mmm]{xmode} + \link[=ccc]{xclass} combination \link[=prop_funs]{property functions} are there? \cr   \tab   \cr
#'                `unq_{mmm}_{ccc}`    \tab Is `X` both unique and a match to the single xmode property `'{mmm}'` and single xclass property `'{ccc}'`? (where `{mmm}` and
#'                                          `{ccc}` are placeholders for any given xmode property and any given xclass property, respectively).                               \cr   \tab   \cr
#'                `unq_mmm_ccc`        \tab Is `X` both unique and a match to the single xmode and xclass properties in arguments `MMM` and `CCC`, respectively?                             }
#' @param X An R object.
#' @param MMM A character scalar single xmode property from \code{\link{mmm_props}()}.
#' @param CCC A character scalar single xclass property from \code{\link{ccc_props}()}.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return **A character vector** \cr\cr `unq_mmm_ccc_funs`
#' \cr\cr  **A logical scalar**   \cr\cr `unq_{mmm}_{ccc}` \cr `unq_mmm_ccc`
#' @examples
#' unq_mmm_ccc_funs()
#' unq_mmm_ccc(letters, "ch1", "vec")
#' unq_mmm_ccc(letters, "str", "scl")
#' unq_ch1_vec(letters)
#' unq_str_scl("a")
#' @export
unq_mmm_ccc <- function(X, MMM, CCC, ...) {
  if (uj::cmp_mmm_ccc(X, MMM, CCC, ...)) {
    if      (CCC == "dtf") {base::nrow(X) == base::nrow(base::unique(X))}
    else if (CCC == "vls") {
      N <- base::lengths(X)
      NU <- base::sapply(X, function(x) {base::length(base::unique(x))})
      base::all(N == NU)
    } else {base::length(X) == base::length(base::unique(X))}
  } else {F}
}

#' @rdname unq_mmm_ccc
#' @export
unq_mmm_ccc_funs <- function() {
  X <- base::expand.grid(mmm = uj::v(mmm), ccc = uj::v(ccc))
  X <- base::apply(X, 1, paste0, collapse = "_")
  base::paste0('unq_', base::sort(uj::av(X)))
}

#' @rdname unq_mmm_ccc
#' @export
unq_atm_arr <- function(X, ...) {uj::unq_mmm_ccc(X, 'atm', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_atm_dtf <- function(X, ...) {uj::unq_mmm_ccc(X, 'atm', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_atm_gen <- function(X, ...) {uj::unq_mmm_ccc(X, 'atm', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_atm_mat <- function(X, ...) {uj::unq_mmm_ccc(X, 'atm', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_atm_mvc <- function(X, ...) {uj::unq_mmm_ccc(X, 'atm', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_atm_scl <- function(X, ...) {uj::unq_mmm_ccc(X, 'atm', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_atm_vec <- function(X, ...) {uj::unq_mmm_ccc(X, 'atm', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_atm_vls <- function(X, ...) {uj::unq_mmm_ccc(X, 'atm', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ch1_arr <- function(X, ...) {uj::unq_mmm_ccc(X, 'ch1', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ch1_dtf <- function(X, ...) {uj::unq_mmm_ccc(X, 'ch1', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ch1_gen <- function(X, ...) {uj::unq_mmm_ccc(X, 'ch1', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ch1_mat <- function(X, ...) {uj::unq_mmm_ccc(X, 'ch1', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ch1_mvc <- function(X, ...) {uj::unq_mmm_ccc(X, 'ch1', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ch1_scl <- function(X, ...) {uj::unq_mmm_ccc(X, 'ch1', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ch1_vec <- function(X, ...) {uj::unq_mmm_ccc(X, 'ch1', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ch1_vls <- function(X, ...) {uj::unq_mmm_ccc(X, 'ch1', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ch3_arr <- function(X, ...) {uj::unq_mmm_ccc(X, 'ch3', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ch3_dtf <- function(X, ...) {uj::unq_mmm_ccc(X, 'ch3', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ch3_gen <- function(X, ...) {uj::unq_mmm_ccc(X, 'ch3', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ch3_mat <- function(X, ...) {uj::unq_mmm_ccc(X, 'ch3', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ch3_mvc <- function(X, ...) {uj::unq_mmm_ccc(X, 'ch3', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ch3_scl <- function(X, ...) {uj::unq_mmm_ccc(X, 'ch3', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ch3_vec <- function(X, ...) {uj::unq_mmm_ccc(X, 'ch3', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ch3_vls <- function(X, ...) {uj::unq_mmm_ccc(X, 'ch3', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_chr_arr <- function(X, ...) {uj::unq_mmm_ccc(X, 'chr', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_chr_dtf <- function(X, ...) {uj::unq_mmm_ccc(X, 'chr', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_chr_gen <- function(X, ...) {uj::unq_mmm_ccc(X, 'chr', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_chr_mat <- function(X, ...) {uj::unq_mmm_ccc(X, 'chr', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_chr_mvc <- function(X, ...) {uj::unq_mmm_ccc(X, 'chr', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_chr_scl <- function(X, ...) {uj::unq_mmm_ccc(X, 'chr', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_chr_vec <- function(X, ...) {uj::unq_mmm_ccc(X, 'chr', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_chr_vls <- function(X, ...) {uj::unq_mmm_ccc(X, 'chr', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_clr_arr <- function(X, ...) {uj::unq_mmm_ccc(X, 'clr', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_clr_dtf <- function(X, ...) {uj::unq_mmm_ccc(X, 'clr', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_clr_gen <- function(X, ...) {uj::unq_mmm_ccc(X, 'clr', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_clr_mat <- function(X, ...) {uj::unq_mmm_ccc(X, 'clr', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_clr_mvc <- function(X, ...) {uj::unq_mmm_ccc(X, 'clr', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_clr_scl <- function(X, ...) {uj::unq_mmm_ccc(X, 'clr', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_clr_vec <- function(X, ...) {uj::unq_mmm_ccc(X, 'clr', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_clr_vls <- function(X, ...) {uj::unq_mmm_ccc(X, 'clr', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_evn_arr <- function(X, ...) {uj::unq_mmm_ccc(X, 'evn', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_evn_dtf <- function(X, ...) {uj::unq_mmm_ccc(X, 'evn', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_evn_gen <- function(X, ...) {uj::unq_mmm_ccc(X, 'evn', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_evn_mat <- function(X, ...) {uj::unq_mmm_ccc(X, 'evn', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_evn_mvc <- function(X, ...) {uj::unq_mmm_ccc(X, 'evn', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_evn_scl <- function(X, ...) {uj::unq_mmm_ccc(X, 'evn', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_evn_vec <- function(X, ...) {uj::unq_mmm_ccc(X, 'evn', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_evn_vls <- function(X, ...) {uj::unq_mmm_ccc(X, 'evn', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_fac_arr <- function(X, ...) {uj::unq_mmm_ccc(X, 'fac', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_fac_dtf <- function(X, ...) {uj::unq_mmm_ccc(X, 'fac', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_fac_gen <- function(X, ...) {uj::unq_mmm_ccc(X, 'fac', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_fac_mat <- function(X, ...) {uj::unq_mmm_ccc(X, 'fac', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_fac_mvc <- function(X, ...) {uj::unq_mmm_ccc(X, 'fac', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_fac_scl <- function(X, ...) {uj::unq_mmm_ccc(X, 'fac', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_fac_vec <- function(X, ...) {uj::unq_mmm_ccc(X, 'fac', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_fac_vls <- function(X, ...) {uj::unq_mmm_ccc(X, 'fac', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_frc_arr <- function(X, ...) {uj::unq_mmm_ccc(X, 'frc', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_frc_dtf <- function(X, ...) {uj::unq_mmm_ccc(X, 'frc', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_frc_gen <- function(X, ...) {uj::unq_mmm_ccc(X, 'frc', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_frc_mat <- function(X, ...) {uj::unq_mmm_ccc(X, 'frc', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_frc_mvc <- function(X, ...) {uj::unq_mmm_ccc(X, 'frc', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_frc_scl <- function(X, ...) {uj::unq_mmm_ccc(X, 'frc', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_frc_vec <- function(X, ...) {uj::unq_mmm_ccc(X, 'frc', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_frc_vls <- function(X, ...) {uj::unq_mmm_ccc(X, 'frc', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ind_arr <- function(X, ...) {uj::unq_mmm_ccc(X, 'ind', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ind_dtf <- function(X, ...) {uj::unq_mmm_ccc(X, 'ind', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ind_gen <- function(X, ...) {uj::unq_mmm_ccc(X, 'ind', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ind_mat <- function(X, ...) {uj::unq_mmm_ccc(X, 'ind', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ind_mvc <- function(X, ...) {uj::unq_mmm_ccc(X, 'ind', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ind_scl <- function(X, ...) {uj::unq_mmm_ccc(X, 'ind', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ind_vec <- function(X, ...) {uj::unq_mmm_ccc(X, 'ind', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ind_vls <- function(X, ...) {uj::unq_mmm_ccc(X, 'ind', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_lgl_arr <- function(X, ...) {uj::unq_mmm_ccc(X, 'lgl', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_lgl_dtf <- function(X, ...) {uj::unq_mmm_ccc(X, 'lgl', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_lgl_gen <- function(X, ...) {uj::unq_mmm_ccc(X, 'lgl', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_lgl_mat <- function(X, ...) {uj::unq_mmm_ccc(X, 'lgl', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_lgl_mvc <- function(X, ...) {uj::unq_mmm_ccc(X, 'lgl', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_lgl_scl <- function(X, ...) {uj::unq_mmm_ccc(X, 'lgl', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_lgl_vec <- function(X, ...) {uj::unq_mmm_ccc(X, 'lgl', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_lgl_vls <- function(X, ...) {uj::unq_mmm_ccc(X, 'lgl', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_neg_arr <- function(X, ...) {uj::unq_mmm_ccc(X, 'neg', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_neg_dtf <- function(X, ...) {uj::unq_mmm_ccc(X, 'neg', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_neg_gen <- function(X, ...) {uj::unq_mmm_ccc(X, 'neg', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_neg_mat <- function(X, ...) {uj::unq_mmm_ccc(X, 'neg', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_neg_mvc <- function(X, ...) {uj::unq_mmm_ccc(X, 'neg', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_neg_scl <- function(X, ...) {uj::unq_mmm_ccc(X, 'neg', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_neg_vec <- function(X, ...) {uj::unq_mmm_ccc(X, 'neg', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_neg_vls <- function(X, ...) {uj::unq_mmm_ccc(X, 'neg', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ngw_arr <- function(X, ...) {uj::unq_mmm_ccc(X, 'ngw', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ngw_dtf <- function(X, ...) {uj::unq_mmm_ccc(X, 'ngw', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ngw_gen <- function(X, ...) {uj::unq_mmm_ccc(X, 'ngw', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ngw_mat <- function(X, ...) {uj::unq_mmm_ccc(X, 'ngw', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ngw_mvc <- function(X, ...) {uj::unq_mmm_ccc(X, 'ngw', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ngw_scl <- function(X, ...) {uj::unq_mmm_ccc(X, 'ngw', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ngw_vec <- function(X, ...) {uj::unq_mmm_ccc(X, 'ngw', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ngw_vls <- function(X, ...) {uj::unq_mmm_ccc(X, 'ngw', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nng_arr <- function(X, ...) {uj::unq_mmm_ccc(X, 'nng', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nng_dtf <- function(X, ...) {uj::unq_mmm_ccc(X, 'nng', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nng_gen <- function(X, ...) {uj::unq_mmm_ccc(X, 'nng', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nng_mat <- function(X, ...) {uj::unq_mmm_ccc(X, 'nng', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nng_mvc <- function(X, ...) {uj::unq_mmm_ccc(X, 'nng', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nng_scl <- function(X, ...) {uj::unq_mmm_ccc(X, 'nng', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nng_vec <- function(X, ...) {uj::unq_mmm_ccc(X, 'nng', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nng_vls <- function(X, ...) {uj::unq_mmm_ccc(X, 'nng', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nnw_arr <- function(X, ...) {uj::unq_mmm_ccc(X, 'nnw', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nnw_dtf <- function(X, ...) {uj::unq_mmm_ccc(X, 'nnw', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nnw_gen <- function(X, ...) {uj::unq_mmm_ccc(X, 'nnw', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nnw_mat <- function(X, ...) {uj::unq_mmm_ccc(X, 'nnw', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nnw_mvc <- function(X, ...) {uj::unq_mmm_ccc(X, 'nnw', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nnw_scl <- function(X, ...) {uj::unq_mmm_ccc(X, 'nnw', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nnw_vec <- function(X, ...) {uj::unq_mmm_ccc(X, 'nnw', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nnw_vls <- function(X, ...) {uj::unq_mmm_ccc(X, 'nnw', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nps_arr <- function(X, ...) {uj::unq_mmm_ccc(X, 'nps', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nps_dtf <- function(X, ...) {uj::unq_mmm_ccc(X, 'nps', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nps_gen <- function(X, ...) {uj::unq_mmm_ccc(X, 'nps', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nps_mat <- function(X, ...) {uj::unq_mmm_ccc(X, 'nps', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nps_mvc <- function(X, ...) {uj::unq_mmm_ccc(X, 'nps', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nps_scl <- function(X, ...) {uj::unq_mmm_ccc(X, 'nps', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nps_vec <- function(X, ...) {uj::unq_mmm_ccc(X, 'nps', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nps_vls <- function(X, ...) {uj::unq_mmm_ccc(X, 'nps', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_npw_arr <- function(X, ...) {uj::unq_mmm_ccc(X, 'npw', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_npw_dtf <- function(X, ...) {uj::unq_mmm_ccc(X, 'npw', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_npw_gen <- function(X, ...) {uj::unq_mmm_ccc(X, 'npw', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_npw_mat <- function(X, ...) {uj::unq_mmm_ccc(X, 'npw', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_npw_mvc <- function(X, ...) {uj::unq_mmm_ccc(X, 'npw', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_npw_scl <- function(X, ...) {uj::unq_mmm_ccc(X, 'npw', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_npw_vec <- function(X, ...) {uj::unq_mmm_ccc(X, 'npw', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_npw_vls <- function(X, ...) {uj::unq_mmm_ccc(X, 'npw', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nst_arr <- function(X, ...) {uj::unq_mmm_ccc(X, 'nst', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nst_dtf <- function(X, ...) {uj::unq_mmm_ccc(X, 'nst', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nst_gen <- function(X, ...) {uj::unq_mmm_ccc(X, 'nst', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nst_mat <- function(X, ...) {uj::unq_mmm_ccc(X, 'nst', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nst_mvc <- function(X, ...) {uj::unq_mmm_ccc(X, 'nst', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nst_scl <- function(X, ...) {uj::unq_mmm_ccc(X, 'nst', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nst_vec <- function(X, ...) {uj::unq_mmm_ccc(X, 'nst', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nst_vls <- function(X, ...) {uj::unq_mmm_ccc(X, 'nst', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_num_arr <- function(X, ...) {uj::unq_mmm_ccc(X, 'num', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_num_dtf <- function(X, ...) {uj::unq_mmm_ccc(X, 'num', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_num_gen <- function(X, ...) {uj::unq_mmm_ccc(X, 'num', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_num_mat <- function(X, ...) {uj::unq_mmm_ccc(X, 'num', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_num_mvc <- function(X, ...) {uj::unq_mmm_ccc(X, 'num', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_num_scl <- function(X, ...) {uj::unq_mmm_ccc(X, 'num', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_num_vec <- function(X, ...) {uj::unq_mmm_ccc(X, 'num', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_num_vls <- function(X, ...) {uj::unq_mmm_ccc(X, 'num', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_odd_arr <- function(X, ...) {uj::unq_mmm_ccc(X, 'odd', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_odd_dtf <- function(X, ...) {uj::unq_mmm_ccc(X, 'odd', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_odd_gen <- function(X, ...) {uj::unq_mmm_ccc(X, 'odd', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_odd_mat <- function(X, ...) {uj::unq_mmm_ccc(X, 'odd', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_odd_mvc <- function(X, ...) {uj::unq_mmm_ccc(X, 'odd', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_odd_scl <- function(X, ...) {uj::unq_mmm_ccc(X, 'odd', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_odd_vec <- function(X, ...) {uj::unq_mmm_ccc(X, 'odd', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_odd_vls <- function(X, ...) {uj::unq_mmm_ccc(X, 'odd', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ord_arr <- function(X, ...) {uj::unq_mmm_ccc(X, 'ord', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ord_dtf <- function(X, ...) {uj::unq_mmm_ccc(X, 'ord', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ord_gen <- function(X, ...) {uj::unq_mmm_ccc(X, 'ord', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ord_mat <- function(X, ...) {uj::unq_mmm_ccc(X, 'ord', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ord_mvc <- function(X, ...) {uj::unq_mmm_ccc(X, 'ord', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ord_scl <- function(X, ...) {uj::unq_mmm_ccc(X, 'ord', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ord_vec <- function(X, ...) {uj::unq_mmm_ccc(X, 'ord', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ord_vls <- function(X, ...) {uj::unq_mmm_ccc(X, 'ord', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_pct_arr <- function(X, ...) {uj::unq_mmm_ccc(X, 'pct', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_pct_dtf <- function(X, ...) {uj::unq_mmm_ccc(X, 'pct', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_pct_gen <- function(X, ...) {uj::unq_mmm_ccc(X, 'pct', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_pct_mat <- function(X, ...) {uj::unq_mmm_ccc(X, 'pct', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_pct_mvc <- function(X, ...) {uj::unq_mmm_ccc(X, 'pct', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_pct_scl <- function(X, ...) {uj::unq_mmm_ccc(X, 'pct', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_pct_vec <- function(X, ...) {uj::unq_mmm_ccc(X, 'pct', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_pct_vls <- function(X, ...) {uj::unq_mmm_ccc(X, 'pct', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_pos_arr <- function(X, ...) {uj::unq_mmm_ccc(X, 'pos', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_pos_dtf <- function(X, ...) {uj::unq_mmm_ccc(X, 'pos', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_pos_gen <- function(X, ...) {uj::unq_mmm_ccc(X, 'pos', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_pos_mat <- function(X, ...) {uj::unq_mmm_ccc(X, 'pos', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_pos_mvc <- function(X, ...) {uj::unq_mmm_ccc(X, 'pos', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_pos_scl <- function(X, ...) {uj::unq_mmm_ccc(X, 'pos', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_pos_vec <- function(X, ...) {uj::unq_mmm_ccc(X, 'pos', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_pos_vls <- function(X, ...) {uj::unq_mmm_ccc(X, 'pos', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ppn_arr <- function(X, ...) {uj::unq_mmm_ccc(X, 'ppn', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ppn_dtf <- function(X, ...) {uj::unq_mmm_ccc(X, 'ppn', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ppn_gen <- function(X, ...) {uj::unq_mmm_ccc(X, 'ppn', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ppn_mat <- function(X, ...) {uj::unq_mmm_ccc(X, 'ppn', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ppn_mvc <- function(X, ...) {uj::unq_mmm_ccc(X, 'ppn', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ppn_scl <- function(X, ...) {uj::unq_mmm_ccc(X, 'ppn', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ppn_vec <- function(X, ...) {uj::unq_mmm_ccc(X, 'ppn', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ppn_vls <- function(X, ...) {uj::unq_mmm_ccc(X, 'ppn', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_psw_arr <- function(X, ...) {uj::unq_mmm_ccc(X, 'psw', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_psw_dtf <- function(X, ...) {uj::unq_mmm_ccc(X, 'psw', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_psw_gen <- function(X, ...) {uj::unq_mmm_ccc(X, 'psw', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_psw_mat <- function(X, ...) {uj::unq_mmm_ccc(X, 'psw', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_psw_mvc <- function(X, ...) {uj::unq_mmm_ccc(X, 'psw', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_psw_scl <- function(X, ...) {uj::unq_mmm_ccc(X, 'psw', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_psw_vec <- function(X, ...) {uj::unq_mmm_ccc(X, 'psw', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_psw_vls <- function(X, ...) {uj::unq_mmm_ccc(X, 'psw', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_srt_arr <- function(X, ...) {uj::unq_mmm_ccc(X, 'srt', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_srt_dtf <- function(X, ...) {uj::unq_mmm_ccc(X, 'srt', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_srt_gen <- function(X, ...) {uj::unq_mmm_ccc(X, 'srt', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_srt_mat <- function(X, ...) {uj::unq_mmm_ccc(X, 'srt', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_srt_mvc <- function(X, ...) {uj::unq_mmm_ccc(X, 'srt', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_srt_scl <- function(X, ...) {uj::unq_mmm_ccc(X, 'srt', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_srt_vec <- function(X, ...) {uj::unq_mmm_ccc(X, 'srt', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_srt_vls <- function(X, ...) {uj::unq_mmm_ccc(X, 'srt', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_str_arr <- function(X, ...) {uj::unq_mmm_ccc(X, 'str', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_str_dtf <- function(X, ...) {uj::unq_mmm_ccc(X, 'str', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_str_gen <- function(X, ...) {uj::unq_mmm_ccc(X, 'str', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_str_mat <- function(X, ...) {uj::unq_mmm_ccc(X, 'str', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_str_mvc <- function(X, ...) {uj::unq_mmm_ccc(X, 'str', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_str_scl <- function(X, ...) {uj::unq_mmm_ccc(X, 'str', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_str_vec <- function(X, ...) {uj::unq_mmm_ccc(X, 'str', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_str_vls <- function(X, ...) {uj::unq_mmm_ccc(X, 'str', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_uno_arr <- function(X, ...) {uj::unq_mmm_ccc(X, 'uno', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_uno_dtf <- function(X, ...) {uj::unq_mmm_ccc(X, 'uno', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_uno_gen <- function(X, ...) {uj::unq_mmm_ccc(X, 'uno', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_uno_mat <- function(X, ...) {uj::unq_mmm_ccc(X, 'uno', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_uno_mvc <- function(X, ...) {uj::unq_mmm_ccc(X, 'uno', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_uno_scl <- function(X, ...) {uj::unq_mmm_ccc(X, 'uno', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_uno_vec <- function(X, ...) {uj::unq_mmm_ccc(X, 'uno', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_uno_vls <- function(X, ...) {uj::unq_mmm_ccc(X, 'uno', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_whl_arr <- function(X, ...) {uj::unq_mmm_ccc(X, 'whl', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_whl_dtf <- function(X, ...) {uj::unq_mmm_ccc(X, 'whl', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_whl_gen <- function(X, ...) {uj::unq_mmm_ccc(X, 'whl', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_whl_mat <- function(X, ...) {uj::unq_mmm_ccc(X, 'whl', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_whl_mvc <- function(X, ...) {uj::unq_mmm_ccc(X, 'whl', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_whl_scl <- function(X, ...) {uj::unq_mmm_ccc(X, 'whl', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_whl_vec <- function(X, ...) {uj::unq_mmm_ccc(X, 'whl', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_whl_vls <- function(X, ...) {uj::unq_mmm_ccc(X, 'whl', 'vls', ...)}
