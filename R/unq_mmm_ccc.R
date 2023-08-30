#' @encoding UTF-8
#' @family props
#' @title unique + xmode + xclass combination properties
#' @description Functions for checking combinations of \link[=UNQ]{uniqueness}, \link[=mmm]{xmode}, and \link[=ccc]{xclass}.
#' @details
#' \tabular{ll}{  `unq_mmm_ccc_funs`   \tab What \link[=iunq]{unique} + \link[=mmm]{xmode} + \link[=ccc]{xclass} combination \link[=prop_funs]{property functions} are there? \cr   \tab   \cr
#'                `unq_{mmm}_{ccc}`    \tab Is `x` both unique and a match to the single xmode property `'{mmm}'` and single xclass property `'{ccc}'`? (where `{mmm}` and
#'                                          `{ccc}` are placeholders for any given xmode property and any given xclass property, respectively).                               \cr   \tab   \cr
#'                `unq_mmm_ccc`        \tab Is `x` both unique and a match to the single xmode and xclass properties in arguments `.MMM` and `.CCC`, respectively?                           }
#' @param x An R object.
#' @param .MMM A character scalar single xmode property from \code{\link{mmm_props}()}.
#' @param .CCC A character scalar single xclass property from \code{\link{ccc_props}()}.
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
unq_mmm_ccc <- function(x, .MMM, .CCC, ...) {
  if (uj::cmp_mmm_ccc(x, .MMM, .CCC, ...)) {
    if      (.CCC == "dtf") {base::nrow(x) == base::nrow(base::unique(x))}
    else if (.CCC == "vls") {
      N <- base::lengths(x)
      NU <- base::sapply(x, function(x) {base::length(base::unique(x))})
      base::all(N == NU)
    } else {base::length(x) == base::length(base::unique(x))}
  } else {F}
}

#' @rdname unq_mmm_ccc
#' @export
unq_mmm_ccc_funs <- function() {
  x <- base::expand.grid(mmm = uj::v(mmm), ccc = uj::v(ccc))
  x <- base::apply(x, 1, paste0, collapse = "_")
  base::paste0('unq_', base::sort(uj::av(x)))
}

#' @rdname unq_mmm_ccc
#' @export
unq_atm_arr <- function(x, ...) {uj::unq_mmm_ccc(x, 'atm', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_atm_dtf <- function(x, ...) {uj::unq_mmm_ccc(x, 'atm', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_atm_gen <- function(x, ...) {uj::unq_mmm_ccc(x, 'atm', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_atm_mat <- function(x, ...) {uj::unq_mmm_ccc(x, 'atm', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_atm_mvc <- function(x, ...) {uj::unq_mmm_ccc(x, 'atm', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_atm_scl <- function(x, ...) {uj::unq_mmm_ccc(x, 'atm', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_atm_vec <- function(x, ...) {uj::unq_mmm_ccc(x, 'atm', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_atm_vls <- function(x, ...) {uj::unq_mmm_ccc(x, 'atm', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ch1_arr <- function(x, ...) {uj::unq_mmm_ccc(x, 'ch1', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ch1_dtf <- function(x, ...) {uj::unq_mmm_ccc(x, 'ch1', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ch1_gen <- function(x, ...) {uj::unq_mmm_ccc(x, 'ch1', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ch1_mat <- function(x, ...) {uj::unq_mmm_ccc(x, 'ch1', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ch1_mvc <- function(x, ...) {uj::unq_mmm_ccc(x, 'ch1', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ch1_scl <- function(x, ...) {uj::unq_mmm_ccc(x, 'ch1', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ch1_vec <- function(x, ...) {uj::unq_mmm_ccc(x, 'ch1', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ch1_vls <- function(x, ...) {uj::unq_mmm_ccc(x, 'ch1', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ch3_arr <- function(x, ...) {uj::unq_mmm_ccc(x, 'ch3', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ch3_dtf <- function(x, ...) {uj::unq_mmm_ccc(x, 'ch3', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ch3_gen <- function(x, ...) {uj::unq_mmm_ccc(x, 'ch3', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ch3_mat <- function(x, ...) {uj::unq_mmm_ccc(x, 'ch3', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ch3_mvc <- function(x, ...) {uj::unq_mmm_ccc(x, 'ch3', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ch3_scl <- function(x, ...) {uj::unq_mmm_ccc(x, 'ch3', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ch3_vec <- function(x, ...) {uj::unq_mmm_ccc(x, 'ch3', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ch3_vls <- function(x, ...) {uj::unq_mmm_ccc(x, 'ch3', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_chr_arr <- function(x, ...) {uj::unq_mmm_ccc(x, 'chr', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_chr_dtf <- function(x, ...) {uj::unq_mmm_ccc(x, 'chr', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_chr_gen <- function(x, ...) {uj::unq_mmm_ccc(x, 'chr', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_chr_mat <- function(x, ...) {uj::unq_mmm_ccc(x, 'chr', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_chr_mvc <- function(x, ...) {uj::unq_mmm_ccc(x, 'chr', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_chr_scl <- function(x, ...) {uj::unq_mmm_ccc(x, 'chr', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_chr_vec <- function(x, ...) {uj::unq_mmm_ccc(x, 'chr', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_chr_vls <- function(x, ...) {uj::unq_mmm_ccc(x, 'chr', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_clr_arr <- function(x, ...) {uj::unq_mmm_ccc(x, 'clr', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_clr_dtf <- function(x, ...) {uj::unq_mmm_ccc(x, 'clr', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_clr_gen <- function(x, ...) {uj::unq_mmm_ccc(x, 'clr', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_clr_mat <- function(x, ...) {uj::unq_mmm_ccc(x, 'clr', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_clr_mvc <- function(x, ...) {uj::unq_mmm_ccc(x, 'clr', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_clr_scl <- function(x, ...) {uj::unq_mmm_ccc(x, 'clr', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_clr_vec <- function(x, ...) {uj::unq_mmm_ccc(x, 'clr', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_clr_vls <- function(x, ...) {uj::unq_mmm_ccc(x, 'clr', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_evn_arr <- function(x, ...) {uj::unq_mmm_ccc(x, 'evn', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_evn_dtf <- function(x, ...) {uj::unq_mmm_ccc(x, 'evn', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_evn_gen <- function(x, ...) {uj::unq_mmm_ccc(x, 'evn', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_evn_mat <- function(x, ...) {uj::unq_mmm_ccc(x, 'evn', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_evn_mvc <- function(x, ...) {uj::unq_mmm_ccc(x, 'evn', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_evn_scl <- function(x, ...) {uj::unq_mmm_ccc(x, 'evn', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_evn_vec <- function(x, ...) {uj::unq_mmm_ccc(x, 'evn', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_evn_vls <- function(x, ...) {uj::unq_mmm_ccc(x, 'evn', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_fac_arr <- function(x, ...) {uj::unq_mmm_ccc(x, 'fac', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_fac_dtf <- function(x, ...) {uj::unq_mmm_ccc(x, 'fac', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_fac_gen <- function(x, ...) {uj::unq_mmm_ccc(x, 'fac', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_fac_mat <- function(x, ...) {uj::unq_mmm_ccc(x, 'fac', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_fac_mvc <- function(x, ...) {uj::unq_mmm_ccc(x, 'fac', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_fac_scl <- function(x, ...) {uj::unq_mmm_ccc(x, 'fac', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_fac_vec <- function(x, ...) {uj::unq_mmm_ccc(x, 'fac', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_fac_vls <- function(x, ...) {uj::unq_mmm_ccc(x, 'fac', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_frc_arr <- function(x, ...) {uj::unq_mmm_ccc(x, 'frc', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_frc_dtf <- function(x, ...) {uj::unq_mmm_ccc(x, 'frc', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_frc_gen <- function(x, ...) {uj::unq_mmm_ccc(x, 'frc', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_frc_mat <- function(x, ...) {uj::unq_mmm_ccc(x, 'frc', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_frc_mvc <- function(x, ...) {uj::unq_mmm_ccc(x, 'frc', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_frc_scl <- function(x, ...) {uj::unq_mmm_ccc(x, 'frc', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_frc_vec <- function(x, ...) {uj::unq_mmm_ccc(x, 'frc', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_frc_vls <- function(x, ...) {uj::unq_mmm_ccc(x, 'frc', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ind_arr <- function(x, ...) {uj::unq_mmm_ccc(x, 'ind', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ind_dtf <- function(x, ...) {uj::unq_mmm_ccc(x, 'ind', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ind_gen <- function(x, ...) {uj::unq_mmm_ccc(x, 'ind', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ind_mat <- function(x, ...) {uj::unq_mmm_ccc(x, 'ind', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ind_mvc <- function(x, ...) {uj::unq_mmm_ccc(x, 'ind', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ind_scl <- function(x, ...) {uj::unq_mmm_ccc(x, 'ind', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ind_vec <- function(x, ...) {uj::unq_mmm_ccc(x, 'ind', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ind_vls <- function(x, ...) {uj::unq_mmm_ccc(x, 'ind', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_lgl_arr <- function(x, ...) {uj::unq_mmm_ccc(x, 'lgl', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_lgl_dtf <- function(x, ...) {uj::unq_mmm_ccc(x, 'lgl', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_lgl_gen <- function(x, ...) {uj::unq_mmm_ccc(x, 'lgl', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_lgl_mat <- function(x, ...) {uj::unq_mmm_ccc(x, 'lgl', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_lgl_mvc <- function(x, ...) {uj::unq_mmm_ccc(x, 'lgl', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_lgl_scl <- function(x, ...) {uj::unq_mmm_ccc(x, 'lgl', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_lgl_vec <- function(x, ...) {uj::unq_mmm_ccc(x, 'lgl', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_lgl_vls <- function(x, ...) {uj::unq_mmm_ccc(x, 'lgl', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_neg_arr <- function(x, ...) {uj::unq_mmm_ccc(x, 'neg', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_neg_dtf <- function(x, ...) {uj::unq_mmm_ccc(x, 'neg', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_neg_gen <- function(x, ...) {uj::unq_mmm_ccc(x, 'neg', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_neg_mat <- function(x, ...) {uj::unq_mmm_ccc(x, 'neg', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_neg_mvc <- function(x, ...) {uj::unq_mmm_ccc(x, 'neg', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_neg_scl <- function(x, ...) {uj::unq_mmm_ccc(x, 'neg', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_neg_vec <- function(x, ...) {uj::unq_mmm_ccc(x, 'neg', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_neg_vls <- function(x, ...) {uj::unq_mmm_ccc(x, 'neg', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ngw_arr <- function(x, ...) {uj::unq_mmm_ccc(x, 'ngw', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ngw_dtf <- function(x, ...) {uj::unq_mmm_ccc(x, 'ngw', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ngw_gen <- function(x, ...) {uj::unq_mmm_ccc(x, 'ngw', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ngw_mat <- function(x, ...) {uj::unq_mmm_ccc(x, 'ngw', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ngw_mvc <- function(x, ...) {uj::unq_mmm_ccc(x, 'ngw', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ngw_scl <- function(x, ...) {uj::unq_mmm_ccc(x, 'ngw', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ngw_vec <- function(x, ...) {uj::unq_mmm_ccc(x, 'ngw', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ngw_vls <- function(x, ...) {uj::unq_mmm_ccc(x, 'ngw', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nng_arr <- function(x, ...) {uj::unq_mmm_ccc(x, 'nng', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nng_dtf <- function(x, ...) {uj::unq_mmm_ccc(x, 'nng', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nng_gen <- function(x, ...) {uj::unq_mmm_ccc(x, 'nng', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nng_mat <- function(x, ...) {uj::unq_mmm_ccc(x, 'nng', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nng_mvc <- function(x, ...) {uj::unq_mmm_ccc(x, 'nng', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nng_scl <- function(x, ...) {uj::unq_mmm_ccc(x, 'nng', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nng_vec <- function(x, ...) {uj::unq_mmm_ccc(x, 'nng', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nng_vls <- function(x, ...) {uj::unq_mmm_ccc(x, 'nng', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nnw_arr <- function(x, ...) {uj::unq_mmm_ccc(x, 'nnw', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nnw_dtf <- function(x, ...) {uj::unq_mmm_ccc(x, 'nnw', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nnw_gen <- function(x, ...) {uj::unq_mmm_ccc(x, 'nnw', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nnw_mat <- function(x, ...) {uj::unq_mmm_ccc(x, 'nnw', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nnw_mvc <- function(x, ...) {uj::unq_mmm_ccc(x, 'nnw', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nnw_scl <- function(x, ...) {uj::unq_mmm_ccc(x, 'nnw', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nnw_vec <- function(x, ...) {uj::unq_mmm_ccc(x, 'nnw', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nnw_vls <- function(x, ...) {uj::unq_mmm_ccc(x, 'nnw', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nps_arr <- function(x, ...) {uj::unq_mmm_ccc(x, 'nps', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nps_dtf <- function(x, ...) {uj::unq_mmm_ccc(x, 'nps', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nps_gen <- function(x, ...) {uj::unq_mmm_ccc(x, 'nps', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nps_mat <- function(x, ...) {uj::unq_mmm_ccc(x, 'nps', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nps_mvc <- function(x, ...) {uj::unq_mmm_ccc(x, 'nps', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nps_scl <- function(x, ...) {uj::unq_mmm_ccc(x, 'nps', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nps_vec <- function(x, ...) {uj::unq_mmm_ccc(x, 'nps', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nps_vls <- function(x, ...) {uj::unq_mmm_ccc(x, 'nps', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_npw_arr <- function(x, ...) {uj::unq_mmm_ccc(x, 'npw', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_npw_dtf <- function(x, ...) {uj::unq_mmm_ccc(x, 'npw', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_npw_gen <- function(x, ...) {uj::unq_mmm_ccc(x, 'npw', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_npw_mat <- function(x, ...) {uj::unq_mmm_ccc(x, 'npw', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_npw_mvc <- function(x, ...) {uj::unq_mmm_ccc(x, 'npw', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_npw_scl <- function(x, ...) {uj::unq_mmm_ccc(x, 'npw', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_npw_vec <- function(x, ...) {uj::unq_mmm_ccc(x, 'npw', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_npw_vls <- function(x, ...) {uj::unq_mmm_ccc(x, 'npw', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nst_arr <- function(x, ...) {uj::unq_mmm_ccc(x, 'nst', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nst_dtf <- function(x, ...) {uj::unq_mmm_ccc(x, 'nst', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nst_gen <- function(x, ...) {uj::unq_mmm_ccc(x, 'nst', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nst_mat <- function(x, ...) {uj::unq_mmm_ccc(x, 'nst', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nst_mvc <- function(x, ...) {uj::unq_mmm_ccc(x, 'nst', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nst_scl <- function(x, ...) {uj::unq_mmm_ccc(x, 'nst', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nst_vec <- function(x, ...) {uj::unq_mmm_ccc(x, 'nst', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_nst_vls <- function(x, ...) {uj::unq_mmm_ccc(x, 'nst', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_num_arr <- function(x, ...) {uj::unq_mmm_ccc(x, 'num', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_num_dtf <- function(x, ...) {uj::unq_mmm_ccc(x, 'num', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_num_gen <- function(x, ...) {uj::unq_mmm_ccc(x, 'num', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_num_mat <- function(x, ...) {uj::unq_mmm_ccc(x, 'num', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_num_mvc <- function(x, ...) {uj::unq_mmm_ccc(x, 'num', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_num_scl <- function(x, ...) {uj::unq_mmm_ccc(x, 'num', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_num_vec <- function(x, ...) {uj::unq_mmm_ccc(x, 'num', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_num_vls <- function(x, ...) {uj::unq_mmm_ccc(x, 'num', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_odd_arr <- function(x, ...) {uj::unq_mmm_ccc(x, 'odd', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_odd_dtf <- function(x, ...) {uj::unq_mmm_ccc(x, 'odd', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_odd_gen <- function(x, ...) {uj::unq_mmm_ccc(x, 'odd', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_odd_mat <- function(x, ...) {uj::unq_mmm_ccc(x, 'odd', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_odd_mvc <- function(x, ...) {uj::unq_mmm_ccc(x, 'odd', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_odd_scl <- function(x, ...) {uj::unq_mmm_ccc(x, 'odd', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_odd_vec <- function(x, ...) {uj::unq_mmm_ccc(x, 'odd', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_odd_vls <- function(x, ...) {uj::unq_mmm_ccc(x, 'odd', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ord_arr <- function(x, ...) {uj::unq_mmm_ccc(x, 'ord', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ord_dtf <- function(x, ...) {uj::unq_mmm_ccc(x, 'ord', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ord_gen <- function(x, ...) {uj::unq_mmm_ccc(x, 'ord', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ord_mat <- function(x, ...) {uj::unq_mmm_ccc(x, 'ord', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ord_mvc <- function(x, ...) {uj::unq_mmm_ccc(x, 'ord', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ord_scl <- function(x, ...) {uj::unq_mmm_ccc(x, 'ord', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ord_vec <- function(x, ...) {uj::unq_mmm_ccc(x, 'ord', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ord_vls <- function(x, ...) {uj::unq_mmm_ccc(x, 'ord', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_pct_arr <- function(x, ...) {uj::unq_mmm_ccc(x, 'pct', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_pct_dtf <- function(x, ...) {uj::unq_mmm_ccc(x, 'pct', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_pct_gen <- function(x, ...) {uj::unq_mmm_ccc(x, 'pct', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_pct_mat <- function(x, ...) {uj::unq_mmm_ccc(x, 'pct', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_pct_mvc <- function(x, ...) {uj::unq_mmm_ccc(x, 'pct', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_pct_scl <- function(x, ...) {uj::unq_mmm_ccc(x, 'pct', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_pct_vec <- function(x, ...) {uj::unq_mmm_ccc(x, 'pct', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_pct_vls <- function(x, ...) {uj::unq_mmm_ccc(x, 'pct', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_pos_arr <- function(x, ...) {uj::unq_mmm_ccc(x, 'pos', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_pos_dtf <- function(x, ...) {uj::unq_mmm_ccc(x, 'pos', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_pos_gen <- function(x, ...) {uj::unq_mmm_ccc(x, 'pos', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_pos_mat <- function(x, ...) {uj::unq_mmm_ccc(x, 'pos', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_pos_mvc <- function(x, ...) {uj::unq_mmm_ccc(x, 'pos', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_pos_scl <- function(x, ...) {uj::unq_mmm_ccc(x, 'pos', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_pos_vec <- function(x, ...) {uj::unq_mmm_ccc(x, 'pos', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_pos_vls <- function(x, ...) {uj::unq_mmm_ccc(x, 'pos', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ppn_arr <- function(x, ...) {uj::unq_mmm_ccc(x, 'ppn', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ppn_dtf <- function(x, ...) {uj::unq_mmm_ccc(x, 'ppn', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ppn_gen <- function(x, ...) {uj::unq_mmm_ccc(x, 'ppn', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ppn_mat <- function(x, ...) {uj::unq_mmm_ccc(x, 'ppn', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ppn_mvc <- function(x, ...) {uj::unq_mmm_ccc(x, 'ppn', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ppn_scl <- function(x, ...) {uj::unq_mmm_ccc(x, 'ppn', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ppn_vec <- function(x, ...) {uj::unq_mmm_ccc(x, 'ppn', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_ppn_vls <- function(x, ...) {uj::unq_mmm_ccc(x, 'ppn', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_psw_arr <- function(x, ...) {uj::unq_mmm_ccc(x, 'psw', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_psw_dtf <- function(x, ...) {uj::unq_mmm_ccc(x, 'psw', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_psw_gen <- function(x, ...) {uj::unq_mmm_ccc(x, 'psw', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_psw_mat <- function(x, ...) {uj::unq_mmm_ccc(x, 'psw', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_psw_mvc <- function(x, ...) {uj::unq_mmm_ccc(x, 'psw', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_psw_scl <- function(x, ...) {uj::unq_mmm_ccc(x, 'psw', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_psw_vec <- function(x, ...) {uj::unq_mmm_ccc(x, 'psw', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_psw_vls <- function(x, ...) {uj::unq_mmm_ccc(x, 'psw', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_srt_arr <- function(x, ...) {uj::unq_mmm_ccc(x, 'srt', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_srt_dtf <- function(x, ...) {uj::unq_mmm_ccc(x, 'srt', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_srt_gen <- function(x, ...) {uj::unq_mmm_ccc(x, 'srt', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_srt_mat <- function(x, ...) {uj::unq_mmm_ccc(x, 'srt', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_srt_mvc <- function(x, ...) {uj::unq_mmm_ccc(x, 'srt', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_srt_scl <- function(x, ...) {uj::unq_mmm_ccc(x, 'srt', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_srt_vec <- function(x, ...) {uj::unq_mmm_ccc(x, 'srt', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_srt_vls <- function(x, ...) {uj::unq_mmm_ccc(x, 'srt', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_str_arr <- function(x, ...) {uj::unq_mmm_ccc(x, 'str', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_str_dtf <- function(x, ...) {uj::unq_mmm_ccc(x, 'str', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_str_gen <- function(x, ...) {uj::unq_mmm_ccc(x, 'str', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_str_mat <- function(x, ...) {uj::unq_mmm_ccc(x, 'str', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_str_mvc <- function(x, ...) {uj::unq_mmm_ccc(x, 'str', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_str_scl <- function(x, ...) {uj::unq_mmm_ccc(x, 'str', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_str_vec <- function(x, ...) {uj::unq_mmm_ccc(x, 'str', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_str_vls <- function(x, ...) {uj::unq_mmm_ccc(x, 'str', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_uno_arr <- function(x, ...) {uj::unq_mmm_ccc(x, 'uno', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_uno_dtf <- function(x, ...) {uj::unq_mmm_ccc(x, 'uno', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_uno_gen <- function(x, ...) {uj::unq_mmm_ccc(x, 'uno', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_uno_mat <- function(x, ...) {uj::unq_mmm_ccc(x, 'uno', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_uno_mvc <- function(x, ...) {uj::unq_mmm_ccc(x, 'uno', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_uno_scl <- function(x, ...) {uj::unq_mmm_ccc(x, 'uno', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_uno_vec <- function(x, ...) {uj::unq_mmm_ccc(x, 'uno', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_uno_vls <- function(x, ...) {uj::unq_mmm_ccc(x, 'uno', 'vls', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_whl_arr <- function(x, ...) {uj::unq_mmm_ccc(x, 'whl', 'arr', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_whl_dtf <- function(x, ...) {uj::unq_mmm_ccc(x, 'whl', 'dtf', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_whl_gen <- function(x, ...) {uj::unq_mmm_ccc(x, 'whl', 'gen', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_whl_mat <- function(x, ...) {uj::unq_mmm_ccc(x, 'whl', 'mat', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_whl_mvc <- function(x, ...) {uj::unq_mmm_ccc(x, 'whl', 'mvc', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_whl_scl <- function(x, ...) {uj::unq_mmm_ccc(x, 'whl', 'scl', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_whl_vec <- function(x, ...) {uj::unq_mmm_ccc(x, 'whl', 'vec', ...)}

#' @rdname unq_mmm_ccc
#' @export
unq_whl_vls <- function(x, ...) {uj::unq_mmm_ccc(x, 'whl', 'vls', ...)}
