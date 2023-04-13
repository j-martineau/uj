#' @encoding UTF-8
#' @family properties
#' @title xmode + xclass combination properties
#' @description Check for combinations of \link[=mmm]{xmode} and \link[=ccc]{xclass}.
#' @details
#' \tabular{ll}{  `mmm_ccc_funs`   \tab What \link[=mmm]{xmode} + \link[=ccc]{xclass} combination \link[=prop_funs]{property functions} are there? \cr   \tab   \cr
#'                `{mmm}_{ccc}`    \tab Does `X` match the single xmode property `'{mmm}''` and single xclass property `'{ccc}'`, where `'{mmm}'`
#'                                      and `'{ccc}'` are placeholders for any given xmode and any given xclass properties, respectively?          \cr   \tab   \cr
#'                `mmm_ccc`        \tab Does `X` match the single xmode and xclass properties in arguments `MMM` and `CCC`, respectively?                         }
#' @param X An R object.
#' @param MMM A character scalar single xmode property from \code{\link{mmm_props}()}.
#' @param CCC A character scalar single xclass property from \code{\link{ccc_props}()}.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return **A character scalar** \cr\cr `mmm_ccc_funs`
#' \cr\cr  **A logical scalar**   \cr\cr `{mmm}_{ccc}, mmm_ccc`
#' @examples
#' egScl <- "a"
#' egLst <- list(letters = letters, LETTERS = LETTERS)
#' mmm_cccfuns()
#' c(mmm_ccc(lst2x26, "chr", "vls"), chr_vls(egLst))
#' c(mmm_ccc(lst2x26, "num", "mat"), num_mat(egLst))
#' c(mmm_ccc(scalar1, "ch1", "scl"), ch1_scl(egScl))
#' c(mmm_ccc(letters, "ch1", "vec"), ch1_vec(letters))
#' c(mmm_ccc(letters, "num", "dtf"), num_dtf(letters))
#' @export
mmm_ccc <- function(X, MMM, CCC, ...) {
  cfun <- function(cx) {uj::run("uj:::.", base::toupper(CCC), "(cx)")}
  mfun <- function(mx) {uj::run("uj:::.", base::toupper(MMM), "(mx)")}
  dfun <- function(dx) {
    if (base::NROW(dx) * base::NCOL(dx) > 0) {
      for (i in 1:base::NCOL(dx)) {if (!mfun(dx[[i]])) {return(F)}}
      T
    } else {F}
  }
  vfun <- function(vx) {if (base::length(vx) == 0) {F} else {base::all(base::sapply(X, mfun))}}
  Errors <- uj:::.meets_errs(X, ...)
  ErrMMM <- "[MMM] is not a scalar value from mmm_props()."
  ErrCCC <- "[CCC] is not a scalar value from ccc_props()."
  if      (base::length(MMM) != 1                     ) {Errors <- base::c(Errors, ErrMMM)}
  else if (!base::is.character(MMM)                   ) {Errors <- base::c(Errors, ErrMMM)}
  else if (base::is.na(MMM)                           ) {Errors <- base::c(Errors, ErrMMM)}
  else if (!(MMM %in% base::c(uj::v(mmm), uj::v(MMM)))) {Errors <- base::c(Errors, ErrMMM)}
  if      (base::length(CCC) != 1                     ) {Errors <- base::c(Errors, ErrCCC)}
  else if (!base::is.character(CCC)                   ) {Errors <- base::c(Errors, ErrCCC)}
  else if (base::is.na(CCC)                           ) {Errors <- base::c(Errors, ErrCCC)}
  else if (!(CCC %in% base::c(uj::v(ccc), uj::v(CCC)))) {Errors <- base::c(Errors, ErrCCC)}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  CCC <- base::tolower(CCC)
  if (!uj::meets(X, ...)) {F} else if (!cfun(X)) {F} else if (CCC == 'dtf') {dfun(X)} else if (CCC == "vls") {vfun(X)} else {mfun(X)}
}

#' @rdname mmm_ccc
#' @export
mmm_ccc_funs <- function() {
  Y <- base::expand.grid(mmm = uj::v(mmm), ccc = uj::v(ccc))
  Y <- base::apply(Y, 1, base::paste0, collapse = "_")
  base::sort(uj::av(Y))
}

#' @rdname mmm_ccc
#' @export
ch1_arr <- function(X, ...) {uj::mmm_ccc(X, 'ch1', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
ch1_dtf <- function(X, ...) {uj::mmm_ccc(X, 'ch1', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
ch1_gen <- function(X, ...) {uj::mmm_ccc(X, 'ch1', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
ch1_mat <- function(X, ...) {uj::mmm_ccc(X, 'ch1', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
ch1_mvc <- function(X, ...) {uj::mmm_ccc(X, 'ch1', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
ch1_scl <- function(X, ...) {uj::mmm_ccc(X, 'ch1', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
ch1_vec <- function(X, ...) {uj::mmm_ccc(X, 'ch1', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
ch1_vls <- function(X, ...) {uj::mmm_ccc(X, 'ch1', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
ch3_arr <- function(X, ...) {uj::mmm_ccc(X, 'ch3', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
ch3_dtf <- function(X, ...) {uj::mmm_ccc(X, 'ch3', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
ch3_gen <- function(X, ...) {uj::mmm_ccc(X, 'ch3', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
ch3_mat <- function(X, ...) {uj::mmm_ccc(X, 'ch3', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
ch3_mvc <- function(X, ...) {uj::mmm_ccc(X, 'ch3', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
ch3_scl <- function(X, ...) {uj::mmm_ccc(X, 'ch3', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
ch3_vec <- function(X, ...) {uj::mmm_ccc(X, 'ch3', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
ch3_vls <- function(X, ...) {uj::mmm_ccc(X, 'ch3', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
chr_arr <- function(X, ...) {uj::mmm_ccc(X, 'chr', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
chr_dtf <- function(X, ...) {uj::mmm_ccc(X, 'chr', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
chr_gen <- function(X, ...) {uj::mmm_ccc(X, 'chr', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
chr_mat <- function(X, ...) {uj::mmm_ccc(X, 'chr', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
chr_mvc <- function(X, ...) {uj::mmm_ccc(X, 'chr', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
chr_scl <- function(X, ...) {uj::mmm_ccc(X, 'chr', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
chr_vec <- function(X, ...) {uj::mmm_ccc(X, 'chr', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
chr_vls <- function(X, ...) {uj::mmm_ccc(X, 'chr', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
clr_arr <- function(X, ...) {uj::mmm_ccc(X, 'clr', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
clr_dtf <- function(X, ...) {uj::mmm_ccc(X, 'clr', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
clr_gen <- function(X, ...) {uj::mmm_ccc(X, 'clr', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
clr_mat <- function(X, ...) {uj::mmm_ccc(X, 'clr', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
clr_mvc <- function(X, ...) {uj::mmm_ccc(X, 'clr', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
clr_scl <- function(X, ...) {uj::mmm_ccc(X, 'clr', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
clr_vec <- function(X, ...) {uj::mmm_ccc(X, 'clr', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
clr_vls <- function(X, ...) {uj::mmm_ccc(X, 'clr', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
evn_arr <- function(X, ...) {uj::mmm_ccc(X, 'evn', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
evn_dtf <- function(X, ...) {uj::mmm_ccc(X, 'evn', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
evn_gen <- function(X, ...) {uj::mmm_ccc(X, 'evn', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
evn_mat <- function(X, ...) {uj::mmm_ccc(X, 'evn', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
evn_mvc <- function(X, ...) {uj::mmm_ccc(X, 'evn', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
evn_scl <- function(X, ...) {uj::mmm_ccc(X, 'evn', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
evn_vec <- function(X, ...) {uj::mmm_ccc(X, 'evn', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
evn_vls <- function(X, ...) {uj::mmm_ccc(X, 'evn', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
fac_arr <- function(X, ...) {uj::mmm_ccc(X, 'fac', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
fac_dtf <- function(X, ...) {uj::mmm_ccc(X, 'fac', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
fac_gen <- function(X, ...) {uj::mmm_ccc(X, 'fac', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
fac_mat <- function(X, ...) {uj::mmm_ccc(X, 'fac', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
fac_mvc <- function(X, ...) {uj::mmm_ccc(X, 'fac', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
fac_scl <- function(X, ...) {uj::mmm_ccc(X, 'fac', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
fac_vec <- function(X, ...) {uj::mmm_ccc(X, 'fac', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
fac_vls <- function(X, ...) {uj::mmm_ccc(X, 'fac', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
frc_arr <- function(X, ...) {uj::mmm_ccc(X, 'frc', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
frc_dtf <- function(X, ...) {uj::mmm_ccc(X, 'frc', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
frc_gen <- function(X, ...) {uj::mmm_ccc(X, 'frc', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
frc_mat <- function(X, ...) {uj::mmm_ccc(X, 'frc', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
frc_mvc <- function(X, ...) {uj::mmm_ccc(X, 'frc', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
frc_scl <- function(X, ...) {uj::mmm_ccc(X, 'frc', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
frc_vec <- function(X, ...) {uj::mmm_ccc(X, 'frc', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
frc_vls <- function(X, ...) {uj::mmm_ccc(X, 'frc', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
ind_arr <- function(X, ...) {uj::mmm_ccc(X, 'ind', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
ind_dtf <- function(X, ...) {uj::mmm_ccc(X, 'ind', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
ind_gen <- function(X, ...) {uj::mmm_ccc(X, 'ind', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
ind_mat <- function(X, ...) {uj::mmm_ccc(X, 'ind', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
ind_mvc <- function(X, ...) {uj::mmm_ccc(X, 'ind', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
ind_scl <- function(X, ...) {uj::mmm_ccc(X, 'ind', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
ind_vec <- function(X, ...) {uj::mmm_ccc(X, 'ind', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
ind_vls <- function(X, ...) {uj::mmm_ccc(X, 'ind', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
lgl_arr <- function(X, ...) {uj::mmm_ccc(X, 'lgl', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
lgl_dtf <- function(X, ...) {uj::mmm_ccc(X, 'lgl', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
lgl_gen <- function(X, ...) {uj::mmm_ccc(X, 'lgl', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
lgl_mat <- function(X, ...) {uj::mmm_ccc(X, 'lgl', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
lgl_mvc <- function(X, ...) {uj::mmm_ccc(X, 'lgl', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
lgl_scl <- function(X, ...) {uj::mmm_ccc(X, 'lgl', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
lgl_vec <- function(X, ...) {uj::mmm_ccc(X, 'lgl', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
lgl_vls <- function(X, ...) {uj::mmm_ccc(X, 'lgl', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
neg_arr <- function(X, ...) {uj::mmm_ccc(X, 'neg', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
neg_dtf <- function(X, ...) {uj::mmm_ccc(X, 'neg', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
neg_gen <- function(X, ...) {uj::mmm_ccc(X, 'neg', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
neg_mat <- function(X, ...) {uj::mmm_ccc(X, 'neg', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
neg_mvc <- function(X, ...) {uj::mmm_ccc(X, 'neg', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
neg_scl <- function(X, ...) {uj::mmm_ccc(X, 'neg', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
neg_vec <- function(X, ...) {uj::mmm_ccc(X, 'neg', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
neg_vls <- function(X, ...) {uj::mmm_ccc(X, 'neg', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
ngw_arr <- function(X, ...) {uj::mmm_ccc(X, 'ngw', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
ngw_dtf <- function(X, ...) {uj::mmm_ccc(X, 'ngw', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
ngw_gen <- function(X, ...) {uj::mmm_ccc(X, 'ngw', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
ngw_mat <- function(X, ...) {uj::mmm_ccc(X, 'ngw', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
ngw_mvc <- function(X, ...) {uj::mmm_ccc(X, 'ngw', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
ngw_scl <- function(X, ...) {uj::mmm_ccc(X, 'ngw', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
ngw_vec <- function(X, ...) {uj::mmm_ccc(X, 'ngw', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
ngw_vls <- function(X, ...) {uj::mmm_ccc(X, 'ngw', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
nng_arr <- function(X, ...) {uj::mmm_ccc(X, 'nng', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
nng_dtf <- function(X, ...) {uj::mmm_ccc(X, 'nng', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
nng_gen <- function(X, ...) {uj::mmm_ccc(X, 'nng', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
nng_mat <- function(X, ...) {uj::mmm_ccc(X, 'nng', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
nng_mvc <- function(X, ...) {uj::mmm_ccc(X, 'nng', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
nng_scl <- function(X, ...) {uj::mmm_ccc(X, 'nng', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
nng_vec <- function(X, ...) {uj::mmm_ccc(X, 'nng', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
nng_vls <- function(X, ...) {uj::mmm_ccc(X, 'nng', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
nnw_arr <- function(X, ...) {uj::mmm_ccc(X, 'nnw', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
nnw_dtf <- function(X, ...) {uj::mmm_ccc(X, 'nnw', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
nnw_gen <- function(X, ...) {uj::mmm_ccc(X, 'nnw', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
nnw_mat <- function(X, ...) {uj::mmm_ccc(X, 'nnw', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
nnw_mvc <- function(X, ...) {uj::mmm_ccc(X, 'nnw', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
nnw_scl <- function(X, ...) {uj::mmm_ccc(X, 'nnw', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
nnw_vec <- function(X, ...) {uj::mmm_ccc(X, 'nnw', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
nnw_vls <- function(X, ...) {uj::mmm_ccc(X, 'nnw', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
nps_arr <- function(X, ...) {uj::mmm_ccc(X, 'nps', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
nps_dtf <- function(X, ...) {uj::mmm_ccc(X, 'nps', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
nps_gen <- function(X, ...) {uj::mmm_ccc(X, 'nps', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
nps_mat <- function(X, ...) {uj::mmm_ccc(X, 'nps', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
nps_mvc <- function(X, ...) {uj::mmm_ccc(X, 'nps', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
nps_scl <- function(X, ...) {uj::mmm_ccc(X, 'nps', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
nps_vec <- function(X, ...) {uj::mmm_ccc(X, 'nps', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
nps_vls <- function(X, ...) {uj::mmm_ccc(X, 'nps', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
npw_arr <- function(X, ...) {uj::mmm_ccc(X, 'npw', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
npw_dtf <- function(X, ...) {uj::mmm_ccc(X, 'npw', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
npw_gen <- function(X, ...) {uj::mmm_ccc(X, 'npw', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
npw_mat <- function(X, ...) {uj::mmm_ccc(X, 'npw', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
npw_mvc <- function(X, ...) {uj::mmm_ccc(X, 'npw', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
npw_scl <- function(X, ...) {uj::mmm_ccc(X, 'npw', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
npw_vec <- function(X, ...) {uj::mmm_ccc(X, 'npw', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
npw_vls <- function(X, ...) {uj::mmm_ccc(X, 'npw', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
nst_arr <- function(X, ...) {uj::mmm_ccc(X, 'nst', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
nst_dtf <- function(X, ...) {uj::mmm_ccc(X, 'nst', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
nst_gen <- function(X, ...) {uj::mmm_ccc(X, 'nst', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
nst_mat <- function(X, ...) {uj::mmm_ccc(X, 'nst', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
nst_mvc <- function(X, ...) {uj::mmm_ccc(X, 'nst', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
nst_scl <- function(X, ...) {uj::mmm_ccc(X, 'nst', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
nst_vec <- function(X, ...) {uj::mmm_ccc(X, 'nst', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
nst_vls <- function(X, ...) {uj::mmm_ccc(X, 'nst', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
num_arr <- function(X, ...) {uj::mmm_ccc(X, 'num', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
num_dtf <- function(X, ...) {uj::mmm_ccc(X, 'num', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
num_gen <- function(X, ...) {uj::mmm_ccc(X, 'num', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
num_mat <- function(X, ...) {uj::mmm_ccc(X, 'num', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
num_mvc <- function(X, ...) {uj::mmm_ccc(X, 'num', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
num_scl <- function(X, ...) {uj::mmm_ccc(X, 'num', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
num_vec <- function(X, ...) {uj::mmm_ccc(X, 'num', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
num_vls <- function(X, ...) {uj::mmm_ccc(X, 'num', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
odd_arr <- function(X, ...) {uj::mmm_ccc(X, 'odd', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
odd_dtf <- function(X, ...) {uj::mmm_ccc(X, 'odd', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
odd_gen <- function(X, ...) {uj::mmm_ccc(X, 'odd', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
odd_mat <- function(X, ...) {uj::mmm_ccc(X, 'odd', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
odd_mvc <- function(X, ...) {uj::mmm_ccc(X, 'odd', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
odd_scl <- function(X, ...) {uj::mmm_ccc(X, 'odd', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
odd_vec <- function(X, ...) {uj::mmm_ccc(X, 'odd', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
odd_vls <- function(X, ...) {uj::mmm_ccc(X, 'odd', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
ord_arr <- function(X, ...) {uj::mmm_ccc(X, 'ord', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
ord_dtf <- function(X, ...) {uj::mmm_ccc(X, 'ord', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
ord_gen <- function(X, ...) {uj::mmm_ccc(X, 'ord', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
ord_mat <- function(X, ...) {uj::mmm_ccc(X, 'ord', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
ord_mvc <- function(X, ...) {uj::mmm_ccc(X, 'ord', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
ord_scl <- function(X, ...) {uj::mmm_ccc(X, 'ord', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
ord_vec <- function(X, ...) {uj::mmm_ccc(X, 'ord', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
ord_vls <- function(X, ...) {uj::mmm_ccc(X, 'ord', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
pct_arr <- function(X, ...) {uj::mmm_ccc(X, 'pct', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
pct_dtf <- function(X, ...) {uj::mmm_ccc(X, 'pct', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
pct_gen <- function(X, ...) {uj::mmm_ccc(X, 'pct', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
pct_mat <- function(X, ...) {uj::mmm_ccc(X, 'pct', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
pct_mvc <- function(X, ...) {uj::mmm_ccc(X, 'pct', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
pct_scl <- function(X, ...) {uj::mmm_ccc(X, 'pct', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
pct_vec <- function(X, ...) {uj::mmm_ccc(X, 'pct', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
pct_vls <- function(X, ...) {uj::mmm_ccc(X, 'pct', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
pos_arr <- function(X, ...) {uj::mmm_ccc(X, 'pos', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
pos_dtf <- function(X, ...) {uj::mmm_ccc(X, 'pos', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
pos_gen <- function(X, ...) {uj::mmm_ccc(X, 'pos', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
pos_mat <- function(X, ...) {uj::mmm_ccc(X, 'pos', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
pos_mvc <- function(X, ...) {uj::mmm_ccc(X, 'pos', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
pos_scl <- function(X, ...) {uj::mmm_ccc(X, 'pos', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
pos_vec <- function(X, ...) {uj::mmm_ccc(X, 'pos', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
pos_vls <- function(X, ...) {uj::mmm_ccc(X, 'pos', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
ppn_arr <- function(X, ...) {uj::mmm_ccc(X, 'ppn', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
ppn_dtf <- function(X, ...) {uj::mmm_ccc(X, 'ppn', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
ppn_gen <- function(X, ...) {uj::mmm_ccc(X, 'ppn', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
ppn_mat <- function(X, ...) {uj::mmm_ccc(X, 'ppn', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
ppn_mvc <- function(X, ...) {uj::mmm_ccc(X, 'ppn', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
ppn_scl <- function(X, ...) {uj::mmm_ccc(X, 'ppn', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
ppn_vec <- function(X, ...) {uj::mmm_ccc(X, 'ppn', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
ppn_vls <- function(X, ...) {uj::mmm_ccc(X, 'ppn', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
psw_arr <- function(X, ...) {uj::mmm_ccc(X, 'psw', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
psw_dtf <- function(X, ...) {uj::mmm_ccc(X, 'psw', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
psw_gen <- function(X, ...) {uj::mmm_ccc(X, 'psw', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
psw_mat <- function(X, ...) {uj::mmm_ccc(X, 'psw', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
psw_mvc <- function(X, ...) {uj::mmm_ccc(X, 'psw', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
psw_scl <- function(X, ...) {uj::mmm_ccc(X, 'psw', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
psw_vec <- function(X, ...) {uj::mmm_ccc(X, 'psw', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
psw_vls <- function(X, ...) {uj::mmm_ccc(X, 'psw', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
srt_arr <- function(X, ...) {uj::mmm_ccc(X, 'srt', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
srt_dtf <- function(X, ...) {uj::mmm_ccc(X, 'srt', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
srt_gen <- function(X, ...) {uj::mmm_ccc(X, 'srt', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
srt_mat <- function(X, ...) {uj::mmm_ccc(X, 'srt', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
srt_mvc <- function(X, ...) {uj::mmm_ccc(X, 'srt', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
srt_scl <- function(X, ...) {uj::mmm_ccc(X, 'srt', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
srt_vec <- function(X, ...) {uj::mmm_ccc(X, 'srt', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
srt_vls <- function(X, ...) {uj::mmm_ccc(X, 'srt', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
str_arr <- function(X, ...) {uj::mmm_ccc(X, 'str', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
str_dtf <- function(X, ...) {uj::mmm_ccc(X, 'str', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
str_gen <- function(X, ...) {uj::mmm_ccc(X, 'str', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
str_mat <- function(X, ...) {uj::mmm_ccc(X, 'str', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
str_mvc <- function(X, ...) {uj::mmm_ccc(X, 'str', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
str_scl <- function(X, ...) {uj::mmm_ccc(X, 'str', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
str_vec <- function(X, ...) {uj::mmm_ccc(X, 'str', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
str_vls <- function(X, ...) {uj::mmm_ccc(X, 'str', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
uno_arr <- function(X, ...) {uj::mmm_ccc(X, 'uno', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
uno_dtf <- function(X, ...) {uj::mmm_ccc(X, 'uno', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
uno_gen <- function(X, ...) {uj::mmm_ccc(X, 'uno', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
uno_mat <- function(X, ...) {uj::mmm_ccc(X, 'uno', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
uno_mvc <- function(X, ...) {uj::mmm_ccc(X, 'uno', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
uno_scl <- function(X, ...) {uj::mmm_ccc(X, 'uno', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
uno_vec <- function(X, ...) {uj::mmm_ccc(X, 'uno', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
uno_vls <- function(X, ...) {uj::mmm_ccc(X, 'uno', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
whl_arr <- function(X, ...) {uj::mmm_ccc(X, 'whl', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
whl_dtf <- function(X, ...) {uj::mmm_ccc(X, 'whl', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
whl_gen <- function(X, ...) {uj::mmm_ccc(X, 'whl', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
whl_mat <- function(X, ...) {uj::mmm_ccc(X, 'whl', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
whl_mvc <- function(X, ...) {uj::mmm_ccc(X, 'whl', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
whl_scl <- function(X, ...) {uj::mmm_ccc(X, 'whl', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
whl_vec <- function(X, ...) {uj::mmm_ccc(X, 'whl', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
whl_vls <- function(X, ...) {uj::mmm_ccc(X, 'whl', 'vls', ...)}
