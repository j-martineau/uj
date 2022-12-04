#' @family props
#' @title Extended mode + extended class properties
#' @description \tabular{rl}{
#'   `mmm_ccc_props`   \tab Gets a character vector of all possible \link[=mmm]{extended mode} + \link[=ccc]{extended class} properties.
#'   \cr               \tab  
#'   \cr   `mmm_ccc`   \tab Evaluates whether `x` matches the extended mode specified in `mmm` and the extended class in argument `ccc`, subject to any restrictions in `...`.
#'   \cr               \tab  
#'   \cr   `xxx_yyy`   \tab Evaluates whether `x` matches extended mode `xxx`\eqn{^1} and extended class `yyy`\eqn{^2} subject to any restrictions in `...`.
#' }
#'            \eqn{^1} A placeholder for any given extended mode.
#' \cr\cr     \eqn{^2} A placeholder for any given extended class.
#' @param x An R object.
#' @param mmm A character scalar extended mode property from `mmm_props()`.
#' @param ccc A character scalar extended class property from `ccc_props()`.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return \tabular{rl}{
#'     `mmm_ccc,MMM_CCC`   \tab A logical scalar.
#'   \cr `mmm_ccc_props`   \tab A character vector.
#' }
#' @examples
#' scalar1 <- "a"
#' lst2x26 <- list(letters = letters, LETTERS = LETTERS)
#'
#' c(mmm_ccc(lst2x26, "chr", "vls"), chr_vls(lst2x26))
#' c(mmm_ccc(lst2x26, "num", "mat"), num_mat(lst2x26))
#' c(mmm_ccc(scalar1, "ch1", "scl"), ch1_scl(scalar1))
#' c(mmm_ccc(letters, "ch1", "vec"), ch1_vec(letters))
#' c(mmm_ccc(letters, "num", "dtf"), num_dtf(letters))
#'
#' mmm_ccc_props()
#' @export
mmm_ccc <- function(x, mmm, ccc, ...) {
  errs <- c(.meets_errs(x, ...),
            f0(f0(length(mmm) != 1 | !is.character(mmm), F, f0(is.na(mmm), F, mmm %in% .mmms)), NULL, '\n \u2022 [mmm] is not a scalar value from mmm_props().'),
            f0(f0(length(ccc) != 1 | !is.character(ccc), F, f0(is.na(ccc), F, ccc %in% .cccs)), NULL, '\n \u2022 [ccc] is not a scalar value from ccc_props().'))
  if (!is.null(errs)) {stop(errs)}
  if (!meets(x, ...)) {return(F)}
  if (!run(paste0('.i', ccc, '(x)'))) {return(F)}
  mfun <- paste0('.i', mmm)
  if (ccc == 'dtf') {all(apply(x, 2, mfun))}
  else if (ccc == 'vls') {all(sapply(x, mfun))}
  else if (!is.atomic(x)) {F}
  else {run(mfun, "(x)")}
}

#' @rdname mmm_ccc
#' @export
mmm_ccc_props <- function() {sort(av(apply(expand.grid(mmm = .mmms, ccc = .cccs), 1, paste0, collapse = '_')))}

#' @rdname mmm_ccc
#' @export
ch1_arr <- function(x, ...) {mmm_ccc(x, 'ch1', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
ch1_dtf <- function(x, ...) {mmm_ccc(x, 'ch1', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
ch1_gen <- function(x, ...) {mmm_ccc(x, 'ch1', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
ch1_mat <- function(x, ...) {mmm_ccc(x, 'ch1', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
ch1_mvc <- function(x, ...) {mmm_ccc(x, 'ch1', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
ch1_scl <- function(x, ...) {mmm_ccc(x, 'ch1', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
ch1_vec <- function(x, ...) {mmm_ccc(x, 'ch1', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
ch1_vls <- function(x, ...) {mmm_ccc(x, 'ch1', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
ch3_arr <- function(x, ...) {mmm_ccc(x, 'ch3', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
ch3_dtf <- function(x, ...) {mmm_ccc(x, 'ch3', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
ch3_gen <- function(x, ...) {mmm_ccc(x, 'ch3', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
ch3_mat <- function(x, ...) {mmm_ccc(x, 'ch3', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
ch3_mvc <- function(x, ...) {mmm_ccc(x, 'ch3', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
ch3_scl <- function(x, ...) {mmm_ccc(x, 'ch3', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
ch3_vec <- function(x, ...) {mmm_ccc(x, 'ch3', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
ch3_vls <- function(x, ...) {mmm_ccc(x, 'ch3', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
chr_arr <- function(x, ...) {mmm_ccc(x, 'chr', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
chr_dtf <- function(x, ...) {mmm_ccc(x, 'chr', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
chr_gen <- function(x, ...) {mmm_ccc(x, 'chr', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
chr_mat <- function(x, ...) {mmm_ccc(x, 'chr', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
chr_mvc <- function(x, ...) {mmm_ccc(x, 'chr', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
chr_scl <- function(x, ...) {mmm_ccc(x, 'chr', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
chr_vec <- function(x, ...) {mmm_ccc(x, 'chr', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
chr_vls <- function(x, ...) {mmm_ccc(x, 'chr', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
clr_arr <- function(x, ...) {mmm_ccc(x, 'clr', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
clr_dtf <- function(x, ...) {mmm_ccc(x, 'clr', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
clr_gen <- function(x, ...) {mmm_ccc(x, 'clr', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
clr_mat <- function(x, ...) {mmm_ccc(x, 'clr', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
clr_mvc <- function(x, ...) {mmm_ccc(x, 'clr', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
clr_scl <- function(x, ...) {mmm_ccc(x, 'clr', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
clr_vec <- function(x, ...) {mmm_ccc(x, 'clr', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
clr_vls <- function(x, ...) {mmm_ccc(x, 'clr', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
evn_arr <- function(x, ...) {mmm_ccc(x, 'evn', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
evn_dtf <- function(x, ...) {mmm_ccc(x, 'evn', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
evn_gen <- function(x, ...) {mmm_ccc(x, 'evn', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
evn_mat <- function(x, ...) {mmm_ccc(x, 'evn', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
evn_mvc <- function(x, ...) {mmm_ccc(x, 'evn', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
evn_scl <- function(x, ...) {mmm_ccc(x, 'evn', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
evn_vec <- function(x, ...) {mmm_ccc(x, 'evn', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
evn_vls <- function(x, ...) {mmm_ccc(x, 'evn', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
fac_arr <- function(x, ...) {mmm_ccc(x, 'fac', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
fac_dtf <- function(x, ...) {mmm_ccc(x, 'fac', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
fac_gen <- function(x, ...) {mmm_ccc(x, 'fac', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
fac_mat <- function(x, ...) {mmm_ccc(x, 'fac', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
fac_mvc <- function(x, ...) {mmm_ccc(x, 'fac', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
fac_scl <- function(x, ...) {mmm_ccc(x, 'fac', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
fac_vec <- function(x, ...) {mmm_ccc(x, 'fac', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
fac_vls <- function(x, ...) {mmm_ccc(x, 'fac', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
frc_arr <- function(x, ...) {mmm_ccc(x, 'frc', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
frc_dtf <- function(x, ...) {mmm_ccc(x, 'frc', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
frc_gen <- function(x, ...) {mmm_ccc(x, 'frc', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
frc_mat <- function(x, ...) {mmm_ccc(x, 'frc', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
frc_mvc <- function(x, ...) {mmm_ccc(x, 'frc', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
frc_scl <- function(x, ...) {mmm_ccc(x, 'frc', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
frc_vec <- function(x, ...) {mmm_ccc(x, 'frc', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
frc_vls <- function(x, ...) {mmm_ccc(x, 'frc', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
ind_arr <- function(x, ...) {mmm_ccc(x, 'ind', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
ind_dtf <- function(x, ...) {mmm_ccc(x, 'ind', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
ind_gen <- function(x, ...) {mmm_ccc(x, 'ind', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
ind_mat <- function(x, ...) {mmm_ccc(x, 'ind', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
ind_mvc <- function(x, ...) {mmm_ccc(x, 'ind', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
ind_scl <- function(x, ...) {mmm_ccc(x, 'ind', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
ind_vec <- function(x, ...) {mmm_ccc(x, 'ind', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
ind_vls <- function(x, ...) {mmm_ccc(x, 'ind', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
lgl_arr <- function(x, ...) {mmm_ccc(x, 'lgl', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
lgl_dtf <- function(x, ...) {mmm_ccc(x, 'lgl', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
lgl_gen <- function(x, ...) {mmm_ccc(x, 'lgl', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
lgl_mat <- function(x, ...) {mmm_ccc(x, 'lgl', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
lgl_mvc <- function(x, ...) {mmm_ccc(x, 'lgl', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
lgl_scl <- function(x, ...) {mmm_ccc(x, 'lgl', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
lgl_vec <- function(x, ...) {mmm_ccc(x, 'lgl', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
lgl_vls <- function(x, ...) {mmm_ccc(x, 'lgl', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
neg_arr <- function(x, ...) {mmm_ccc(x, 'neg', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
neg_dtf <- function(x, ...) {mmm_ccc(x, 'neg', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
neg_gen <- function(x, ...) {mmm_ccc(x, 'neg', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
neg_mat <- function(x, ...) {mmm_ccc(x, 'neg', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
neg_mvc <- function(x, ...) {mmm_ccc(x, 'neg', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
neg_scl <- function(x, ...) {mmm_ccc(x, 'neg', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
neg_vec <- function(x, ...) {mmm_ccc(x, 'neg', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
neg_vls <- function(x, ...) {mmm_ccc(x, 'neg', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
ngw_arr <- function(x, ...) {mmm_ccc(x, 'ngw', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
ngw_dtf <- function(x, ...) {mmm_ccc(x, 'ngw', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
ngw_gen <- function(x, ...) {mmm_ccc(x, 'ngw', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
ngw_mat <- function(x, ...) {mmm_ccc(x, 'ngw', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
ngw_mvc <- function(x, ...) {mmm_ccc(x, 'ngw', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
ngw_scl <- function(x, ...) {mmm_ccc(x, 'ngw', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
ngw_vec <- function(x, ...) {mmm_ccc(x, 'ngw', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
ngw_vls <- function(x, ...) {mmm_ccc(x, 'ngw', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
nng_arr <- function(x, ...) {mmm_ccc(x, 'nng', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
nng_dtf <- function(x, ...) {mmm_ccc(x, 'nng', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
nng_gen <- function(x, ...) {mmm_ccc(x, 'nng', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
nng_mat <- function(x, ...) {mmm_ccc(x, 'nng', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
nng_mvc <- function(x, ...) {mmm_ccc(x, 'nng', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
nng_scl <- function(x, ...) {mmm_ccc(x, 'nng', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
nng_vec <- function(x, ...) {mmm_ccc(x, 'nng', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
nng_vls <- function(x, ...) {mmm_ccc(x, 'nng', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
nnw_arr <- function(x, ...) {mmm_ccc(x, 'nnw', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
nnw_dtf <- function(x, ...) {mmm_ccc(x, 'nnw', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
nnw_gen <- function(x, ...) {mmm_ccc(x, 'nnw', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
nnw_mat <- function(x, ...) {mmm_ccc(x, 'nnw', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
nnw_mvc <- function(x, ...) {mmm_ccc(x, 'nnw', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
nnw_scl <- function(x, ...) {mmm_ccc(x, 'nnw', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
nnw_vec <- function(x, ...) {mmm_ccc(x, 'nnw', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
nnw_vls <- function(x, ...) {mmm_ccc(x, 'nnw', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
nps_arr <- function(x, ...) {mmm_ccc(x, 'nps', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
nps_dtf <- function(x, ...) {mmm_ccc(x, 'nps', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
nps_gen <- function(x, ...) {mmm_ccc(x, 'nps', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
nps_mat <- function(x, ...) {mmm_ccc(x, 'nps', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
nps_mvc <- function(x, ...) {mmm_ccc(x, 'nps', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
nps_scl <- function(x, ...) {mmm_ccc(x, 'nps', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
nps_vec <- function(x, ...) {mmm_ccc(x, 'nps', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
nps_vls <- function(x, ...) {mmm_ccc(x, 'nps', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
npw_arr <- function(x, ...) {mmm_ccc(x, 'npw', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
npw_dtf <- function(x, ...) {mmm_ccc(x, 'npw', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
npw_gen <- function(x, ...) {mmm_ccc(x, 'npw', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
npw_mat <- function(x, ...) {mmm_ccc(x, 'npw', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
npw_mvc <- function(x, ...) {mmm_ccc(x, 'npw', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
npw_scl <- function(x, ...) {mmm_ccc(x, 'npw', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
npw_vec <- function(x, ...) {mmm_ccc(x, 'npw', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
npw_vls <- function(x, ...) {mmm_ccc(x, 'npw', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
nst_arr <- function(x, ...) {mmm_ccc(x, 'nst', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
nst_dtf <- function(x, ...) {mmm_ccc(x, 'nst', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
nst_gen <- function(x, ...) {mmm_ccc(x, 'nst', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
nst_mat <- function(x, ...) {mmm_ccc(x, 'nst', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
nst_mvc <- function(x, ...) {mmm_ccc(x, 'nst', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
nst_scl <- function(x, ...) {mmm_ccc(x, 'nst', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
nst_vec <- function(x, ...) {mmm_ccc(x, 'nst', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
nst_vls <- function(x, ...) {mmm_ccc(x, 'nst', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
num_arr <- function(x, ...) {mmm_ccc(x, 'num', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
num_dtf <- function(x, ...) {mmm_ccc(x, 'num', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
num_gen <- function(x, ...) {mmm_ccc(x, 'num', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
num_mat <- function(x, ...) {mmm_ccc(x, 'num', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
num_mvc <- function(x, ...) {mmm_ccc(x, 'num', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
num_scl <- function(x, ...) {mmm_ccc(x, 'num', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
num_vec <- function(x, ...) {mmm_ccc(x, 'num', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
num_vls <- function(x, ...) {mmm_ccc(x, 'num', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
odd_arr <- function(x, ...) {mmm_ccc(x, 'odd', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
odd_dtf <- function(x, ...) {mmm_ccc(x, 'odd', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
odd_gen <- function(x, ...) {mmm_ccc(x, 'odd', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
odd_mat <- function(x, ...) {mmm_ccc(x, 'odd', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
odd_mvc <- function(x, ...) {mmm_ccc(x, 'odd', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
odd_scl <- function(x, ...) {mmm_ccc(x, 'odd', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
odd_vec <- function(x, ...) {mmm_ccc(x, 'odd', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
odd_vls <- function(x, ...) {mmm_ccc(x, 'odd', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
ord_arr <- function(x, ...) {mmm_ccc(x, 'ord', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
ord_dtf <- function(x, ...) {mmm_ccc(x, 'ord', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
ord_gen <- function(x, ...) {mmm_ccc(x, 'ord', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
ord_mat <- function(x, ...) {mmm_ccc(x, 'ord', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
ord_mvc <- function(x, ...) {mmm_ccc(x, 'ord', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
ord_scl <- function(x, ...) {mmm_ccc(x, 'ord', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
ord_vec <- function(x, ...) {mmm_ccc(x, 'ord', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
ord_vls <- function(x, ...) {mmm_ccc(x, 'ord', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
pct_arr <- function(x, ...) {mmm_ccc(x, 'pct', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
pct_dtf <- function(x, ...) {mmm_ccc(x, 'pct', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
pct_gen <- function(x, ...) {mmm_ccc(x, 'pct', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
pct_mat <- function(x, ...) {mmm_ccc(x, 'pct', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
pct_mvc <- function(x, ...) {mmm_ccc(x, 'pct', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
pct_scl <- function(x, ...) {mmm_ccc(x, 'pct', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
pct_vec <- function(x, ...) {mmm_ccc(x, 'pct', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
pct_vls <- function(x, ...) {mmm_ccc(x, 'pct', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
pos_arr <- function(x, ...) {mmm_ccc(x, 'pos', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
pos_dtf <- function(x, ...) {mmm_ccc(x, 'pos', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
pos_gen <- function(x, ...) {mmm_ccc(x, 'pos', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
pos_mat <- function(x, ...) {mmm_ccc(x, 'pos', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
pos_mvc <- function(x, ...) {mmm_ccc(x, 'pos', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
pos_scl <- function(x, ...) {mmm_ccc(x, 'pos', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
pos_vec <- function(x, ...) {mmm_ccc(x, 'pos', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
pos_vls <- function(x, ...) {mmm_ccc(x, 'pos', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
ppn_arr <- function(x, ...) {mmm_ccc(x, 'ppn', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
ppn_dtf <- function(x, ...) {mmm_ccc(x, 'ppn', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
ppn_gen <- function(x, ...) {mmm_ccc(x, 'ppn', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
ppn_mat <- function(x, ...) {mmm_ccc(x, 'ppn', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
ppn_mvc <- function(x, ...) {mmm_ccc(x, 'ppn', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
ppn_scl <- function(x, ...) {mmm_ccc(x, 'ppn', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
ppn_vec <- function(x, ...) {mmm_ccc(x, 'ppn', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
ppn_vls <- function(x, ...) {mmm_ccc(x, 'ppn', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
psw_arr <- function(x, ...) {mmm_ccc(x, 'psw', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
psw_dtf <- function(x, ...) {mmm_ccc(x, 'psw', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
psw_gen <- function(x, ...) {mmm_ccc(x, 'psw', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
psw_mat <- function(x, ...) {mmm_ccc(x, 'psw', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
psw_mvc <- function(x, ...) {mmm_ccc(x, 'psw', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
psw_scl <- function(x, ...) {mmm_ccc(x, 'psw', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
psw_vec <- function(x, ...) {mmm_ccc(x, 'psw', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
psw_vls <- function(x, ...) {mmm_ccc(x, 'psw', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
srt_arr <- function(x, ...) {mmm_ccc(x, 'srt', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
srt_dtf <- function(x, ...) {mmm_ccc(x, 'srt', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
srt_gen <- function(x, ...) {mmm_ccc(x, 'srt', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
srt_mat <- function(x, ...) {mmm_ccc(x, 'srt', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
srt_mvc <- function(x, ...) {mmm_ccc(x, 'srt', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
srt_scl <- function(x, ...) {mmm_ccc(x, 'srt', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
srt_vec <- function(x, ...) {mmm_ccc(x, 'srt', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
srt_vls <- function(x, ...) {mmm_ccc(x, 'srt', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
str_arr <- function(x, ...) {mmm_ccc(x, 'str', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
str_dtf <- function(x, ...) {mmm_ccc(x, 'str', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
str_gen <- function(x, ...) {mmm_ccc(x, 'str', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
str_mat <- function(x, ...) {mmm_ccc(x, 'str', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
str_mvc <- function(x, ...) {mmm_ccc(x, 'str', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
str_scl <- function(x, ...) {mmm_ccc(x, 'str', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
str_vec <- function(x, ...) {mmm_ccc(x, 'str', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
str_vls <- function(x, ...) {mmm_ccc(x, 'str', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
uno_arr <- function(x, ...) {mmm_ccc(x, 'uno', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
uno_dtf <- function(x, ...) {mmm_ccc(x, 'uno', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
uno_gen <- function(x, ...) {mmm_ccc(x, 'uno', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
uno_mat <- function(x, ...) {mmm_ccc(x, 'uno', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
uno_mvc <- function(x, ...) {mmm_ccc(x, 'uno', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
uno_scl <- function(x, ...) {mmm_ccc(x, 'uno', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
uno_vec <- function(x, ...) {mmm_ccc(x, 'uno', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
uno_vls <- function(x, ...) {mmm_ccc(x, 'uno', 'vls', ...)}

#' @rdname mmm_ccc
#' @export
whl_arr <- function(x, ...) {mmm_ccc(x, 'whl', 'arr', ...)}

#' @rdname mmm_ccc
#' @export
whl_dtf <- function(x, ...) {mmm_ccc(x, 'whl', 'dtf', ...)}

#' @rdname mmm_ccc
#' @export
whl_gen <- function(x, ...) {mmm_ccc(x, 'whl', 'gen', ...)}

#' @rdname mmm_ccc
#' @export
whl_mat <- function(x, ...) {mmm_ccc(x, 'whl', 'mat', ...)}

#' @rdname mmm_ccc
#' @export
whl_mvc <- function(x, ...) {mmm_ccc(x, 'whl', 'mvc', ...)}

#' @rdname mmm_ccc
#' @export
whl_scl <- function(x, ...) {mmm_ccc(x, 'whl', 'scl', ...)}

#' @rdname mmm_ccc
#' @export
whl_vec <- function(x, ...) {mmm_ccc(x, 'whl', 'vec', ...)}

#' @rdname mmm_ccc
#' @export
whl_vls <- function(x, ...) {mmm_ccc(x, 'whl', 'vls', ...)}
