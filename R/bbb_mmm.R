#' @encoding UTF-8
#' @family properties
#' @title Combination Basic Plus Extended Mode Properties
#' @description Check for combinations of \link[=bbb]{basic} and \link[=mmm]{extended mode} properties.
#' @param x An R object.
#' @param bbb A character scalar single basic property from `c('atm', 'pop')`.
#' @param mmm  A character scalar single extended mode property from \code{\link{mmm_props}()}.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @examples
#' bbb_mmm_funs()
#' bbb_mmm(letters, "atm", "ch1")
#' atm_ch1(letters)
#' pop_psw(1:10)
#' pop_psw(0:10)
#' @export
bbb_mmm_prop_funs <- function() {utils::help("bbb_mmm_prop_funs", package = "uj")}

#' @describeIn bbb_mmm_prop_funs Checks `x` against the basic property in `bbb` and the extended mode property in `mmm ` subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
bbb_mmm <- function(x, bbb, mmm , ...) {
  bbbValid <- base::c("atm", "pop")
  mmmValid <- uj::mmm_props()
  if (base::is.character(bbb)) {bbb <- base::tolower(bbb)}
  if (base::is.character(mmm)) {mmm <- base::tolower(mmm)}
  errs <- uj::meets_errs(x, ...)
  errBBB <- "[bbb] is not a scalar value from c('atm', 'pop')."
  errMMM <- "[mmm ] is not a scalar value from mmm_props()."
  if (base::length(bbb) != 1) {errs <- base::c(errs, errBBB)} else if (!(bbb %in% bbbValid)) {errs <- base::c(errs, errBBB)}
  if (base::length(mmm) != 1) {errs <- base::c(errs, errMMM)} else if (!(mmm %in% mmmValid)) {errs <- base::c(errs, errMMM)}
  if (!base::is.null(errs)) {uj::stopperr(errs)}
  if (!uj::meets(x, ...)) {return(F)}
  if (!base::is.atomic(x)) {return(F)}
  if (bbb != "pop" & bbb != "atm") {return(F)}
  if (bbb == "pop" & base::length(x) == 0) {return(F)}
  uj::run("uj::.", base::toupper(mmm), "(x)")
}

#' @describeIn bbb_mmm_prop_funs Lists all combo basic plus extended mode property checking functions. Returns a character vector.
#' @export
bbb_mmm_funs <- function() {
  mmm <- uj::mmm_props()
  mmm <- mmm[mmm != "atm"]
  base::c(base::paste0("atm_", mmm), "pop_atm", base::paste0("pop_", mmm))
}

#' @describeIn bbb_mmm_prop_funs Lists all combo basic plus extended mode properties.
#' @export
bbb_mmm_props <- function() {
  mmm <- uj::mmm_props()
  mmm <- mmm[mmm != "atm"]
  base::c(base::paste0("atm_", mmm), "pop_atm", base::paste0("pop_", mmm))
}

#' @describeIn bbb_mmm_prop_funs Checks `x` for atomic-ness and onechar-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
atm_ch1 <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'ch1', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for atomic-ness and threechar-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
atm_ch3 <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'ch3', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for atomic-ness and character-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
atm_chr <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'chr', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for atomic-ness and color-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
atm_clr <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'clr', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for atomic-ness and correlation-valued-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
atm_cor <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'cor', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for atomic-ness and even-whole-number-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
atm_evn <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'evn', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for atomic-ness and factor-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
atm_fac <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'fac', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for atomic-ness and fractional-numeric-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
atm_frc <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'frc', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for atomic-ness and indexer-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
atm_ind <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'ind', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for atomic-ness and logical-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
atm_lgl <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'lgl', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for atomic-ness and negative-numeric-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
atm_neg <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'neg', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for atomic-ness and negative-whole-number-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
atm_ngw <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'ngw', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for atomic-ness and non-negative-numeric-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
atm_nng <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'nng', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for atomic-ness and non-negative-whole-number-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
atm_nnw <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'nnw', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for atomic-ness and non-positive-numeric-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
atm_nps <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'nps', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for atomic-ness and non-positive-whole-number-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
atm_npw <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'npw', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for atomic-ness and non-sortable-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
atm_nst <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'nst', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for atomic-ness and numeric-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
atm_num <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'num', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for atomic-ness and odd-whole-number-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
atm_odd <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'odd', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for atomic-ness and ordered-factor-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
atm_ord <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'ord', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for atomic-ness and percent-valued-numeric-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
atm_pct <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'pct', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for atomic-ness and positive-numeric-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
atm_pos <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'pos', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for atomic-ness and proportion-valued-numeric-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
atm_ppn <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'ppn', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for atomic-ness and positive-whole-number-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
atm_psw <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'psw', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for atomic-ness and sortable-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
atm_srt <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'srt', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for atomic-ness and string-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
atm_str <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'str', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for atomic-ness and unordered-factor-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
atm_uno <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'uno', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for atomic-ness and whole-number-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
atm_whl <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'whl', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for populated-ness and atomic-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
pop_atm <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'atm', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for populated-ness and onechar-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
pop_ch1 <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'ch1', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for populated-ness and threechar-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
pop_ch3 <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'ch3', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for populated-ness and character-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
pop_chr <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'chr', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for populated-ness and color-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
pop_clr <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'clr', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for populated-ness and correlation-valued-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
pop_cor <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'cor', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for populated-ness and even-whole-number-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
pop_evn <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'evn', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for populated-ness and factor-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
pop_fac <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'fac', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for populated-ness and fractional-numeric-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
pop_frc <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'frc', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for populated-ness and indexer-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
pop_ind <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'ind', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for populated-ness and logical-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
pop_lgl <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'lgl', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for populated-ness and negative-numeric-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
pop_neg <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'neg', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for populated-ness and negative-whole-number-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
pop_ngw <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'ngw', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for populated-ness and non-negative-numeric-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
pop_nng <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'nng', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for populated-ness and non-negative-whole-number-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
pop_nnw <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'nnw', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for populated-ness and non-positive-numeric-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
pop_nps <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'nps', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for populated-ness and non-positive-whole-number-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
pop_npw <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'npw', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for populated-ness and non-sortable-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
pop_nst <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'nst', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for populated-ness and numeric-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
pop_num <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'num', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for populated-ness and odd-whole-number-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
pop_odd <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'odd', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for populated-ness and ordered-factor-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
pop_ord <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'ord', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for populated-ness and percent-valued-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
pop_pct <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'pct', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for populated-ness and positive-number-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
pop_pos <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'pos', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for populated-ness and proportion-valued-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
pop_ppn <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'ppn', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for populated-ness and positive-whole-number-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
pop_psw <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'psw', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for populated-ness and sortable-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
pop_srt <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'srt', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for populated-ness and string-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
pop_str <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'str', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for populated-ness and unordered-factor-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
pop_uno <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'uno', ...)}

#' @describeIn bbb_mmm_prop_funs Checks `x` for populated-ness and whole-number-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
pop_whl <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'whl', ...)}
