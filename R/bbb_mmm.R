#' @encoding UTF-8
#' @family properties
#' @title basic + xmode combination properties
#' @description Check for combinations of \link[=bbb]{basic} and \link[=mmm]{xmode} properties.
#' @details
#' \tabular{ll}{  `bbb_mmm_funs`   \tab What \link[=bbb]{basic} + \link[=mmm]{xmode} combination property functions are there?                    \cr   \tab   \cr
#'                `{bbb}_{mmm}`    \tab Is `X` a match to the single basic property `'{bbb}'` and single xmode property `'{mmm}'`? (where `{bbb}`
#'                                      and `{mmm}` are placeholders for any given basic property and any given xmode property, respectively).    \cr   \tab   \cr
#'                `bbb_mmm`        \tab Is `X` a match to single basic and xmode properties in `BBB` and `MMM`, respectively?                                    }
#' @param X An R object.
#' @param BBB A character scalar single basic property from `c('atm', 'pop')`.
#' @param MMM A character scalar single xmode property from \code{\link{mmm_props}()}.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return **A character vector** \cr\cr `bbb_mmm_funs`
#' \cr\cr  **A logical scalar**   \cr\cr `{bbb}_{mmm}, bbb_mmm`
#' @examples
#' bbb_mmm_funs()
#' bbb_mmm_props()
#' bbb_mmm(letters, "atm", "ch1")
#' atm_ch1(letters)
#' pop_psw(1:10)
#' pop_psw(0:10)
#'
#' @export
bbb_mmm <- function(X, BBB, MMM, ...) {
  bbbValid <- base::c("atm", "pop")
  mmmValid <- uj::v(mmm)
  if (base::is.character(BBB)) {BBB <- base::tolower(BBB)}
  if (base::is.character(MMM)) {MMM <- base::tolower(MMM)}
  Errors <- uj:::.meets_errs(X, ...)
  ErrBBB <- "[bbb] is not a scalar value from c('atm', 'pop')."
  ErrMMM <- "[MMM] is not a scalar value from mmm_props()."
  if (base::length(BBB) == 1) {Errors <- base::c(Errors, ErrBBB)} else if (!(BBB %in% bbbValid)) {Errors <- base::c(Errors, ErrBBB)}
  if (base::length(MMM) == 1) {Errors <- base::c(Errors, ErrMMM)} else if (!(MMM %in% mmmValid)) {Errors <- base::c(Errors, ErrMMM)}
  if (!base::is.null(Errors)) {uj::stopperrs(Errors, PKG = "uj")}
  if (!uj::meets(X, ...)) {F}
  else if (BBB == "atm") {if (!base::is.atomic(X)) {F} else {uj::run("uj:::.", base::toupper(MMM), "(X)")}}
  else if (BBB == "pop") {if (!base::is.atomic(X)) {F} else if (base::length(X) == 0) {F} else {uj::run("uj:::.", base::toupper(MMM), "(X)")}}
  else {F}
}

#' @rdname bbb_mmm
#' @export
bbb_mmm_funs <- function() {
  MMM <- uj::v(MMM)
  MMM <- MMM[MMM != "atm"]
  base::c(base::paste0("atm_", MMM), "pop_atm", base::paste0("pop_", MMM))
}

#' @rdname bbb_mmm
#' @export
atm_ch1 <- function(X, ...) {uj::bbb_mmm(X, 'atm', 'ch1', ...)}

#' @rdname bbb_mmm
#' @export
atm_ch3 <- function(X, ...) {uj::bbb_mmm(X, 'atm', 'ch3', ...)}

#' @rdname bbb_mmm
#' @export
atm_chr <- function(X, ...) {uj::bbb_mmm(X, 'atm', 'chr', ...)}

#' @rdname bbb_mmm
#' @export
atm_clr <- function(X, ...) {uj::bbb_mmm(X, 'atm', 'clr', ...)}

#' @rdname bbb_mmm
#' @export
atm_evn <- function(X, ...) {uj::bbb_mmm(X, 'atm', 'evn', ...)}

#' @rdname bbb_mmm
#' @export
atm_fac <- function(X, ...) {uj::bbb_mmm(X, 'atm', 'fac', ...)}

#' @rdname bbb_mmm
#' @export
atm_frc <- function(X, ...) {uj::bbb_mmm(X, 'atm', 'frc', ...)}

#' @rdname bbb_mmm
#' @export
atm_ind <- function(X, ...) {uj::bbb_mmm(X, 'atm', 'ind', ...)}

#' @rdname bbb_mmm
#' @export
atm_lgl <- function(X, ...) {uj::bbb_mmm(X, 'atm', 'lgl', ...)}

#' @rdname bbb_mmm
#' @export
atm_neg <- function(X, ...) {uj::bbb_mmm(X, 'atm', 'neg', ...)}

#' @rdname bbb_mmm
#' @export
atm_ngw <- function(X, ...) {uj::bbb_mmm(X, 'atm', 'ngw', ...)}

#' @rdname bbb_mmm
#' @export
atm_nng <- function(X, ...) {uj::bbb_mmm(X, 'atm', 'nng', ...)}

#' @rdname bbb_mmm
#' @export
atm_nnw <- function(X, ...) {uj::bbb_mmm(X, 'atm', 'nnw', ...)}

#' @rdname bbb_mmm
#' @export
atm_nps <- function(X, ...) {uj::bbb_mmm(X, 'atm', 'nps', ...)}

#' @rdname bbb_mmm
#' @export
atm_npw <- function(X, ...) {uj::bbb_mmm(X, 'atm', 'npw', ...)}

#' @rdname bbb_mmm
#' @export
atm_nst <- function(X, ...) {uj::bbb_mmm(X, 'atm', 'nst', ...)}

#' @rdname bbb_mmm
#' @export
atm_num <- function(X, ...) {uj::bbb_mmm(X, 'atm', 'num', ...)}

#' @rdname bbb_mmm
#' @export
atm_odd <- function(X, ...) {uj::bbb_mmm(X, 'atm', 'odd', ...)}

#' @rdname bbb_mmm
#' @export
atm_ord <- function(X, ...) {uj::bbb_mmm(X, 'atm', 'ord', ...)}

#' @rdname bbb_mmm
#' @export
atm_pct <- function(X, ...) {uj::bbb_mmm(X, 'atm', 'pct', ...)}

#' @rdname bbb_mmm
#' @export
atm_pos <- function(X, ...) {uj::bbb_mmm(X, 'atm', 'pos', ...)}

#' @rdname bbb_mmm
#' @export
atm_ppn <- function(X, ...) {uj::bbb_mmm(X, 'atm', 'ppn', ...)}

#' @rdname bbb_mmm
#' @export
atm_psw <- function(X, ...) {uj::bbb_mmm(X, 'atm', 'psw', ...)}

#' @rdname bbb_mmm
#' @export
atm_srt <- function(X, ...) {uj::bbb_mmm(X, 'atm', 'srt', ...)}

#' @rdname bbb_mmm
#' @export
atm_str <- function(X, ...) {uj::bbb_mmm(X, 'atm', 'str', ...)}

#' @rdname bbb_mmm
#' @export
atm_uno <- function(X, ...) {uj::bbb_mmm(X, 'atm', 'uno', ...)}

#' @rdname bbb_mmm
#' @export
atm_whl <- function(X, ...) {uj::bbb_mmm(X, 'atm', 'whl', ...)}

#' @rdname bbb_mmm
#' @export
pop_atm <- function(X, ...) {uj::bbb_mmm(X, 'pop', 'atm', ...)}

#' @rdname bbb_mmm
#' @export
pop_ch1 <- function(X, ...) {uj::bbb_mmm(X, 'pop', 'ch1', ...)}

#' @rdname bbb_mmm
#' @export
pop_ch3 <- function(X, ...) {uj::bbb_mmm(X, 'pop', 'ch3', ...)}

#' @rdname bbb_mmm
#' @export
pop_chr <- function(X, ...) {uj::bbb_mmm(X, 'pop', 'chr', ...)}

#' @rdname bbb_mmm
#' @export
pop_clr <- function(X, ...) {uj::bbb_mmm(X, 'pop', 'clr', ...)}

#' @rdname bbb_mmm
#' @export
pop_evn <- function(X, ...) {uj::bbb_mmm(X, 'pop', 'evn', ...)}

#' @rdname bbb_mmm
#' @export
pop_fac <- function(X, ...) {uj::bbb_mmm(X, 'pop', 'fac', ...)}

#' @rdname bbb_mmm
#' @export
pop_frc <- function(X, ...) {uj::bbb_mmm(X, 'pop', 'frc', ...)}

#' @rdname bbb_mmm
#' @export
pop_ind <- function(X, ...) {uj::bbb_mmm(X, 'pop', 'ind', ...)}

#' @rdname bbb_mmm
#' @export
pop_lgl <- function(X, ...) {uj::bbb_mmm(X, 'pop', 'lgl', ...)}

#' @rdname bbb_mmm
#' @export
pop_neg <- function(X, ...) {uj::bbb_mmm(X, 'pop', 'neg', ...)}

#' @rdname bbb_mmm
#' @export
pop_ngw <- function(X, ...) {uj::bbb_mmm(X, 'pop', 'ngw', ...)}

#' @rdname bbb_mmm
#' @export
pop_nng <- function(X, ...) {uj::bbb_mmm(X, 'pop', 'nng', ...)}

#' @rdname bbb_mmm
#' @export
pop_nnw <- function(X, ...) {uj::bbb_mmm(X, 'pop', 'nnw', ...)}

#' @rdname bbb_mmm
#' @export
pop_nps <- function(X, ...) {uj::bbb_mmm(X, 'pop', 'nps', ...)}

#' @rdname bbb_mmm
#' @export
pop_npw <- function(X, ...) {uj::bbb_mmm(X, 'pop', 'npw', ...)}

#' @rdname bbb_mmm
#' @export
pop_nst <- function(X, ...) {uj::bbb_mmm(X, 'pop', 'nst', ...)}

#' @rdname bbb_mmm
#' @export
pop_num <- function(X, ...) {uj::bbb_mmm(X, 'pop', 'num', ...)}

#' @rdname bbb_mmm
#' @export
pop_odd <- function(X, ...) {uj::bbb_mmm(X, 'pop', 'odd', ...)}

#' @rdname bbb_mmm
#' @export
pop_ord <- function(X, ...) {uj::bbb_mmm(X, 'pop', 'ord', ...)}

#' @rdname bbb_mmm
#' @export
pop_pct <- function(X, ...) {uj::bbb_mmm(X, 'pop', 'pct', ...)}

#' @rdname bbb_mmm
#' @export
pop_pos <- function(X, ...) {uj::bbb_mmm(X, 'pop', 'pos', ...)}

#' @rdname bbb_mmm
#' @export
pop_ppn <- function(X, ...) {uj::bbb_mmm(X, 'pop', 'ppn', ...)}

#' @rdname bbb_mmm
#' @export
pop_psw <- function(X, ...) {uj::bbb_mmm(X, 'pop', 'psw', ...)}

#' @rdname bbb_mmm
#' @export
pop_srt <- function(X, ...) {uj::bbb_mmm(X, 'pop', 'srt', ...)}

#' @rdname bbb_mmm
#' @export
pop_str <- function(X, ...) {uj::bbb_mmm(X, 'pop', 'str', ...)}

#' @rdname bbb_mmm
#' @export
pop_uno <- function(X, ...) {uj::bbb_mmm(X, 'pop', 'uno', ...)}

#' @rdname bbb_mmm
#' @export
pop_whl <- function(X, ...) {uj::bbb_mmm(X, 'pop', 'whl', ...)}
