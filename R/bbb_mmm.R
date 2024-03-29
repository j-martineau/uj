#' @encoding UTF-8
#' @family properties
#' @title basic + xmode combination properties
#' @description Check for combinations of \link[=bbb]{basic} and \link[=mmm]{xmode} properties.
#' @details
#' \tabular{ll}{  `bbb_mmm_funs`   \tab What \link[=bbb]{basic} + \link[=mmm]{xmode} combination property functions are there?                    \cr   \tab   \cr
#'                `{bbb}_{mmm}`    \tab Is `x` a match to the single basic property `'{bbb}'` and single xmode property `'{mmm}'`? (where `{bbb}`
#'                                      and `{mmm}` are placeholders for any given basic property and any given xmode property, respectively).    \cr   \tab   \cr
#'                `bbb_mmm`        \tab Is `x` a match to single basic and xmode properties in `.BBB` and `.MMM`, respectively?                                  }
#' @param x An R object.
#' @param .BBB A character scalar single basic property from `c('atm', 'pop')`.
#' @param .MMM A character scalar single xmode property from \code{\link{mmm_props}()}.
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
bbb_mmm <- function(x, .BBB, .MMM, ...) {
  bbbValid <- base::c("atm", "pop")
  mmmValid <- uj::v(mmm)
  if (base::is.character(.BBB)) {.BBB <- base::tolower(.BBB)}
  if (base::is.character(.MMM)) {.MMM <- base::tolower(.MMM)}
  Errors <- uj:::.meets_errs(x, ...)
  ErrBBB <- "[.BBB] is not a scalar value from c('atm', 'pop')."
  ErrMMM <- "[.MMM] is not a scalar value from mmm_props()."
  if (base::length(.BBB) == 1) {Errors <- base::c(Errors, ErrBBB)} else if (!(.BBB %in% bbbValid)) {Errors <- base::c(Errors, ErrBBB)}
  if (base::length(.MMM) == 1) {Errors <- base::c(Errors, ErrMMM)} else if (!(.MMM %in% mmmValid)) {Errors <- base::c(Errors, ErrMMM)}
  if (!base::is.null(Errors)) {uj::stopperrs(Errors, .PKG = "uj")}
  if (!uj::meets(x, ...)) {F}
  else if (.BBB == "atm") {if (!base::is.atomic(x)) {F} else {uj::run("uj:::.", base::toupper(.MMM), "(x)")}}
  else if (.BBB == "pop") {if (!base::is.atomic(x)) {F} else if (base::length(x) == 0) {F} else {uj::run("uj:::.", base::toupper(.MMM), "(x)")}}
  else {F}
}

#' @rdname bbb_mmm
#' @export
bbb_mmm_funs <- function() {
  mmm <- uj::v(mmm)
  mmm <- mmm[mmm != "atm"]
  base::c(base::paste0("atm_", mmm), "pop_atm", base::paste0("pop_", mmm))
}

#' @rdname bbb_mmm
#' @export
atm_ch1 <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'ch1', ...)}

#' @rdname bbb_mmm
#' @export
atm_ch3 <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'ch3', ...)}

#' @rdname bbb_mmm
#' @export
atm_chr <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'chr', ...)}

#' @rdname bbb_mmm
#' @export
atm_clr <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'clr', ...)}

#' @rdname bbb_mmm
#' @export
atm_evn <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'evn', ...)}

#' @rdname bbb_mmm
#' @export
atm_fac <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'fac', ...)}

#' @rdname bbb_mmm
#' @export
atm_frc <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'frc', ...)}

#' @rdname bbb_mmm
#' @export
atm_ind <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'ind', ...)}

#' @rdname bbb_mmm
#' @export
atm_lgl <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'lgl', ...)}

#' @rdname bbb_mmm
#' @export
atm_neg <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'neg', ...)}

#' @rdname bbb_mmm
#' @export
atm_ngw <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'ngw', ...)}

#' @rdname bbb_mmm
#' @export
atm_nng <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'nng', ...)}

#' @rdname bbb_mmm
#' @export
atm_nnw <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'nnw', ...)}

#' @rdname bbb_mmm
#' @export
atm_nps <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'nps', ...)}

#' @rdname bbb_mmm
#' @export
atm_npw <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'npw', ...)}

#' @rdname bbb_mmm
#' @export
atm_nst <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'nst', ...)}

#' @rdname bbb_mmm
#' @export
atm_num <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'num', ...)}

#' @rdname bbb_mmm
#' @export
atm_odd <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'odd', ...)}

#' @rdname bbb_mmm
#' @export
atm_ord <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'ord', ...)}

#' @rdname bbb_mmm
#' @export
atm_pct <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'pct', ...)}

#' @rdname bbb_mmm
#' @export
atm_pos <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'pos', ...)}

#' @rdname bbb_mmm
#' @export
atm_ppn <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'ppn', ...)}

#' @rdname bbb_mmm
#' @export
atm_psw <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'psw', ...)}

#' @rdname bbb_mmm
#' @export
atm_srt <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'srt', ...)}

#' @rdname bbb_mmm
#' @export
atm_str <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'str', ...)}

#' @rdname bbb_mmm
#' @export
atm_uno <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'uno', ...)}

#' @rdname bbb_mmm
#' @export
atm_whl <- function(x, ...) {uj::bbb_mmm(x, 'atm', 'whl', ...)}

#' @rdname bbb_mmm
#' @export
pop_atm <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'atm', ...)}

#' @rdname bbb_mmm
#' @export
pop_ch1 <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'ch1', ...)}

#' @rdname bbb_mmm
#' @export
pop_ch3 <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'ch3', ...)}

#' @rdname bbb_mmm
#' @export
pop_chr <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'chr', ...)}

#' @rdname bbb_mmm
#' @export
pop_clr <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'clr', ...)}

#' @rdname bbb_mmm
#' @export
pop_evn <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'evn', ...)}

#' @rdname bbb_mmm
#' @export
pop_fac <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'fac', ...)}

#' @rdname bbb_mmm
#' @export
pop_frc <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'frc', ...)}

#' @rdname bbb_mmm
#' @export
pop_ind <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'ind', ...)}

#' @rdname bbb_mmm
#' @export
pop_lgl <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'lgl', ...)}

#' @rdname bbb_mmm
#' @export
pop_neg <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'neg', ...)}

#' @rdname bbb_mmm
#' @export
pop_ngw <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'ngw', ...)}

#' @rdname bbb_mmm
#' @export
pop_nng <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'nng', ...)}

#' @rdname bbb_mmm
#' @export
pop_nnw <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'nnw', ...)}

#' @rdname bbb_mmm
#' @export
pop_nps <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'nps', ...)}

#' @rdname bbb_mmm
#' @export
pop_npw <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'npw', ...)}

#' @rdname bbb_mmm
#' @export
pop_nst <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'nst', ...)}

#' @rdname bbb_mmm
#' @export
pop_num <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'num', ...)}

#' @rdname bbb_mmm
#' @export
pop_odd <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'odd', ...)}

#' @rdname bbb_mmm
#' @export
pop_ord <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'ord', ...)}

#' @rdname bbb_mmm
#' @export
pop_pct <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'pct', ...)}

#' @rdname bbb_mmm
#' @export
pop_pos <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'pos', ...)}

#' @rdname bbb_mmm
#' @export
pop_ppn <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'ppn', ...)}

#' @rdname bbb_mmm
#' @export
pop_psw <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'psw', ...)}

#' @rdname bbb_mmm
#' @export
pop_srt <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'srt', ...)}

#' @rdname bbb_mmm
#' @export
pop_str <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'str', ...)}

#' @rdname bbb_mmm
#' @export
pop_uno <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'uno', ...)}

#' @rdname bbb_mmm
#' @export
pop_whl <- function(x, ...) {uj::bbb_mmm(x, 'pop', 'whl', ...)}
