#' @encoding UTF-8
#' @family properties
#' @title complete + xmode combination properties
#' @description Check for combination of \link[=CMP]{completeness} and \link[=mmm]{xmode}.
#' @details
#' \tabular{ll}{  `cmp_mmm_funs`   \tab What \link[=CMP]{complete} + \link[=mmm]{xmode} combination \link[=prop_funs]{property functions} are there?                         \cr   \tab   \cr
#'                `cmp_{mmm}`      \tab Is `x` both complete and a match to the single xmode property `'{mmm}'` where `{mmm}` is a placeholder for any given xmode property? \cr   \tab   \cr
#'                `cmp_mmm`        \tab Is `x` both complete and a match to single xmode property in arg `mmm`?                                                                             }
#' @param x An R object.
#' @param mmm A character scalar xmode property from `mmm_props()`.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return **A character vector** \cr\cr `cmp_mmm_funs`
#' \cr\cr  **A logical scalar**   \cr\cr `cmp_{mmm}, cmp_mmm`
#' @examples
#' cmp_mmm_funs()
#' cmp_mmm(letters, "ch1")
#' cmp_mmm(c("abc", "def"), "ch3")
#' cmp_psw(0:10)
#' cmp_ord(factor(letters, ordered = T))
#' @export
cmp_mmm <- function(x, mmm, ...) {
  errs <- uj:::.meets_errs(x, ...)
  if (!uj:::.cmp_chr_scl(mmm, valid = base::c(uj:::.mmm, uj:::.MMM))) {errs <- base::c(errs, '[mmm] is not a scalar value from mmm_props().')}
  if (!base::is.null(errs)) {uj::stopperr(errs, PKG = "uj")}
  if (!uj::meets(x, ...)) {F}
  else if (!base::is.atomic(x) | base::length(x) == 0) {F}
  else if (base::any(base::is.na(x))) {F}
  else {uj::run("uj:::.", base::toupper(mmm), "(x)")}
}

#' @rdname cmp_mmm
#' @export
cmp_mmm_funs <- function() {base::paste0('cmp_', uj:::.mmm)}

#' @rdname cmp_mmm
#' @export
cmp_atm <- function(x, ...) {uj::f0(base::is.atomic(x) & base::length(x) > 0, !base::any(base::is.na(x)), F)}

#' @rdname cmp_mmm
#' @export
cmp_ch1 <- function(x, ...) {uj::cmp_mmm(x, 'ch1', ...)}

#' @rdname cmp_mmm
#' @export
cmp_ch3 <- function(x, ...) {uj::cmp_mmm(x, 'ch3', ...)}

#' @rdname cmp_mmm
#' @export
cmp_chr <- function(x, ...) {uj::cmp_mmm(x, 'chr', ...)}

#' @rdname cmp_mmm
#' @export
cmp_clr <- function(x, ...) {uj::cmp_mmm(x, 'clr', ...)}

#' @rdname cmp_mmm
#' @export
cmp_evn <- function(x, ...) {uj::cmp_mmm(x, 'evn', ...)}

#' @rdname cmp_mmm
#' @export
cmp_fac <- function(x, ...) {uj::cmp_mmm(x, 'fac', ...)}

#' @rdname cmp_mmm
#' @export
cmp_frc <- function(x, ...) {uj::cmp_mmm(x, 'frc', ...)}

#' @rdname cmp_mmm
#' @export
cmp_ind <- function(x, ...) {uj::cmp_mmm(x, 'ind', ...)}

#' @rdname cmp_mmm
#' @export
cmp_lgl <- function(x, ...) {uj::cmp_mmm(x, 'lgl', ...)}

#' @rdname cmp_mmm
#' @export
cmp_neg <- function(x, ...) {uj::cmp_mmm(x, 'neg', ...)}

#' @rdname cmp_mmm
#' @export
cmp_ngw <- function(x, ...) {uj::cmp_mmm(x, 'ngw', ...)}

#' @rdname cmp_mmm
#' @export
cmp_nng <- function(x, ...) {uj::cmp_mmm(x, 'nng', ...)}

#' @rdname cmp_mmm
#' @export
cmp_nnw <- function(x, ...) {uj::cmp_mmm(x, 'nnw', ...)}

#' @rdname cmp_mmm
#' @export
cmp_nps <- function(x, ...) {uj::cmp_mmm(x, 'nps', ...)}

#' @rdname cmp_mmm
#' @export
cmp_npw <- function(x, ...) {uj::cmp_mmm(x, 'npw', ...)}

#' @rdname cmp_mmm
#' @export
cmp_nst <- function(x, ...) {uj::cmp_mmm(x, 'nst', ...)}

#' @rdname cmp_mmm
#' @export
cmp_num <- function(x, ...) {uj::cmp_mmm(x, 'num', ...)}

#' @rdname cmp_mmm
#' @export
cmp_odd <- function(x, ...) {uj::cmp_mmm(x, 'odd', ...)}

#' @rdname cmp_mmm
#' @export
cmp_ord <- function(x, ...) {uj::cmp_mmm(x, 'ord', ...)}

#' @rdname cmp_mmm
#' @export
cmp_pct <- function(x, ...) {uj::cmp_mmm(x, 'pct', ...)}

#' @rdname cmp_mmm
#' @export
cmp_pos <- function(x, ...) {uj::cmp_mmm(x, 'pos', ...)}

#' @rdname cmp_mmm
#' @export
cmp_pos <- function(x, ...) {uj::cmp_mmm(x, 'ppn', ...)}

#' @rdname cmp_mmm
#' @export
cmp_psw <- function(x, ...) {uj::cmp_mmm(x, 'psw', ...)}

#' @rdname cmp_mmm
#' @export
cmp_srt <- function(x, ...) {uj::cmp_mmm(x, 'srt', ...)}

#' @rdname cmp_mmm
#' @export
cmp_str <- function(x, ...) {uj::cmp_mmm(x, 'str', ...)}

#' @rdname cmp_mmm
#' @export
cmp_uno <- function(x, ...) {uj::cmp_mmm(x, 'uno', ...)}

#' @rdname cmp_mmm
#' @export
cmp_whl <- function(x, ...) {uj::cmp_mmm(x, 'whl', ...)}
