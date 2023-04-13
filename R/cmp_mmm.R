#' @encoding UTF-8
#' @family properties
#' @title complete + Xmode combination properties
#' @description Check for combination of \link[=CMP]{completeness} and \link[=mmm]{Xmode}.
#' @details
#' \tabular{ll}{  `cmp_mmm_funs`   \tab What \link[=CMP]{complete} + \link[=mmm]{Xmode} combination \link[=prop_funs]{property functions} are there?                         \cr   \tab   \cr
#'                `cmp_{mmm}`      \tab Is `X` both complete and a match to the single Xmode property `'{mmm}'` where `{mmm}` is a placeholder for any given Xmode property? \cr   \tab   \cr
#'                `cmp_mmm`        \tab Is `X` both complete and a match to single Xmode property in arg `mmm`?                                                                             }
#' @param X An R object.
#' @param MMM A character scalar Xmode property from `mmm_props()`.
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
cmp_mmm <- function(X, MMM, ...) {
  Errors <- uj:::.meets_Errors(X, ...)
  if (!uj:::.cmp_chr_scl(MMM, valid = base::c(uj::v(mmm), uj::v(MMM)))) {Errors <- base::c(Errors, '[MMM] is not a scalar value from mmm_props().')}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  if (!uj::meets(X, ...)) {F}
  else if (!base::is.atomic(X) | base::length(X) == 0) {F}
  else if (base::any(base::is.na(X))) {F}
  else {uj::run("uj:::.", base::toupper(MMM), "(X)")}
}

#' @rdname cmp_mmm
#' @export
cmp_mmm_funs <- function() {base::paste0('cmp_', uj::v(mmm))}

#' @rdname cmp_mmm
#' @export
cmp_atm <- function(X, ...) {uj::f0(base::is.atomic(X) & base::length(X) > 0, !base::any(base::is.na(X)), F)}

#' @rdname cmp_mmm
#' @export
cmp_ch1 <- function(X, ...) {uj::cmp_mmm(X, 'ch1', ...)}

#' @rdname cmp_mmm
#' @export
cmp_ch3 <- function(X, ...) {uj::cmp_mmm(X, 'ch3', ...)}

#' @rdname cmp_mmm
#' @export
cmp_chr <- function(X, ...) {uj::cmp_mmm(X, 'chr', ...)}

#' @rdname cmp_mmm
#' @export
cmp_clr <- function(X, ...) {uj::cmp_mmm(X, 'clr', ...)}

#' @rdname cmp_mmm
#' @export
cmp_evn <- function(X, ...) {uj::cmp_mmm(X, 'evn', ...)}

#' @rdname cmp_mmm
#' @export
cmp_fac <- function(X, ...) {uj::cmp_mmm(X, 'fac', ...)}

#' @rdname cmp_mmm
#' @export
cmp_frc <- function(X, ...) {uj::cmp_mmm(X, 'frc', ...)}

#' @rdname cmp_mmm
#' @export
cmp_ind <- function(X, ...) {uj::cmp_mmm(X, 'ind', ...)}

#' @rdname cmp_mmm
#' @export
cmp_lgl <- function(X, ...) {uj::cmp_mmm(X, 'lgl', ...)}

#' @rdname cmp_mmm
#' @export
cmp_neg <- function(X, ...) {uj::cmp_mmm(X, 'neg', ...)}

#' @rdname cmp_mmm
#' @export
cmp_ngw <- function(X, ...) {uj::cmp_mmm(X, 'ngw', ...)}

#' @rdname cmp_mmm
#' @export
cmp_nng <- function(X, ...) {uj::cmp_mmm(X, 'nng', ...)}

#' @rdname cmp_mmm
#' @export
cmp_nnw <- function(X, ...) {uj::cmp_mmm(X, 'nnw', ...)}

#' @rdname cmp_mmm
#' @export
cmp_nps <- function(X, ...) {uj::cmp_mmm(X, 'nps', ...)}

#' @rdname cmp_mmm
#' @export
cmp_npw <- function(X, ...) {uj::cmp_mmm(X, 'npw', ...)}

#' @rdname cmp_mmm
#' @export
cmp_nst <- function(X, ...) {uj::cmp_mmm(X, 'nst', ...)}

#' @rdname cmp_mmm
#' @export
cmp_num <- function(X, ...) {uj::cmp_mmm(X, 'num', ...)}

#' @rdname cmp_mmm
#' @export
cmp_odd <- function(X, ...) {uj::cmp_mmm(X, 'odd', ...)}

#' @rdname cmp_mmm
#' @export
cmp_ord <- function(X, ...) {uj::cmp_mmm(X, 'ord', ...)}

#' @rdname cmp_mmm
#' @export
cmp_pct <- function(X, ...) {uj::cmp_mmm(X, 'pct', ...)}

#' @rdname cmp_mmm
#' @export
cmp_pos <- function(X, ...) {uj::cmp_mmm(X, 'pos', ...)}

#' @rdname cmp_mmm
#' @export
cmp_pos <- function(X, ...) {uj::cmp_mmm(X, 'ppn', ...)}

#' @rdname cmp_mmm
#' @export
cmp_psw <- function(X, ...) {uj::cmp_mmm(X, 'psw', ...)}

#' @rdname cmp_mmm
#' @export
cmp_srt <- function(X, ...) {uj::cmp_mmm(X, 'srt', ...)}

#' @rdname cmp_mmm
#' @export
cmp_str <- function(X, ...) {uj::cmp_mmm(X, 'str', ...)}

#' @rdname cmp_mmm
#' @export
cmp_uno <- function(X, ...) {uj::cmp_mmm(X, 'uno', ...)}

#' @rdname cmp_mmm
#' @export
cmp_whl <- function(X, ...) {uj::cmp_mmm(X, 'whl', ...)}
