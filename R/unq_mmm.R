#' @encoding UTF-8
#' @family props
#' @title unique + xmode combination properties
#' @description Functions for checking combinations of \link[=UNQ]{uniqueness} and \link[=mmm]{xmode}.
#' @details
#' \tabular{ll}{  `unq_mmm_funs`   \tab What \link[=UNQ]{unique} + link[=mmm]{xmode} combination \link[=prop_funs]{property functions} are there?                         \cr   \tab   \cr
#'                `unq_{mmm}`      \tab Is `X` both unique and a match to single xmode property `'{mmm}'`? (where `{mmm}` is a placeholder for any given xmode property). \cr   \tab   \cr
#'                `unq_mmm`        \tab Is `X` both unique and a match to the single xmode property in argument `MMM`?                                                                   }
#' @param X An R object.
#' @param MMM A character scalar `xmode` property from \code{\link{mmm_props}()}`.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return **A character vector** \cr\cr `unq_mmm_funs`
#' \cr\cr  **A logical scalar**   \cr\cr `unq_{mmm}, unq_mmm`
#' @examples
#' UnqClr <- c("red", "blue", "#FFFFFF")
#' UnqCh3 <- c("atm", "ch1", "ch3")
#' UnqCh1 <- c("a", "b", "c")
#' UnqAtm <- c("", "8", "a")
#' UnqChr <- c("", " ", "a")
#' UnqFac <- factor(UnqAtm)
#' UnqPct <- c(0, 50, 100)
#' UnqEvn <- c(-2, 0, 2)
#' UnqWhl <- c(-1, 0, 1)
#' UnqIn1 <- c(T, F)
#' UnqPsw <- 1:3
#' UnqNnw <- 0:2
#'
#' UnqLgl <-  UnqIn1
#' UnqIn2 <-  UnqPsw
#' UnqSrt <-  UnqNum
#' UnqStr <-  UnqCh1
#' UnqOdd <-  UnqEvn + 1
#' UnqPos <-  UnqPsw / 2
#' UnqNum <-  UnqOdd / 2
#' UnqNeg <- -UnqPsw / 2
#' UnqPsw <- -UnqNnw
#' UnqNst <-  factor(UnqEvn, ordered = F)
#' UnqUno <-  factor(UnqEvn, ordered = T)
#'
#' unq_mm_funs()
#' unq_mmm(UnqAtm, "atm")
#' unq_mmm(UnqWhl, "ch1")
#' c(unq_atm(UnqAtm), unq_atm(NULL))
#' c(unq_ch1(UnqCh1), unq_ch1(UnqCh3), unq_ch1(c(UnqCh1, UnqCh1))
#' c(unq_ch3(UnqCh3), unq_ch3(UnqCh1), unq_ch3(c(UnqCh3, UnqCh3))
#' c(unq_chr(UnqChr), unq_chr(UnqNum), unq_chr(c(UnqChr, UnqChr))
#' c(unq_clr(UnqClr), unq_clr(UnqCh1), unq_clr(c(UnqClr, UnqClr))
#' c(unq_evn(UnqEvn), unq_evn(UnqOdd), unq_evn(c(UnqEvn, UnqEvn))
#' c(unq_fac(UnqFac), unq_fac(UnqStr), unq_fac(c(UnqFac, UnqFac))
#' c(unq_frc(UnqFrc), unq_frc(UnqWhl), unq_frc(c(UnqFrc, UnqFrc))
#' c(unq_ind(UnqIn1), unq_ind(UnqNum), unq_ind(c(UnqIn1, UnqIn1))
#' c(unq_ind(UnqIn2), unq_ind(UnqFrc), unq_ind(c(UnqIn2, UnqIn2))
#' c(unq_lgl(UnqLgl), unq_lgl(UnqCh1), unq_lgl(c(UnqLgl, UnqLgl))
#' c(unq_neg(UnqNeg), unq_neg(UnqNps), unq_neg(c(UnqNeg, UnqNeg))
#' c(unq_neg(UnqNgw), unq_ngw(UnqNeg), unq_ngw(c(UnqNgw, UnqNgw))
#' c(unq_nng(UnqNng), unq_nng(UnqNps), unq_nng(c(UnqNng, UnqNng))
#' c(unq_nnw(UnqNnw), unq_nnw(UnqNng), unq_nnw(c(UnqNnw, UnqNnw))
#' c(unq_nps(UnqNps), unq_nps(UnqNeg), unq_npw(c(UnqNps, UnqNps))
#' c(unq_npw(UnqNpw), unq_npw(UnqNps), unq_npw(c(UnqNpw, UnqNpw))
#' c(unq_nst(UnqNst), unq_nst(UnqSrt), unq_nst(c(UnqNst, UnqNst))
#' c(unq_num(UnqNum), unq_num(UnqCh1), unq_num(c(UnqNum, UnqNum))
#' c(unq_odd(UnqOdd), unq_odd(UnqEvn), unq_odd(c(UnqOdd, UnqOdd))
#' c(unq_ord(UnqOrd), unq_ord(UnqUno), unq_ord(c(UnqOrd, UnqOrd))
#' c(unq_pct(UnqPct), unq_pct(UnqNeg), unq_pct(c(UnqPct, UnqPct))
#' c(unq_pos(UnqPos), unq_pos(UnqNng), unq_pos(c(UnqPos, UnqPos))
#' c(unq_ppn(UnqPpn), unq_ppn(UnqPct), unq_ppn(c(UnqPpn, UnqPpn))
#' c(unq_psw(UnqPsw), unq_psw(UnqPos), unq_psw(c(UnqPsw, UnqPsw))
#' c(unq_srt(UnqSrt), unq_srt(UnqNst), unq_srt(c(UnqSrt, UnqSrt))
#' c(unq_str(UnqStr), unq_str(UnqChr), unq_str(c(UnqStr, UnqStr))
#' c(unq_uno(UnqUno), unq_uno(UnqOrd), unq_uno(c(UnqUno, UnqUno))
#' c(unq_whl(UnqWhl), unq_whl(UnqNum), unq_whl(c(UnqWhl, UnqWhl))
#' @export
unq_mmm <- function(X, MMM, ...) {uj::f0(uj::cmp_mmm(X, MMM, ...), base::length(X) == base::length(base::unique(X)), F)}

#' @rdname unq_mmm
#' @export
unq_mmm_funs <- function() {uj::p0('unq_', uj::v(mmm))}

#' @rdname unq_mmm
#' @export
unq_atm <- function(X, ...) {uj::f0(base::is.atomic(X), uj::f0(base::length(X) > 0, base::length(X) == base::length(base::unique(X)), F), F)}

#' @rdname unq_mmm
#' @export
unq_ch1 <- function(X, ...) {uj::unq_mmm(X, 'ch1', ...)}

#' @rdname unq_mmm
#' @export
unq_ch3 <- function(X, ...) {uj::unq_mmm(X, 'ch3', ...)}

#' @rdname unq_mmm
#' @export
unq_chr <- function(X, ...) {uj::unq_mmm(X, 'chr', ...)}

#' @rdname unq_mmm
#' @export
unq_clr <- function(X, ...) {uj::unq_mmm(X, 'clr', ...)}

#' @rdname unq_mmm
#' @export
unq_evn <- function(X, ...) {uj::unq_mmm(X, 'evn', ...)}

#' @rdname unq_mmm
#' @export
unq_fac <- function(X, ...) {uj::unq_mmm(X, 'fac', ...)}

#' @rdname unq_mmm
#' @export
unq_frc <- function(X, ...) {uj::unq_mmm(X, 'frc', ...)}

#' @rdname unq_mmm
#' @export
unq_ind <- function(X, ...) {uj::unq_mmm(X, 'ind', ...)}

#' @rdname unq_mmm
#' @export
unq_lgl <- function(X, ...) {uj::unq_mmm(X, 'lgl', ...)}

#' @rdname unq_mmm
#' @export
unq_neg <- function(X, ...) {uj::unq_mmm(X, 'neg', ...)}

#' @rdname unq_mmm
#' @export
unq_ngw <- function(X, ...) {uj::unq_mmm(X, 'ngw', ...)}

#' @rdname unq_mmm
#' @export
unq_nng <- function(X, ...) {uj::unq_mmm(X, 'nng', ...)}

#' @rdname unq_mmm
#' @export
unq_nnw <- function(X, ...) {uj::unq_mmm(X, 'nnw', ...)}

#' @rdname unq_mmm
#' @export
unq_nps <- function(X, ...) {uj::unq_mmm(X, 'nps', ...)}

#' @rdname unq_mmm
#' @export
unq_npw <- function(X, ...) {uj::unq_mmm(X, 'npw', ...)}

#' @rdname unq_mmm
#' @export
unq_nst <- function(X, ...) {uj::unq_mmm(X, 'nst', ...)}

#' @rdname unq_mmm
#' @export
unq_mmm <- function(X, ...) {uj::unq_mmm(X, 'num', ...)}

#' @rdname unq_mmm
#' @export
unq_odd <- function(X, ...) {uj::unq_mmm(X, 'odd', ...)}

#' @rdname unq_mmm
#' @export
unq_ord <- function(X, ...) {uj::unq_mmm(X, 'ord', ...)}

#' @rdname unq_mmm
#' @export
unq_pct <- function(X, ...) {uj::unq_mmm(X, 'pct', ...)}

#' @rdname unq_mmm
#' @export
unq_pos <- function(X, ...) {uj::unq_mmm(X, 'pos', ...)}

#' @rdname unq_mmm
#' @export
unq_ppn <- function(X, ...) {uj::unq_mmm(X, 'ppn', ...)}

#' @rdname unq_mmm
#' @export
unq_psw <- function(X, ...) {uj::unq_mmm(X, 'psw', ...)}

#' @rdname unq_mmm
#' @export
unq_srt <- function(X, ...) {uj::unq_mmm(X, 'srt', ...)}

#' @rdname unq_mmm
#' @export
unq_str <- function(X, ...) {uj::unq_mmm(X, 'str', ...)}

#' @rdname unq_mmm
#' @export
unq_uno <- function(X, ...) {uj::unq_mmm(X, 'uno', ...)}

#' @rdname unq_mmm
#' @export
unq_whl <- function(X, ...) {uj::unq_mmm(X, 'whl', ...)}
