#' @family props
#' @title Base + Extended Mode Properties
#' @description \tabular{ll}{
#'   FUNCTION          \tab WHAT IT DOES                                     \cr
#'   `bbb_mmm`         \tab Evaluates whether `x` matches the base property
#'                          specified in the argument `bbb` and matches the
#'                          extended mode specified in the argument `mmm`
#'                          subject to any restrictions in `...`.            \cr
#'   `BBB_MMM`         \tab Evaluates whether `x` matches base property `BBB`
#'                          and extended mode `MMM` subject to any restrictions
#'                          in `...` where `BBB` and `MMM` are placeholders for
#'                          any given base property and any given extended mode
#'                          property, respectively.                          \cr
#'   `bbb_mmm_props`   \tab Gets a character vector of all possible base +
#'                          extended mode properties.                          }
#' @param x An R object
#' @param bbb A character scalar containing an base property from `bbb_props()`.
#' @param mmm A character scalar containing an extended mode property from
#'   `mmm_props()`.
#' @inheritDotParams meets
#' @inheritSection meets Specifying Count and Value Restrictions
#' @return \tabular{ll}{
#'   FUNCTION               \tab RETURN VALUE                                \cr
#'   `bbb_mmm_props`        \tab A character vector.                         \cr
#'   `bbb_mmm`, `BBB_MMM`   \tab A logical scalar.                             }
#' @export
bbb_mmm <- function(x, bbb, mmm, ...) {
  BBB <- c("atm", "pop")
  errs <- c(.meets_errs(x, ...),
            f0(f0(length(bbb) != 1 | !is.character(bbb), F, f0(is.na(bbb), F, bbb %in% BBB  )), NULL, "\n \u2022 [bbb] is not a scalar value from c('atm', 'pop')."),
            f0(f0(length(mmm) != 1 | !is.character(mmm), F, f0(is.na(mmm), F, mmm %in% .mmms)), NULL, '\n \u2022 [mmm] is not a scalar value from mmm_props().'))
  if (!is.null(errs)) {stop(errs)}
  else if (!meets(x, ...)) {return(F)}
  else if (bbb == "atm") {if (!is.atomic(x)) {F} else {run(".i", mmm, "(x)")}}
  else if (bbb == "pop") {if (!is.atomic(x)) {F} else if (length(x) == 0) {F} else {run(".i", mmm, "(x)")}}
  else {F}
}

#' @rdname bbb_mmm
#' @export
bbb_mmm_props <- function() {sort(av(apply(expand.grid(bbb = c("atm", "pop"), mmm = .mmms), 1, paste0, collapse = '_')))}


#' @rdname bbb_mmm
#' @export
atm_ch1 <- function(x, ...) {bbb_mmm(x, 'atm', 'ch1', ...)}

#' @rdname bbb_mmm
#' @export
atm_ch3 <- function(x, ...) {bbb_mmm(x, 'atm', 'ch3', ...)}

#' @rdname bbb_mmm
#' @export
atm_chr <- function(x, ...) {bbb_mmm(x, 'atm', 'chr', ...)}

#' @rdname bbb_mmm
#' @export
atm_clr <- function(x, ...) {bbb_mmm(x, 'atm', 'clr', ...)}

#' @rdname bbb_mmm
#' @export
atm_evn <- function(x, ...) {bbb_mmm(x, 'atm', 'evn', ...)}

#' @rdname bbb_mmm
#' @export
atm_fac <- function(x, ...) {bbb_mmm(x, 'atm', 'fac', ...)}

#' @rdname bbb_mmm
#' @export
atm_frc <- function(x, ...) {bbb_mmm(x, 'atm', 'frc', ...)}

#' @rdname bbb_mmm
#' @export
atm_ind <- function(x, ...) {bbb_mmm(x, 'atm', 'ind', ...)}

#' @rdname bbb_mmm
#' @export
atm_lgl <- function(x, ...) {bbb_mmm(x, 'atm', 'lgl', ...)}

#' @rdname bbb_mmm
#' @export
atm_neg <- function(x, ...) {bbb_mmm(x, 'atm', 'neg', ...)}

#' @rdname bbb_mmm
#' @export
atm_ngw <- function(x, ...) {bbb_mmm(x, 'atm', 'ngw', ...)}

#' @rdname bbb_mmm
#' @export
atm_nng <- function(x, ...) {bbb_mmm(x, 'atm', 'nng', ...)}

#' @rdname bbb_mmm
#' @export
atm_nnw <- function(x, ...) {bbb_mmm(x, 'atm', 'nnw', ...)}

#' @rdname bbb_mmm
#' @export
atm_nps <- function(x, ...) {bbb_mmm(x, 'atm', 'nps', ...)}

#' @rdname bbb_mmm
#' @export
atm_npw <- function(x, ...) {bbb_mmm(x, 'atm', 'npw', ...)}

#' @rdname bbb_mmm
#' @export
atm_nst <- function(x, ...) {bbb_mmm(x, 'atm', 'nst', ...)}

#' @rdname bbb_mmm
#' @export
atm_num <- function(x, ...) {bbb_mmm(x, 'atm', 'num', ...)}

#' @rdname bbb_mmm
#' @export
atm_odd <- function(x, ...) {bbb_mmm(x, 'atm', 'odd', ...)}

#' @rdname bbb_mmm
#' @export
atm_ord <- function(x, ...) {bbb_mmm(x, 'atm', 'ord', ...)}

#' @rdname bbb_mmm
#' @export
atm_pct <- function(x, ...) {bbb_mmm(x, 'atm', 'pct', ...)}

#' @rdname bbb_mmm
#' @export
atm_pos <- function(x, ...) {bbb_mmm(x, 'atm', 'pos', ...)}

#' @rdname bbb_mmm
#' @export
atm_ppn <- function(x, ...) {bbb_mmm(x, 'atm', 'ppn', ...)}

#' @rdname bbb_mmm
#' @export
atm_psw <- function(x, ...) {bbb_mmm(x, 'atm', 'psw', ...)}

#' @rdname bbb_mmm
#' @export
atm_srt <- function(x, ...) {bbb_mmm(x, 'atm', 'srt', ...)}

#' @rdname bbb_mmm
#' @export
atm_str <- function(x, ...) {bbb_mmm(x, 'atm', 'str', ...)}

#' @rdname bbb_mmm
#' @export
atm_uno <- function(x, ...) {bbb_mmm(x, 'atm', 'uno', ...)}

#' @rdname bbb_mmm
#' @export
atm_whl <- function(x, ...) {bbb_mmm(x, 'atm', 'whl', ...)}


#' @rdname bbb_mmm
#' @export
pop_ch1 <- function(x, ...) {bbb_mmm(x, 'pop', 'ch1', ...)}

#' @rdname bbb_mmm
#' @export
pop_ch3 <- function(x, ...) {bbb_mmm(x, 'pop', 'ch3', ...)}

#' @rdname bbb_mmm
#' @export
pop_chr <- function(x, ...) {bbb_mmm(x, 'pop', 'chr', ...)}

#' @rdname bbb_mmm
#' @export
pop_clr <- function(x, ...) {bbb_mmm(x, 'pop', 'clr', ...)}

#' @rdname bbb_mmm
#' @export
pop_evn <- function(x, ...) {bbb_mmm(x, 'pop', 'evn', ...)}

#' @rdname bbb_mmm
#' @export
pop_fac <- function(x, ...) {bbb_mmm(x, 'pop', 'fac', ...)}

#' @rdname bbb_mmm
#' @export
pop_frc <- function(x, ...) {bbb_mmm(x, 'pop', 'frc', ...)}

#' @rdname bbb_mmm
#' @export
pop_ind <- function(x, ...) {bbb_mmm(x, 'pop', 'ind', ...)}

#' @rdname bbb_mmm
#' @export
pop_lgl <- function(x, ...) {bbb_mmm(x, 'pop', 'lgl', ...)}

#' @rdname bbb_mmm
#' @export
pop_neg <- function(x, ...) {bbb_mmm(x, 'pop', 'neg', ...)}

#' @rdname bbb_mmm
#' @export
pop_ngw <- function(x, ...) {bbb_mmm(x, 'pop', 'ngw', ...)}

#' @rdname bbb_mmm
#' @export
pop_nng <- function(x, ...) {bbb_mmm(x, 'pop', 'nng', ...)}

#' @rdname bbb_mmm
#' @export
pop_nnw <- function(x, ...) {bbb_mmm(x, 'pop', 'nnw', ...)}

#' @rdname bbb_mmm
#' @export
pop_nps <- function(x, ...) {bbb_mmm(x, 'pop', 'nps', ...)}

#' @rdname bbb_mmm
#' @export
pop_npw <- function(x, ...) {bbb_mmm(x, 'pop', 'npw', ...)}

#' @rdname bbb_mmm
#' @export
pop_nst <- function(x, ...) {bbb_mmm(x, 'pop', 'nst', ...)}

#' @rdname bbb_mmm
#' @export
pop_num <- function(x, ...) {bbb_mmm(x, 'pop', 'num', ...)}

#' @rdname bbb_mmm
#' @export
pop_odd <- function(x, ...) {bbb_mmm(x, 'pop', 'odd', ...)}

#' @rdname bbb_mmm
#' @export
pop_ord <- function(x, ...) {bbb_mmm(x, 'pop', 'ord', ...)}

#' @rdname bbb_mmm
#' @export
pop_pct <- function(x, ...) {bbb_mmm(x, 'pop', 'pct', ...)}

#' @rdname bbb_mmm
#' @export
pop_pos <- function(x, ...) {bbb_mmm(x, 'pop', 'pos', ...)}

#' @rdname bbb_mmm
#' @export
pop_ppn <- function(x, ...) {bbb_mmm(x, 'pop', 'ppn', ...)}

#' @rdname bbb_mmm
#' @export
pop_psw <- function(x, ...) {bbb_mmm(x, 'pop', 'psw', ...)}

#' @rdname bbb_mmm
#' @export
pop_srt <- function(x, ...) {bbb_mmm(x, 'pop', 'srt', ...)}

#' @rdname bbb_mmm
#' @export
pop_str <- function(x, ...) {bbb_mmm(x, 'pop', 'str', ...)}

#' @rdname bbb_mmm
#' @export
pop_uno <- function(x, ...) {bbb_mmm(x, 'pop', 'uno', ...)}

#' @rdname bbb_mmm
#' @export
pop_whl <- function(x, ...) {bbb_mmm(x, 'pop', 'whl', ...)}
