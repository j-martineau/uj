#' @encoding UTF-8
#' @family strings
#' @title Oxford-comma separated lists
#' @description Create Oxford-comma separated lists with a variety of templates (see *details*).
#' \cr\cr With the exception of `{N} > length(uj::av(...))`, these functions appropriately process lists of length `1` and `2`.
#' @details Each function in this family operates from a template as shown in the following tables where `{conj}` and `{N}` represent the values of arguments `conj` and `N`; `{pref}` and `{comp}` indicate the potentially-`NULL` values of arguments `pref` and `conj`; and `[a]`, `[b]`, and `[z]` represents elements of a list.
#' \cr\cr **Simple lists**
#' \cr\cr These functions do not incorporate a number into the Oxford-comma separated list:
#' \tabular{ll}{  `ox`             \tab `'(pref) [a], [b], ..., {conj} [z]'`         \cr
#'                `oxford`         \tab                                              \cr
#'                `oxford_comma`   \tab                                              \cr   \tab   \cr
#'                `ox_or `         \tab `'(pref) [a], [b], ..., or [z]'`             \cr
#'                `ox_and`         \tab `'(pref) [a], [b], ..., and [z]'`            \cr
#'                `ox_any `        \tab `'(pref) any of [a], [b], ..., {conj} [z]'`  \cr
#'                `ox_all `        \tab `'(pref) all of [a], [b], ..., {conj} [z]'`  \cr
#'                `ox_none`        \tab `'(pref) none of [a], [b], ..., {conj} [z]'` \cr
#'                `ox_some`        \tab `'(pref) some of [a], [b], ..., {conj} [z]'` \cr
#'                `ox_either`      \tab `'(pref) either [a], [b], ..., or [z]'`      \cr
#'                `ox_neither`     \tab `'(pref) neither [a], [b], ..., nor [z]'`      }
#' \cr\cr **Numeric-comparison lists**
#' \cr\cr These functions incorporate a number into the Oxford-comma separated list:
#' \tabular{ll}{  `ox_n`            \tab `'(pref) {N} of [a], [b], ..., {conj} [z]'`                 \cr
#'                `ox_exactly`      \tab `'(pref) exactly {N} of [a], [b], ..., {conj} [z]'`         \cr
#'                `ox_less`         \tab `'(pref) less than {N} of [a], [b], ..., {conj} [z]'`       \cr
#'                `ox_more`         \tab `'(pref) more than {N} of [a], [b], ..., {conj} [z]'`       \cr
#'                `ox_fewer`        \tab `'(pref) fewer than {N} of [a], [b], ..., {conj} [z]'`      \cr
#'                `ox_greater`      \tab `'(pref) greater than {N} of [a], [b], ..., {conj} [z]'`    \cr
#'                `ox_at_most`      \tab `'(pref) at most {N} of [a], [b], ..., {conj} [z]'`         \cr
#'                `ox_at_least`     \tab `'(pref) at least {N} of [a], [b], ..., {conj} [z]'`        \cr
#'                `ox_no_less`      \tab `'(pref) no less than {N} of [a], [b], ..., {conj} [z]'`    \cr
#'                `ox_no_more`      \tab `'(pref) no more than {N} of [a], [b], ..., {conj} [z]'`    \cr
#'                `ox_no_fewer`     \tab `'(pref) no fewer than {N} of [a], [b], ..., {conj} [z]'`   \cr
#'                `ox_no_greater`   \tab `'(pref) no greater than {N} of [a], [b], ..., {conj} [z]'` \cr
#'                `ox_or_less`      \tab `'(pref) {N} or less of [a], [b], ..., {conj} [z]'`         \cr
#'                `ox_or_more`      \tab `'(pref) {N} or more of [a], [b], ..., {conj} [z]'`         \cr
#'                `ox_or_fewer`     \tab `'(pref) {N} or fewer of [a], [b], ..., {conj} [z]'`        \cr
#'                `ox_or_greater`   \tab `'(pref) {N} or greater of [a], [b], ..., {conj} [z]'`        }
#' @param ... Any number of arguments coerceable to mode character.
#' @param Pref A \link[=cmp_chr_scl]{complete character scalar} prefix to prepend to the list.
#' @param Conj A complete character scalar conjunction to use between the next to last and last elements of the list. Typical values are `and`, `or` and `nor`.
#' @param N A \link[=cmp_psw_scl]{complete positive whole-number scalar}.
#' @param Comp A complete character scalar used for comparing to `N`, such as `'at least'` or `'or fewer'`.
#' @param First `TRUE` or `FALSE` used to determine whether `Comp` is placed in front of `N` rather than after `N`.
#' @return A character scalar.
#' @examples
#' egFruits <- c("apples", "bananas", "oranges")
#'
#' ox(egFruits)
#' ox("apples", "bananas", "orange")
#' ox_or(egFruits)
#' ox_and(egFruits)
#' ox_nor(egFruits)
#' ox_nor(egFruits, Pref = "")
#'
#' ox(egFruits)
#' ox(egFruits, Conj = "or")
#' ox(egFruits, Pref = "neither", Conj = "nor")
#'
#' ox(egFruits, Pref = "", Conj = "nor")
#' ox(egFruits, Pref = "either", Conj = "or")
#' ox(egFruits, Pref = "all of", Conj = "and")
#'
#' ox_either(egFruits)
#' ox_either("apples", "bananas", "oranges")
#'
#' ox_neither(egFruits)
#' ox_neither("apples", "bananas", "oranges")
#'
#' ox_all(egFruits)
#' ox_all("apples", "bananas", "oranges")
#'
#' ox_none(egFruits)
#' ox_none("apples", "bananas", "oranges")
#'
#' ox_n(egFruits, N = 1)
#' ox_n(egFruits, Conj = "and", N = 2, Comp = "from among")
#' ox_n(egFruits, Conj = "and", N = 2, Comp = "at least", First = TRUE)
#' ox_n(egFruits, Conj = "and", N = 2, Comp = "or more", First = FALSE)
#'
#' ox_exactly(egFruits, N = 2)
#' ox_less(egFruits, N = 2)
#' ox_more(egFruits, N = 2)
#' ox_fewer(egFruits, N = 2)
#' ox_greater(egFruits, N = 2)
#' ox_at_least(egFruits, N = 2)
#' ox_at_most(egFruits, N = 2)
#' ox_no_greater(egFruits, N = 2)
#' ox_no_fewer(egFruits, N = 2)
#' ox_no_more(egFruits, N = 2)
#' ox_no_less(egFruits, N = 2)
#' ox_or_more(egFruits, N = 2)
#' ox_or_greater(egFruits, N = 2)
#' ox_or_less(egFruits, N = 2)
#' ox_or_fewer(egFruits, N = 2)
#' @export
ox <- function(..., Conj = "and", Pref = "", Quote = 0) {
  Vals <- uj::av(...)
  if (!uj:::.cmp_nnw_scl(Quote)) {OkQuote <- F} else {OkQuote <- Quote %in% 0:2}
  Errors <- NULL
  if (base::length(Vals) == 0) {Errors <- base::c(Errors, "[...] is empty.")}
  if (uj:::.cmp_chr_scl(Conj)) {Errors <- base::c(Errors, "[Conj] must be a complete character scalar (?cmp_chr_scl).")}
  if (uj:::.cmp_chr_scl(Pref)) {Errors <- base::c(Errors, "[Pref] must be a complete character scalar (?cmp_chr_scl).")}
  if (!OkQuote) {Errors <- base::c(Errors, "[Quote] must be 0, 1, or 2.")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  nVals <- base::length(Vals)
  if (nVals == 1) {
    if (Conj == "nor") {uj::stopperr("[Conj = 'nor'], but [...] contains only 1 atomic element.", PKG = "uj")}
    else if (Conj == "or" & Pref == "either") {uj::stopperr("[Conj = 'or'] and [Pref = 'either'], but [...] contains only 1 atomic element.", PKG = "uj")}
  }
  if (Pref != "") {Pref <- base::paste0(Pref, " ")}                             # if pref(ix) is not empty, follow it with a space
  Last <- Vals[nVals]                                                           # get the last element of X
  if (nVals > 1) {                                                              # if there is more than one element in the list
    List <- base::paste0(Vals[1:(nVals - 1)], collapse = ", ")                  # : create a comma separated list with all but the last element of X
    if (nVals == 2) {ConjPref <- " "} else {ConjPref <- ", "}
    ConjSuff <- " "
    Conj <- base::paste0(ConjPref, Conj, ConjSuff)
  } else {List <- Conj <- ""}
  base::paste0(Pref, List, Conj, Last)
}

#' @rdname ox
#' @export
oxford <- ox

#' @rdname ox
#' @export
oxford_comma <- ox

#' @rdname ox
#' @export
ox_n <- function(..., Conj = "and", Comp = "", Quote = 0, N = 1, First = TRUE) {
  Vals <- uj::av(...)
  if (!uj:::.cmp_nnw_scl(Quote)) {OkQuote <- F} else {OkQuote <- Quote %in% 0:2}
  Errors <- NULL
  if (base::length(Vals) == 0) {Errors <- base::c(errs, "[...] is empty.")}
  if (uj:::.cmp_chr_scl(Conj)) {Errors <- base::c(errs, "[Conj] must be a complete character scalar (?cmp_chr_scl).")}
  if (uj:::.cmp_chr_scl(Comp)) {Errors <- base::c(errs, "[Comp] must be a complete character scalar (?cmp_chr_scl).")}
  if (!OkQuote) {Errors <- base::c(Errors, "[Quote] must be 0, 1, or 2.")}
  if (!uj:::.cmp_nnw_scl(N)) {Errors <- base::c(Errors, "[N] must be a complete non-negative whole-number scalar (?cmp_nnw_scl)")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  if (Comp == "") {Pref <- base::paste(N, "of")}
  else if (First) {Pref <- base::paste(Comp, N, "of")}
  else            {Pref <- base::paste(N, Comp, "of")}
  uj::ox(Vals, Conj = Conj, Pref = Pref, Quote = Quote)
}

#' @rdname ox
#' @export
ox_and <- function(..., Pref = "") {uj::ox(..., Pref = Pref, Conj = "and")}

#' @rdname ox
#' @export
ox_or <- function(..., Pref = "") {uj::ox(..., Pref = Pref, Conj = "or")}

#' @rdname ox
#' @export
ox_nor <- function(..., Pref = "neither") {uj::ox(..., Pref = Pref, Conj = "nor")}

#' @rdname ox
#' @export
ox_either <- function(...) {uj::ox(..., Pref = "either", Conj = "or")}

#' @rdname ox
#' @export
ox_neither <- function(...) {uj::ox(..., Pref = "neither", Conj = "nor")}

#' @rdname ox
#' @export
ox_all <- function(..., Conj = "and") {uj::ox(..., Pref = "all of", Conj = Conj)}

#' @rdname ox
#' @export
ox_any <- function(..., Conj = "or") {uj::ox(..., Pref = "any of", Conj = Conj)}

#' @rdname ox
#' @export
ox_none <- function(..., Conj = "or") {uj::ox(..., Pref = "none of", Conj = Conj)}

#' @rdname ox
#' @export
ox_some <- function(..., Conj = "and") {uj::ox(..., Pref = "some of", Conj = Conj)}

#' @rdname ox
#' @export
ox_exactly <- function(..., Conj = "or", N = 1) {uj::ox_n(..., comp = "exactly", N = N, Conj = Conj)}

#' @rdname ox
#' @export
ox_less <- function(..., Conj = "and", N = 2) {uj::ox_n(..., Comp = "less than", N = N, Conj = Conj)}

#' @rdname ox
#' @export
ox_more <- function(..., Conj = "and", N = 1) {uj::ox_n(..., Comp = "more than", N = N, Conj = Conj)}

#' @rdname ox
#' @export
ox_fewer <- function(..., Conj = "and", N = 2) {uj::ox_n(..., Comp = "fewer than", N = N, Conj = Conj)}

#' @rdname ox
#' @export
ox_greater <- function(..., Conj = "and", N = 2) {uj::ox_n(..., Comp = "greater than", N = N, Conj = Conj)}

#' @rdname ox
#' @export
ox_at_least <- function(..., Conj = "and", N = 1) {uj::ox_n(..., Comp = "at least", N = N, Conj = Conj)}

#' @rdname ox
#' @export
ox_at_most <- function(..., Conj = "and", N = 1) {uj::ox_n(..., Comp = "at most", N = N, Conj = Conj)}

#' @rdname ox
#' @export
ox_no_greater <- function(..., Conj = "and", N = 1) {uj::ox_n(..., Comp = "no greater than", N = N, Conj = Conj)}

#' @rdname ox
#' @export
ox_no_fewer <- function(..., Conj = "and", N = 1) {uj::ox_n(..., Comp = "no fewer than", N = N, Conj = Conj)}

#' @rdname ox
#' @export
ox_no_more <- function(..., Conj = "and", N = 1) {uj::ox_n(..., Comp = "no more than", N = N, Conj = Conj)}

#' @rdname ox
#' @export
ox_no_less <- function(..., Conj = "and", N = 1) {uj::ox_n(..., Comp = "no less than", N = N, Conj = Conj)}

#' @rdname ox
#' @export
ox_or_more <- function(..., Conj = "and", N = 1) {uj::ox_n(..., Comp = "or more", N = N, First = F, Conj = Conj)}

#' @rdname ox
#' @export
ox_or_greater <- function(..., Conj = "and", N = 1) {uj::ox_n(..., Comp = "or greater", N = N, First = F, Conj = Conj)}

#' @rdname ox
#' @export
ox_or_less <- function(..., Conj = "and", N = 1) {uj::ox_n(..., Comp = "or less", N = N, First = F, Conj = Conj)}

#' @rdname ox
#' @export
ox_or_fewer <- function(..., Conj = "and", N = 1) {uj::ox_n(..., Comp = "or fewer", N = N, First = F, Conj = Conj)}
