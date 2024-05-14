#' @encoding UTF-8
#' @family strings
#' @title Oxford-comma separated lsts
#' @description Create Oxford-comma separated lsts with a variety of templates (see *details*).
#' \cr\cr With the exception of `n > length(uj::av(...))`, these functions appropriately process lsts of length `1` and `2`.
#' @details Each function in this family operates from a template as shown in the following tables where `{conj}` and `{n}` represent the values of arguments `conj` and `n`; `{pref}` and `{comp}` indicate the potentially-`NULL` values of arguments `pref` and `comp`; and `[a]`, `[b]`, and `[z]` represents elements of a lst.
#' \cr\cr **Simple lsts**
#' \cr\cr These functions do not incorporate a number into the Oxford-comma separated lst:
#' \tabular{ll}{  `ox`             \tab `'{pref} [a], [b], ..., {conj} [z]'`         \cr
#'                `oxford`         \tab                                              \cr
#'                `oxford_comma`   \tab                                              \cr   \tab   \cr
#'                `ox_or `         \tab `'{pref} [a], [b], ..., or [z]'`             \cr
#'                `ox_and`         \tab `'{pref} [a], [b], ..., and [z]'`            \cr
#'                `ox_any `        \tab `'{pref} any of [a], [b], ..., {conj} [z]'`  \cr
#'                `ox_all `        \tab `'{pref} all of [a], [b], ..., {conj} [z]'`  \cr
#'                `ox_none`        \tab `'{pref} none of [a], [b], ..., {conj} [z]'` \cr
#'                `ox_some`        \tab `'{pref} some of [a], [b], ..., {conj} [z]'` \cr
#'                `ox_either`      \tab `'{pref} either [a], [b], ..., or [z]'`      \cr
#'                `ox_neither`     \tab `'{pref} neither [a], [b], ..., nor [z]'`      }
#' \cr\cr **Numeric-comparison lsts**
#' \cr\cr These functions incorporate a number into the Oxford-comma separated lst:
#' \tabular{ll}{  `ox_n`            \tab `'{n} of [a], [b], ..., {conj} [z]'`          \cr
#'                `ox_exactly`      \tab `'exactly {n} of [a], [b], ..., {conj} [z]'`         \cr
#'                `ox_less`         \tab `'less than {n} of [a], [b], ..., {conj} [z]'`       \cr
#'                `ox_more`         \tab `'more than {n} of [a], [b], ..., {conj} [z]'`       \cr
#'                `ox_fewer`        \tab `'fewer than {n} of [a], [b], ..., {conj} [z]'`      \cr
#'                `ox_greater`      \tab `'greater than {n} of [a], [b], ..., {conj} [z]'`    \cr
#'                `ox_at_most`      \tab `'at most {n} of [a], [b], ..., {conj} [z]'`         \cr
#'                `ox_at_least`     \tab `'at least {n} of [a], [b], ..., {conj} [z]'`        \cr
#'                `ox_no_less`      \tab `'no less than {n} of [a], [b], ..., {conj} [z]'`    \cr
#'                `ox_no_more`      \tab `'no more than {n} of [a], [b], ..., {conj} [z]'`    \cr
#'                `ox_no_fewer`     \tab `'no fewer than {n} of [a], [b], ..., {conj} [z]'`   \cr
#'                `ox_no_greater`   \tab `'no greater than {n} of [a], [b], ..., {conj} [z]'` \cr
#'                `ox_or_less`      \tab `'{n} or less of [a], [b], ..., {conj} [z]'`         \cr
#'                `ox_or_more`      \tab `'{n} or more of [a], [b], ..., {conj} [z]'`         \cr
#'                `ox_or_fewer`     \tab `'{n} or fewer of [a], [b], ..., {conj} [z]'`        \cr
#'                `ox_or_greater`   \tab `'{n} or greater of [a], [b], ..., {conj} [z]'`        }
#' @param ... Any number of arguments coerceable to mode character.
#' @param pref A \link[=cmp_chr_scl]{complete character scalar} prefix to prepend to the lst.
#' @param conj A complete character scalar conjunction to use between the next to last and last elements of the lst. Typical values are `and`, `or` and `nor`.
#' @param n A \link[=cmp_psw_scl]{complete positive whole-number scalar}.
#' @param comp A complete character scalar used for comparing to `n`, such as `'at least'` or `'or fewer'`.
#' @param first `TRUE` or `FALSE` used to determine whether `comp` is placed in front of `n` rather than after `n`.
#' @param quote `0`, `1`, or `2` indicating whether to leave lst elements unquoted, single-quote the lst elements, or double-quote the lst elements.
#' @return A character scalar.
#' @examples
#' egFruits <- c("apples", "bananas", "oranges")
#'
#' ox(egFruits)
#' ox("apples", "bananas", "orange")
#' ox_or(egFruits)
#' ox_and(egFruits)
#' ox_nor(egFruits)
#' ox_nor(egFruits, pref = "")
#'
#' ox(egFruits)
#' ox(egFruits, conj = "or")
#' ox(egFruits, pref = "neither", conj = "nor")
#'
#' ox(egFruits, pref = "", conj = "nor")
#' ox(egFruits, pref = "either", conj = "or")
#' ox(egFruits, pref = "all of", conj = "and")
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
#' ox_n(egFruits, n = 1)
#' ox_n(egFruits, conj = "and", n = 2, comp = "from among")
#' ox_n(egFruits, conj = "and", n = 2, comp = "at least", first = TRUE)
#' ox_n(egFruits, conj = "and", n = 2, comp = "or more", first = FALSE)
#'
#' ox_exactly(egFruits, n = 2)
#' ox_less(egFruits, n = 2)
#' ox_more(egFruits, n = 2)
#' ox_fewer(egFruits, n = 2)
#' ox_greater(egFruits, n = 2)
#' ox_at_least(egFruits, n = 2)
#' ox_at_most(egFruits, n = 2)
#' ox_no_greater(egFruits, n = 2)
#' ox_no_fewer(egFruits, n = 2)
#' ox_no_more(egFruits, n = 2)
#' ox_no_less(egFruits, n = 2)
#' ox_or_more(egFruits, n = 2)
#' ox_or_greater(egFruits, n = 2)
#' ox_or_less(egFruits, n = 2)
#' ox_or_fewer(egFruits, n = 2)
#' @export
ox <- function(..., conj = "and", pref = "", quote = 0) {
  vals <- uj::av(...)
  if (!uj::.cmp_nnw_scl(quote)) {okQuote <- F} else {okQuote <- quote %in% 0:2}
  errs <- NULL
  if (base::length(vals) == 0) {errs <- base::c(errs, "[...] is empty.")}
  if (!uj::.cmp_chr_scl(conj)) {errs <- base::c(errs, "[conj] must be a complete character scalar (?cmp_chr_scl).")}
  if (!uj::.cmp_chr_scl(pref)) {errs <- base::c(errs, "[pref] must be a complete character scalar (?cmp_chr_scl).")}
  if (!okQuote) {errs <- base::c(errs, "[quote] must be 0, 1, or 2.")}
  if (!base::is.null(errs)) {uj::stopperr(errs)}
  nVals <- base::length(vals)
  if (quote == 1) {vals <- base::paste0("'", vals, "'")}
  else if (quote == 2) {vals <- base::paste0("\"", vals, "\"")}
  if (nVals == 1) {
    if (conj == "nor") {uj::stopperr("[conj = 'nor'], but [...] contains only 1 atomic element.")}
    else if (conj == "or" & pref == "either") {uj::stopperr("[conj = 'or'] and [pref = 'either'], but [...] contains only 1 atomic element.")}
  }
  if (pref != "") {pref <- base::paste0(pref, " ")}                             # if pref(ix) is not empty, follow it with a space
  last <- vals[nVals]                                                           # get the last element of X
  if (nVals > 1) {                                                              # if there is more than one element in the lst
    lst <- base::paste0(vals[1:(nVals - 1)], collapse = ", ")                   # : create a comma separated lst with all but the last element of X
    if (nVals == 2) {conjPref <- " "} else {conjPref <- ", "}
    conjSuff <- " "
    conj <- base::paste0(conjPref, conj, conjSuff)
  } else {lst <- conj <- ""}
  base::paste0(pref, lst, conj, last)
}

#' @rdname ox
#' @export
oxford <- ox

#' @rdname ox
#' @export
oxford_comma <- ox

#' @rdname ox
#' @export
ox_n <- function(..., conj = "and", comp = "", quote = 0, n = 1, first = TRUE) {
  Vals <- uj::av(...)
  if (!uj::.cmp_nnw_scl(quote)) {okQuote <- F} else {okQuote <- quote %in% 0:2}
  errs <- NULL
  if (base::length(Vals) == 0) {errs <- base::c(errs, "[...] is empty.")}
  if (uj::.cmp_chr_scl(conj)) {errs <- base::c(errs, "[conj] must be a complete character scalar (?cmp_chr_scl).")}
  if (uj::.cmp_chr_scl(comp)) {errs <- base::c(errs, "[comp] must be a complete character scalar (?cmp_chr_scl).")}
  if (!okQuote) {errs <- base::c(errs, "[quote] must be 0, 1, or 2.")}
  if (!uj::.cmp_nnw_scl(n)) {errs <- base::c(errs, "[n] must be a complete non-negative whole-number scalar (?cmp_nnw_scl)")}
  if (!base::is.null(errs)) {uj::stopperr(errs)}
  if (comp == "") {pref <- base::paste(n, "of")}
  else if (first) {pref <- base::paste(comp, n, "of")}
  else            {pref <- base::paste(n, comp, "of")}
  uj::ox(Vals, conj = conj, pref = pref, quote = quote)
}

#' @rdname ox
#' @export
ox_and <- function(..., pref = "", quote = 0) {uj::ox(..., pref = pref, conj = "and", quote = quote)}

#' @rdname ox
#' @export
ox_or <- function(..., pref = "", quote = 0) {uj::ox(..., pref = pref, conj = "or", quote = quote)}

#' @rdname ox
#' @export
ox_nor <- function(..., pref = "neither", quote = 0) {uj::ox(..., pref = pref, conj = "nor", quote = quote)}

#' @rdname ox
#' @export
ox_either <- function(..., quote = 0) {uj::ox(..., pref = "either", conj = "or", quote = quote)}

#' @rdname ox
#' @export
ox_neither <- function(..., quote = 0) {uj::ox(..., pref = "neither", conj = "nor", quote = quote)}

#' @rdname ox
#' @export
ox_all <- function(..., conj = "and", quote = 0) {uj::ox(..., pref = "all of", conj = conj, quote = quote)}

#' @rdname ox
#' @export
ox_any <- function(..., conj = "or", quote = 0) {uj::ox(..., pref = "any of", conj = conj, quote = quote)}

#' @rdname ox
#' @export
ox_none <- function(..., conj = "or", quote = 0) {uj::ox(..., pref = "none of", conj = conj, quote = quote)}

#' @rdname ox
#' @export
ox_some <- function(..., conj = "and", quote = 0) {uj::ox(..., pref = "some of", conj = conj, quote = quote)}

#' @rdname ox
#' @export
ox_exactly <- function(..., conj = "or", n = 1, quote = 0) {uj::ox_n(..., comp = "exactly", n = n, conj = conj, quote = quote)}

#' @rdname ox
#' @export
ox_less <- function(..., conj = "and", n = 2, quote = 0) {uj::ox_n(..., comp = "less than", n = n, conj = conj, quote = quote)}

#' @rdname ox
#' @export
ox_more <- function(..., conj = "and", n = 1, quote = 0) {uj::ox_n(..., comp = "more than", n = n, conj = conj, quote = quote)}

#' @rdname ox
#' @export
ox_fewer <- function(..., conj = "and", n = 2, quote = 0) {uj::ox_n(..., comp = "fewer than", n = n, conj = conj, quote = quote)}

#' @rdname ox
#' @export
ox_greater <- function(..., conj = "and", n = 2, quote = 0) {uj::ox_n(..., comp = "greater than", n = n, conj = conj, quote = quote)}

#' @rdname ox
#' @export
ox_at_least <- function(..., conj = "and", n = 1, quote = 0) {uj::ox_n(..., comp = "at least", n = n, conj = conj, quote = quote)}

#' @rdname ox
#' @export
ox_at_most <- function(..., conj = "and", n = 1, quote = 0) {uj::ox_n(..., comp = "at most", n = n, conj = conj, quote = quote)}

#' @rdname ox
#' @export
ox_no_greater <- function(..., conj = "and", n = 1, quote = 0) {uj::ox_n(..., comp = "no greater than", n = n, conj = conj, quote = quote)}

#' @rdname ox
#' @export
ox_no_fewer <- function(..., conj = "and", n = 1, quote = 0) {uj::ox_n(..., comp = "no fewer than", n = n, conj = conj, quote = quote)}

#' @rdname ox
#' @export
ox_no_more <- function(..., conj = "and", n = 1, quote = 0) {uj::ox_n(..., comp = "no more than", n = n, conj = conj, quote = quote)}

#' @rdname ox
#' @export
ox_no_less <- function(..., conj = "and", n = 1, quote = 0) {uj::ox_n(..., comp = "no less than", n = n, conj = conj, quote = quote)}

#' @rdname ox
#' @export
ox_or_more <- function(..., conj = "and", n = 1, quote = 0) {uj::ox_n(..., comp = "or more", n = n, first = F, conj = conj, quote = quote)}

#' @rdname ox
#' @export
ox_or_greater <- function(..., conj = "and", n = 1, quote = 0) {uj::ox_n(..., comp = "or greater", n = n, first = F, conj = conj, quote = quote)}

#' @rdname ox
#' @export
ox_or_less <- function(..., conj = "and", n = 1, quote = 0) {uj::ox_n(..., comp = "or less", n = n, first = F, conj = conj, quote = quote)}

#' @rdname ox
#' @export
ox_or_fewer <- function(..., conj = "and", n = 1, quote = 0) {uj::ox_n(..., comp = "or fewer", n = n, first = F, conj = conj, quote = quote)}
