#' @encoding UTF-8
#' @family strings
#' @title Oxford-comma separated lists
#' @description Create Oxford-comma separated lists with a variety of templates (see *details*).
#' \cr\cr With the exception of `.N > length(uj::av(...))`, these functions appropriately process lists of length `1` and `2`.
#' @details Each function in this family operates from a template as shown in the following tables where `{conj}` and `{n}` represent the values of arguments `.CONJ` and `.N`; `{pref}` and `{comp}` indicate the potentially-`NULL` values of arguments `.PREF` and `.COMP`; and `[a]`, `[b]`, and `[z]` represents elements of a list.
#' \cr\cr **Simple lists**
#' \cr\cr These functions do not incorporate a number into the Oxford-comma separated list:
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
#' \cr\cr **Numeric-comparison lists**
#' \cr\cr These functions incorporate a number into the Oxford-comma separated list:
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
#' @param .PREF A \link[=cmp_chr_scl]{complete character scalar} prefix to prepend to the list.
#' @param .CONJ A complete character scalar conjunction to use between the next to last and last elements of the list. Typical values are `and`, `or` and `nor`.
#' @param .N A \link[=cmp_psw_scl]{complete positive whole-number scalar}.
#' @param .COMP A complete character scalar used for comparing to `.N`, such as `'at least'` or `'or fewer'`.
#' @param .FIRST `TRUE` or `FALSE` used to determine whether `.COMP` is placed in front of `.N` rather than after `.N`.
#' @param .QUOTE `0`, `1`, or `2` indicating whether to leave list elements unquoted, single-quote the list elements, or double-quote the list elements.
#' @return A character scalar.
#' @examples
#' egFruits <- c("apples", "bananas", "oranges")
#'
#' ox(egFruits)
#' ox("apples", "bananas", "orange")
#' ox_or(egFruits)
#' ox_and(egFruits)
#' ox_nor(egFruits)
#' ox_nor(egFruits, .PREF = "")
#'
#' ox(egFruits)
#' ox(egFruits, .CONJ = "or")
#' ox(egFruits, .PREF = "neither", .CONJ = "nor")
#'
#' ox(egFruits, .PREF = "", .CONJ = "nor")
#' ox(egFruits, .PREF = "either", .CONJ = "or")
#' ox(egFruits, .PREF = "all of", .CONJ = "and")
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
#' ox_n(egFruits, .N = 1)
#' ox_n(egFruits, .CONJ = "and", .N = 2, .COMP = "from among")
#' ox_n(egFruits, .CONJ = "and", .N = 2, .COMP = "at least", .FIRST = TRUE)
#' ox_n(egFruits, .CONJ = "and", .N = 2, .COMP = "or more", .FIRST = FALSE)
#'
#' ox_exactly(egFruits, .N = 2)
#' ox_less(egFruits, .N = 2)
#' ox_more(egFruits, .N = 2)
#' ox_fewer(egFruits, .N = 2)
#' ox_greater(egFruits, .N = 2)
#' ox_at_least(egFruits, .N = 2)
#' ox_at_most(egFruits, .N = 2)
#' ox_no_greater(egFruits, .N = 2)
#' ox_no_fewer(egFruits, .N = 2)
#' ox_no_more(egFruits, .N = 2)
#' ox_no_less(egFruits, .N = 2)
#' ox_or_more(egFruits, .N = 2)
#' ox_or_greater(egFruits, .N = 2)
#' ox_or_less(egFruits, .N = 2)
#' ox_or_fewer(egFruits, .N = 2)
#' @export
ox <- function(..., .CONJ = "and", .PREF = "", .QUOTE = 0) {
  Vals <- uj::av(...)
  if (!uj:::.cmp_nnw_scl(.QUOTE)) {OkQuote <- F} else {OkQuote <- .QUOTE %in% 0:2}
  Errors <- NULL
  if (base::length(Vals) == 0) {Errors <- base::c(Errors, "[...] is empty.")}
  if (!uj:::.cmp_chr_scl(.CONJ)) {Errors <- base::c(Errors, "[.CONJ] must be a complete character scalar (?cmp_chr_scl).")}
  if (!uj:::.cmp_chr_scl(.PREF)) {Errors <- base::c(Errors, "[.PREF] must be a complete character scalar (?cmp_chr_scl).")}
  if (!OkQuote) {Errors <- base::c(Errors, "[.QUOTE] must be 0, 1, or 2.")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  nVals <- base::length(Vals)
  if (.QUOTE == 1) {Vals <- base::paste0("'", Vals, "'")}
  else if (.QUOTE == 2) {Vals <- base::paste0("\"", Vals, "\"")}
  if (nVals == 1) {
    if (.CONJ == "nor") {uj::stopperr("[.CONJ = 'nor'], but [...] contains only 1 atomic element.", .PKG = "uj")}
    else if (.CONJ == "or" & .PREF == "either") {uj::stopperr("[.CONJ = 'or'] and [.PREF = 'either'], but [...] contains only 1 atomic element.", .PKG = "uj")}
  }
  if (.PREF != "") {.PREF <- base::paste0(.PREF, " ")}                             # if pref(ix) is not empty, follow it with a space
  Last <- Vals[nVals]                                                           # get the last element of X
  if (nVals > 1) {                                                              # if there is more than one element in the list
    List <- base::paste0(Vals[1:(nVals - 1)], collapse = ", ")                  # : create a comma separated list with all but the last element of X
    if (nVals == 2) {ConjPref <- " "} else {ConjPref <- ", "}
    ConjSuff <- " "
    .CONJ <- base::paste0(ConjPref, .CONJ, ConjSuff)
  } else {List <- .CONJ <- ""}
  base::paste0(.PREF, List, .CONJ, Last)
}

#' @rdname ox
#' @export
oxford <- ox

#' @rdname ox
#' @export
oxford_comma <- ox

#' @rdname ox
#' @export
ox_n <- function(..., .CONJ = "and", .COMP = "", .QUOTE = 0, .N = 1, .FIRST = TRUE) {
  Vals <- uj::av(...)
  if (!uj:::.cmp_nnw_scl(.QUOTE)) {OkQuote <- F} else {OkQuote <- .QUOTE %in% 0:2}
  Errors <- NULL
  if (base::length(Vals) == 0) {Errors <- base::c(errs, "[...] is empty.")}
  if (uj:::.cmp_chr_scl(.CONJ)) {Errors <- base::c(errs, "[.CONJ] must be a complete character scalar (?cmp_chr_scl).")}
  if (uj:::.cmp_chr_scl(.COMP)) {Errors <- base::c(errs, "[.COMP] must be a complete character scalar (?cmp_chr_scl).")}
  if (!OkQuote) {Errors <- base::c(Errors, "[.QUOTE] must be 0, 1, or 2.")}
  if (!uj:::.cmp_nnw_scl(.N)) {Errors <- base::c(Errors, "[.N] must be a complete non-negative whole-number scalar (?cmp_nnw_scl)")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  if (.COMP == "") {.PREF <- base::paste(.N, "of")}
  else if (.FIRST) {.PREF <- base::paste(.COMP, .N, "of")}
  else            {.PREF <- base::paste(.N, .COMP, "of")}
  uj::ox(Vals, .CONJ = .CONJ, .PREF = .PREF, .QUOTE = .QUOTE)
}

#' @rdname ox
#' @export
ox_and <- function(..., .PREF = "", .QUOTE = 0) {uj::ox(..., .PREF = .PREF, .CONJ = "and", .QUOTE = .QUOTE)}

#' @rdname ox
#' @export
ox_or <- function(..., .PREF = "", .QUOTE = 0) {uj::ox(..., .PREF = .PREF, .CONJ = "or", .QUOTE = .QUOTE)}

#' @rdname ox
#' @export
ox_nor <- function(..., .PREF = "neither", .QUOTE = 0) {uj::ox(..., .PREF = .PREF, .CONJ = "nor", .QUOTE = .QUOTE)}

#' @rdname ox
#' @export
ox_either <- function(..., .QUOTE = 0) {uj::ox(..., .PREF = "either", .CONJ = "or", .QUOTE = .QUOTE)}

#' @rdname ox
#' @export
ox_neither <- function(..., .QUOTE = 0) {uj::ox(..., .PREF = "neither", .CONJ = "nor", .QUOTE = .QUOTE)}

#' @rdname ox
#' @export
ox_all <- function(..., .CONJ = "and", .QUOTE = 0) {uj::ox(..., .PREF = "all of", .CONJ = .CONJ, .QUOTE = .QUOTE)}

#' @rdname ox
#' @export
ox_any <- function(..., .CONJ = "or", .QUOTE = 0) {uj::ox(..., .PREF = "any of", .CONJ = .CONJ, .QUOTE = .QUOTE)}

#' @rdname ox
#' @export
ox_none <- function(..., .CONJ = "or", .QUOTE = 0) {uj::ox(..., .PREF = "none of", .CONJ = .CONJ, .QUOTE = .QUOTE)}

#' @rdname ox
#' @export
ox_some <- function(..., .CONJ = "and", .QUOTE = 0) {uj::ox(..., .PREF = "some of", .CONJ = .CONJ, .QUOTE = .QUOTE)}

#' @rdname ox
#' @export
ox_exactly <- function(..., .CONJ = "or", .N = 1, .QUOTE = 0) {uj::ox_n(..., comp = "exactly", .N = .N, .CONJ = .CONJ, .QUOTE = .QUOTE)}

#' @rdname ox
#' @export
ox_less <- function(..., .CONJ = "and", .N = 2, .QUOTE = 0) {uj::ox_n(..., .COMP = "less than", .N = .N, .CONJ = .CONJ, .QUOTE = .QUOTE)}

#' @rdname ox
#' @export
ox_more <- function(..., .CONJ = "and", .N = 1, .QUOTE = 0) {uj::ox_n(..., .COMP = "more than", .N = .N, .CONJ = .CONJ, .QUOTE = .QUOTE)}

#' @rdname ox
#' @export
ox_fewer <- function(..., .CONJ = "and", .N = 2, .QUOTE = 0) {uj::ox_n(..., .COMP = "fewer than", .N = .N, .CONJ = .CONJ, .QUOTE = .QUOTE)}

#' @rdname ox
#' @export
ox_greater <- function(..., .CONJ = "and", .N = 2, .QUOTE = 0) {uj::ox_n(..., .COMP = "greater than", .N = .N, .CONJ = .CONJ, .QUOTE = .QUOTE)}

#' @rdname ox
#' @export
ox_at_least <- function(..., .CONJ = "and", .N = 1, .QUOTE = 0) {uj::ox_n(..., .COMP = "at least", .N = .N, .CONJ = .CONJ, .QUOTE = .QUOTE)}

#' @rdname ox
#' @export
ox_at_most <- function(..., .CONJ = "and", .N = 1, .QUOTE = 0) {uj::ox_n(..., .COMP = "at most", .N = .N, .CONJ = .CONJ, .QUOTE = .QUOTE)}

#' @rdname ox
#' @export
ox_no_greater <- function(..., .CONJ = "and", .N = 1, .QUOTE = 0) {uj::ox_n(..., .COMP = "no greater than", .N = .N, .CONJ = .CONJ, .QUOTE = .QUOTE)}

#' @rdname ox
#' @export
ox_no_fewer <- function(..., .CONJ = "and", .N = 1, .QUOTE = 0) {uj::ox_n(..., .COMP = "no fewer than", .N = .N, .CONJ = .CONJ, .QUOTE = .QUOTE)}

#' @rdname ox
#' @export
ox_no_more <- function(..., .CONJ = "and", .N = 1, .QUOTE = 0) {uj::ox_n(..., .COMP = "no more than", .N = .N, .CONJ = .CONJ, .QUOTE = .QUOTE)}

#' @rdname ox
#' @export
ox_no_less <- function(..., .CONJ = "and", .N = 1, .QUOTE = 0) {uj::ox_n(..., .COMP = "no less than", .N = .N, .CONJ = .CONJ, .QUOTE = .QUOTE)}

#' @rdname ox
#' @export
ox_or_more <- function(..., .CONJ = "and", .N = 1, .QUOTE = 0) {uj::ox_n(..., .COMP = "or more", .N = .N, .FIRST = F, .CONJ = .CONJ, .QUOTE = .QUOTE)}

#' @rdname ox
#' @export
ox_or_greater <- function(..., .CONJ = "and", .N = 1, .QUOTE = 0) {uj::ox_n(..., .COMP = "or greater", .N = .N, .FIRST = F, .CONJ = .CONJ, .QUOTE = .QUOTE)}

#' @rdname ox
#' @export
ox_or_less <- function(..., .CONJ = "and", .N = 1, .QUOTE = 0) {uj::ox_n(..., .COMP = "or less", .N = .N, .FIRST = F, .CONJ = .CONJ, .QUOTE = .QUOTE)}

#' @rdname ox
#' @export
ox_or_fewer <- function(..., .CONJ = "and", .N = 1, .QUOTE = 0) {uj::ox_n(..., .COMP = "or fewer", .N = .N, .FIRST = F, .CONJ = .CONJ, .QUOTE = .QUOTE)}
