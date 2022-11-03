#' @name is_xxx
#' @family props
#' @title Three-character is_xxx aliases for basic atomic is.mode functions.
#' @details This family of functions adds missing value and emptiness checks and
#'   defines new dynamically-evaluated modes, including one-char
#'   (single-character valued), colors (valid character color values), unordered
#'   factor, sortable, and non-sortable,
#' @description Check for 3-character object property.
#' @param x An object to check for properties.
#' @param na,emp Logical scalars indicating whether \code{NA} values are allowed
#'   and whether empty atomic objects qualify, respectively.
#' @return Logical scalar
#' @export
is_xxx <- NULL

#' @describeIn is_xxx Check for atomic mode.
#' @export
is_atm <- function(x, na = FALSE, emp = FALSE) {
  err <- NULL
  if (!isTF(na )) {err <- c(err, "\n • [na] must be TRUE or FALSE.")}
  if (!isTF(emp)) {err <- c(err, "\n • [emp] must be TRUE or FALSE.")}
  if (!is.null(err)) {stop(err)}
  if (is.null(x) | !is.atomic(x)) {F} else if (!emp & length(x) == 0) {F} else {(na | !any(is.na(av(x))))}
}

#' @describeIn is_xxx Check for character mode.
#' @export
is_chr <- function(x, na = FALSE, emp = FALSE) {if (!is_atm(x, na, emp)) {F} else if (all(is.na(x)) | length(x) == 0) {T} else {is.character(x)} }

#' @describeIn is_xxx Check for onechar (single character valued) mode.
#' @return Logical scalar.
#' @export
is_ch1 <- function(x, na = FALSE, emp = FALSE) {if (!is_chr(x, na, emp)) {F} else if (all(is.na(x)) | length(x) == 0) {T} else {all(nchar(x[!is.na(x)]) == 1)}}

#' @describeIn is_xxx Check for valid character color mode.
#' @export
is_clr <- function(x, na = FALSE, emp = FALSE) {if (!is_chr(x, na, emp)) {F} else if (all(is.na(x)) | length(x) == 0) {T} else {!isERR(as_clr(x, na = na))}}

#' @describeIn is_xxx Check for factor mode.
#' @export
is_fac <- function(x, na = FALSE, emp = FALSE) {if (!is_atm(x, na, emp)) {F} else if (all(is.na(x)) | length(x) == 0) {T} else {is.factor(x)}}

#' @describeIn is_xxx Check for logical mode.
#' @export
is_lgl <- function(x, na = FALSE, emp = FALSE) {if (!is_atm(x, na, emp)) {F} else if (all(is.na(x)) | length(x) == 0) {T} else {is.logical(x)}}

#' @describeIn is_xxx Check for numeric mode.
#' @export
is_num <- function(x, na = FALSE, emp = FALSE) {if (!is_atm(x, na, emp)) {F} else if (all(is.na(x)) | length(x) == 0) {T} else {is.numeric(x)}}

#' @describeIn is_xxx Check for even-number valued mode.
#' @export
is_evn <- function(x, na = FALSE, emp = FALSE) {if (!is_num(x, na, emp)) {F} else if (all(is.na(x)) | length(x) == 0) {T} else {all(x[!is.na(x)] / 2 == round(x[!is.na(x)] / 2))}}

#' @describeIn is_xxx Check for odd number valued mode.
#' @export
is_odd <- function(x, na = FALSE, emp = FALSE) {if (!is_num(x, na, emp)) {F} else if (all(is.na(x)) | length(x) == 0) {T} else {all((1 + x[!is.na(x)]) / 2 == round((1 + x[!is.na(x)]) / 2))}}

#' @describeIn is_xxx Check for at least one fractional numeric value mode.
#' @export
is_frc <- function(x, na = FALSE, emp = FALSE) {if (!is_num(x, na, emp)) {F} else if (all(is.na(x)) | length(x) == 0) {T} else {any(x[!is.na(x)] != round(x[!is.na(x)]))}}

#' @describeIn is_xxx Check for negative numeric mode.
#' @export
is_neg <- function(x, na = FALSE, emp = FALSE) {if (!is_num(x, na, emp)) {F} else if (all(is.na(x)) | length(x) == 0) {T} else {all(x[!is.na(x)] < 0)}}

#' @describeIn is_xxx Check for positive numeric mode.
#' @export
is_pos <- function(x, na = FALSE, emp = FALSE) {if (!is_num(x, na, emp)) {F} else if (all(is.na(x)) | length(x) == 0) {T} else {all(x[!is.na(x)] > 0)}}

#' @describeIn is_xxx Check for non-positive numeric mode.
#' @export
is_nps <- function(x, na = FALSE, emp = FALSE) {if (!is_num(x, na, emp)) {F} else if (all(is.na(x)) | length(x) == 0) {T} else {all(x[!is.na(x)] <= 0)}}

#' @describeIn is_xxx Check for non-negative numeric mode.
#' @export
is_nng <- function(x, na = FALSE, emp = FALSE) {if (!is_num(x, na, emp)) {F} else if (all(is.na(x)) | length(x) == 0) {T} else {all(x[!is.na(x)] >= 0)}}

#' @describeIn is_xxx Check for whole-number numeric mode.
#' @export
is_whl <- function(x, na = FALSE, emp = FALSE) {if (!is_num(x, na, emp)) {F} else if (all(is.na(x)) | length(x) == 0) {T} else {all(x[!is.na(x)] == round(x[!is.na(x)]))}}

#' @describeIn is_xxx Check for negative whole-number numeric mode.
#' @export
is_ngw <- function(x, na = FALSE, emp = FALSE) {if (!is_neg(x)) {F} else {all(x[!is.na(x)] == round(x[!is.na(x)]))}}

#' @describeIn is_xxx Check for non-negative whole-number numeric mode.
#' @export
is_nnw <- function(x, na = FALSE, emp = FALSE) {if (!is_nng(x)) {F} else {all(x[!is.na(x)] == round(x[!is.na(x)]))}}

#' @describeIn is_xxx Check for non-positive whole-number numeric mode.
#' @export
is_npw <- function(x, na = FALSE, emp = FALSE) {if (!is_nps(x)) {F} else {all(x[!is.na(x)] == round(x[!is.na(x)]))}}

#' @describeIn is_xxx Check for positive whole-number numeric mode.
#' @export
is_psw <- function(x, na = FALSE, emp = FALSE) {if (!is_pos(x)) {F} else {all(x[!is.na(x)] == round(x[!is.na(x)]))}}

#' @describeIn is_xxx Check for positive whole-number numeric mode.
#' @export
is_pct <- function(x, na = FALSE, emp = FALSE) {if (!is_pos(x)) {F} else {all(x[!is.na(x)] >= 0 & x[!is.na(x)] <= 100)}}

#' @describeIn is_xxx Check for positive whole-number numeric mode.
#' @export
is_ppn <- function(x, na = FALSE, emp = FALSE) {if (!is_pos(x)) {F} else {all(x[!is.na(x)] >= 0 & x[!is.na(x)] <= 1)}}

#' @describeIn is_xxx Check for ordered factor mode.
#' @export
is_ord <- function(x, na = FALSE, emp = FALSE) {if (!is_atm(x, na, emp)) {F} else if (all(is.na(x)) | length(x) == 0) {T} else {is.ordered(x)}}

#' @describeIn is_xxx Check for string mode (no \code{NA} values, no blanks)..
#' @export
is_str <- function(x) {if (!is_chr(x)) {F} else {!any(x == v(blank))}}

#' @describeIn is_xxx Check for non-sortable atomic mode. Non-sortable objects
#'   are atomic objects of modes that are not sortable. Sortable objects are
#'   non-empty atomic objects of mode 'character', 'logical', 'numeric', or
#'   'ordered'.
#' @export
is_nst <- function(x, na = FALSE, emp = FALSE) {if (!is_atm(x, na, emp)) {F} else if (all(is.na(x)) | length(x) == 0) {T} else {!is_chr(x) & !is_lgl(x) & !is_num(x) & !is_ord(x)}}

#' @describeIn is_xxx Check for sortable atomic mode. Sortable objects are
#'   non-empty atomic objects of mode 'character', 'logical', 'numeric', or
#'   'ordered'. Non-sortable objects are atomic objects of modes that are not
#'   sortable.
#' @export
is_srt <- function(x, na = FALSE, emp = FALSE) {if (!is_atm(x, na, emp)) {F} else if (all(is.na(x)) | length(x) == 0) {T} else {is_chr(x) | is_lgl(x) | is_num(x) | is_ord(x)}}

#' @describeIn is_xxx Check for unordered factor mode.
#' @export
is_uno <- function(x, na = FALSE, emp = FALSE) {if (!is_atm(x, na, emp)) {F} else if (all(is.na(x)) | length(x) == 0) {T} else {is.factor(x) & is.ordered(x)}}

#' @describeIn is_xxx Check for indexer mode (logical or integer).
#' @export
is_ind <- function(x, na = FALSE, emp = FALSE) {is_lgl(x, na, emp) | is_psw(x, na, emp)}
