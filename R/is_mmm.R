#' @name is_mmm.
#' @family props
#' @title Class-Agnostic Extended Atomic Modes
#' @description This family of functions adds missing value and emptiness checks
#'   and defines new dynamically-evaluated modes. The full set of extended modes
#'   is given in the following tables.
#'   \strong{Character Extended Modes}\tabular{lll}{
#'   EXTENDED    \tab EXTENDED       \tab QUALIFYING                         \cr
#'   MODE VALUE   \tab MODE NAME     \tab CHARACTERISTICS                    \cr
#'   \code{'chr'}\tab Character      \tab Character.                         \cr
#'   \code{'clr'}\tab Xolor          \tab Hex color value or color name.     \cr
#'   \code{'ch1'}\tab Onechar        \tab Single characters.                 \cr
#'   \code{'str'}\tab String         \tab No blank ("") strings                }
#'   \strong{Categorical Extended Modes}\tabular{lll}{
#'   EXTENDED    \tab EXTENDED       \tab QUALIFYING                         \cr
#'   MODE VALUE   \tab MODE NAME     \tab CHARACTERISTICS                    \cr
#'   \code{'fac'}\tab Factor         \tab Factor.                            \cr
#'   \code{'lgl'}\tab Logical        \tab Logical.                           \cr
#'   \code{'ord'}\tab Ordered        \tab Ordered factor.                    \cr
#'   \code{'uno'}\tab Unordered      \tab Unordered factor.                    }
#'   \strong{Combination Extended Modes}\tabular{lll}{
#'   EXTENDED    \tab EXTENDED       \tab QUALIFYING                         \cr
#'   MODE VALUE   \tab MODE NAME     \tab CHARACTERISTICS                    \cr
#'   \code{'ind'}\tab Indexer        \tab Logical or positive whole number.  \cr
#'   \code{'srt'}\tab Sortable       \tab Character, logical, numeric, or
#'                                        ordered factor.                    \cr
#'   \code{'nst'}\tab non-sortable   \tab Atomic, but not sortable.            }
#'   \strong{Numeric Extended Modes}\tabular{lll}{
#'   EXTENDED    \tab EXTENDED       \tab QUALIFYING                         \cr
#'   MODE VALUE   \tab MODE NAME     \tab CHARACTERISTICS                    \cr
#'   \code{'num'}\tab Numeric        \tab Numeric.                           \cr
#'   \code{'frc'}\tab Fractional     \tab Fractionally numeric.              \cr
#'   \code{'pct'}\tab Percent        \tab Percentage (in \[0, 100\]).        \cr
#'   \code{'ppn'}\tab Proportion     \tab Proportion (in \[0, 1\]).          \cr
#'   \code{'pos'}\tab Positive       \tab Positive numeric.                  \cr
#'   \code{'nng'}\tab Non-negative   \tab Non-negative numeric.              \cr
#'   \code{'nps'}\tab Non-positive   \tab Non-positive numeric.             \cr
#'   \code{'neg'}\tab Negative       \tab Negative numeric.                  \cr
#'   \code{'whl'}\tab Whole          \tab Whole number.                      \cr
#'   \code{'evn'}\tab Even           \tab Even (whole) number.               \cr
#'   \code{'odd'}\tab Odd            \tab Odd (whole) number.                \cr
#'   \code{'psw'}\tab Positive whole \tab Positive whole-number.             \cr
#'   \code{'nnw'}\tab Non-neg whole  \tab Non-negative whole-number.         \cr
#'   \code{'npw'}\tab Non-pos whole  \tab Non-positive whole-number.          \cr
#'   \code{'ngw'}\tab Negative whole \tab Negative whole-number.                 }
#' @param x An object to check for properties.
#' @param na,emp \link[cmp_lgl_scl]{Complete logical scalars} indicating whether
#'   \code{NA} values are allowed and whether empty atomic objects qualify,
#'   respectively.
#' @return Logical scalar
#' @export
is_mmm. <- function() {help("is_mmm.", package = "uj")}

#' @describeIn is_mmm. Check for atomic mode.
#' @export
is_atm <- function(x, na = FALSE, emp = FALSE) {
  err <- NULL
  if (!isTF(na )) {err <- c(err, "\n • [na] must be TRUE or FALSE.")}
  if (!isTF(emp)) {err <- c(err, "\n • [emp] must be TRUE or FALSE.")}
  if (!is.null(err)) {stop(err)}
  if (is.null(x) | !is.atomic(x)) {F} else if (!emp & length(x) == 0) {F} else {(na | !any(is.na(av(x))))}
}

#' @describeIn is_mmm. Check for character mode.
#' @export
is_chr <- function(x, na = FALSE, emp = FALSE) {if (!is_atm(x, na, emp)) {F} else if (all(is.na(x)) | length(x) == 0) {T} else {is.character(x)} }

#' @describeIn is_mmm. Check for onechar (single character valued) mode.
#' @return Logical scalar.
#' @export
is_ch1 <- function(x, na = FALSE, emp = FALSE) {if (!is_chr(x, na, emp)) {F} else if (all(is.na(x)) | length(x) == 0) {T} else {all(nchar(x[!is.na(x)]) == 1)}}

#' @describeIn is_mmm. Check for valid character color mode.
#' @export
is_clr <- function(x, na = FALSE, emp = FALSE) {if (!is_chr(x, na, emp)) {F} else if (all(is.na(x)) | length(x) == 0) {T} else {!isERR(as_clr(x, na = na))}}

#' @describeIn is_mmm. Check for factor mode.
#' @export
is_fac <- function(x, na = FALSE, emp = FALSE) {if (!is_atm(x, na, emp)) {F} else if (all(is.na(x)) | length(x) == 0) {T} else {is.factor(x)}}

#' @describeIn is_mmm. Check for logical mode.
#' @export
is_lgl <- function(x, na = FALSE, emp = FALSE) {if (!is_atm(x, na, emp)) {F} else if (all(is.na(x)) | length(x) == 0) {T} else {is.logical(x)}}

#' @describeIn is_mmm. Check for numeric mode.
#' @export
is_num <- function(x, na = FALSE, emp = FALSE) {if (!is_atm(x, na, emp)) {F} else if (all(is.na(x)) | length(x) == 0) {T} else {is.numeric(x)}}

#' @describeIn is_mmm. Check for even-number valued mode.
#' @export
is_evn <- function(x, na = FALSE, emp = FALSE) {if (!is_num(x, na, emp)) {F} else if (all(is.na(x)) | length(x) == 0) {T} else {all(x[!is.na(x)] / 2 == round(x[!is.na(x)] / 2))}}

#' @describeIn is_mmm. Check for odd number valued mode.
#' @export
is_odd <- function(x, na = FALSE, emp = FALSE) {if (!is_num(x, na, emp)) {F} else if (all(is.na(x)) | length(x) == 0) {T} else {all((1 + x[!is.na(x)]) / 2 == round((1 + x[!is.na(x)]) / 2))}}

#' @describeIn is_mmm. Check for at least one fractional numeric value mode.
#' @export
is_frc <- function(x, na = FALSE, emp = FALSE) {if (!is_num(x, na, emp)) {F} else if (all(is.na(x)) | length(x) == 0) {T} else {any(x[!is.na(x)] != round(x[!is.na(x)]))}}

#' @describeIn is_mmm. Check for negative numeric mode.
#' @export
is_neg <- function(x, na = FALSE, emp = FALSE) {if (!is_num(x, na, emp)) {F} else if (all(is.na(x)) | length(x) == 0) {T} else {all(x[!is.na(x)] < 0)}}

#' @describeIn is_mmm. Check for positive numeric mode.
#' @export
is_pos <- function(x, na = FALSE, emp = FALSE) {if (!is_num(x, na, emp)) {F} else if (all(is.na(x)) | length(x) == 0) {T} else {all(x[!is.na(x)] > 0)}}

#' @describeIn is_mmm. Check for non-positive numeric mode.
#' @export
is_nps <- function(x, na = FALSE, emp = FALSE) {if (!is_num(x, na, emp)) {F} else if (all(is.na(x)) | length(x) == 0) {T} else {all(x[!is.na(x)] <= 0)}}

#' @describeIn is_mmm. Check for non-negative numeric mode.
#' @export
is_nng <- function(x, na = FALSE, emp = FALSE) {if (!is_num(x, na, emp)) {F} else if (all(is.na(x)) | length(x) == 0) {T} else {all(x[!is.na(x)] >= 0)}}

#' @describeIn is_mmm. Check for whole-number numeric mode.
#' @export
is_whl <- function(x, na = FALSE, emp = FALSE) {if (!is_num(x, na, emp)) {F} else if (all(is.na(x)) | length(x) == 0) {T} else {all(x[!is.na(x)] == round(x[!is.na(x)]))}}

#' @describeIn is_mmm. Check for negative whole-number numeric mode.
#' @export
is_ngw <- function(x, na = FALSE, emp = FALSE) {if (!is_neg(x)) {F} else {all(x[!is.na(x)] == round(x[!is.na(x)]))}}

#' @describeIn is_mmm. Check for non-negative whole-number numeric mode.
#' @export
is_nnw <- function(x, na = FALSE, emp = FALSE) {if (!is_nng(x)) {F} else {all(x[!is.na(x)] == round(x[!is.na(x)]))}}

#' @describeIn is_mmm. Check for non-positive whole-number numeric mode.
#' @export
is_npw <- function(x, na = FALSE, emp = FALSE) {if (!is_nps(x)) {F} else {all(x[!is.na(x)] == round(x[!is.na(x)]))}}

#' @describeIn is_mmm. Check for positive whole-number numeric mode.
#' @export
is_psw <- function(x, na = FALSE, emp = FALSE) {if (!is_pos(x)) {F} else {all(x[!is.na(x)] == round(x[!is.na(x)]))}}

#' @describeIn is_mmm. Check for positive whole-number numeric mode.
#' @export
is_pct <- function(x, na = FALSE, emp = FALSE) {if (!is_pos(x)) {F} else {all(x[!is.na(x)] >= 0 & x[!is.na(x)] <= 100)}}

#' @describeIn is_mmm. Check for positive whole-number numeric mode.
#' @export
is_ppn <- function(x, na = FALSE, emp = FALSE) {if (!is_pos(x)) {F} else {all(x[!is.na(x)] >= 0 & x[!is.na(x)] <= 1)}}

#' @describeIn is_mmm. Check for ordered factor mode.
#' @export
is_ord <- function(x, na = FALSE, emp = FALSE) {if (!is_atm(x, na, emp)) {F} else if (all(is.na(x)) | length(x) == 0) {T} else {is.ordered(x)}}

#' @describeIn is_mmm. Check for string mode (no \code{NA} values, no blanks)..
#' @export
is_str <- function(x) {if (!is_chr(x)) {F} else {!any(x == v(blank))}}

#' @describeIn is_mmm. Check for non-sortable atomic mode. Non-sortable objects
#'   are atomic objects of modes that are not sortable. Sortable objects are
#'   non-empty atomic objects of mode 'character', 'logical', 'numeric', or
#'   'ordered'.
#' @export
is_nst <- function(x, na = FALSE, emp = FALSE) {if (!is_atm(x, na, emp)) {F} else if (all(is.na(x)) | length(x) == 0) {T} else {!is_chr(x) & !is_lgl(x) & !is_num(x) & !is_ord(x)}}

#' @describeIn is_mmm. Check for sortable atomic mode. Sortable objects are
#'   non-empty atomic objects of mode 'character', 'logical', 'numeric', or
#'   'ordered'. Non-sortable objects are atomic objects of modes that are not
#'   sortable.
#' @export
is_srt <- function(x, na = FALSE, emp = FALSE) {if (!is_atm(x, na, emp)) {F} else if (all(is.na(x)) | length(x) == 0) {T} else {is_chr(x) | is_lgl(x) | is_num(x) | is_ord(x)}}

#' @describeIn is_mmm. Check for unordered factor mode.
#' @export
is_uno <- function(x, na = FALSE, emp = FALSE) {if (!is_atm(x, na, emp)) {F} else if (all(is.na(x)) | length(x) == 0) {T} else {is.factor(x) & is.ordered(x)}}

#' @describeIn is_mmm. Check for indexer mode (logical or integer).
#' @export
is_ind <- function(x, na = FALSE, emp = FALSE) {is_lgl(x, na, emp) | is_psw(x, na, emp)}
