#' @name ttt.
#' @family props
#' @title Fundamental type properties
#' @description An object's fundamental type (ttt) is defined by its most
#'   basic structural properties as described in the following table:
#'   \tabular{lll}{
#'   FUNDAMENTAL   \tab FUNDAMENTAL   \tab CHARACTERISTICS OF                \cr
#'   TYPE VALUE    \tab TYPE NAME     \tab QUALIFYING OBJECTS                \cr
#'   \code{'nll'}\tab null        \tab \code{NULL}.                          \cr
#'   \code{'def'}\tab defined     \tab Not \code{NULL}.                      \cr
#'   \code{'nil'}\tab nil-length  \tab Length is 0 (includes \code{NULL}).   \cr
#'   \code{'pop'}\tab populated   \tab Length is > 0.                        \cr
#'   \code{'fun'}\tab function reference
#'                                \tab Function object or character scalar
#'                                     containing a function name.           \cr
#'   \code{'atm'}\tab atomic      \tab Atomic.                               \cr
#'   \code{'rcr'}\tab recursive   \tab List-like (data.frames, lists, or other
#'                                     objects containing an element is not an
#'                                     atomic scalar).                         }
#'   Functions related to fundamental type are described in the following
#'   table:\tabular{ll}{
#'   FUNCTION          \tab WHAT THE                                         \cr
#'   FORMAT            \tab FUNCTION DOES                                    \cr
#'   \code{i[ttt]}     \tab Evaluates whether an object is of the fundamental
#'                          type represented by the placeholder \code{[ttt]}.\cr
#'   \code{ttt}        \tab Gets a character vector containing all fundamental
#'                          type properties of an object.                    \cr
#'   \code{ittt}       \tab Evaluates an object for a fundamental type
#'                          \code{ttt} and any additional properties specified
#'                          in \code{...}.                                   \cr
#'   \code{ttt_vals}   \tab Gets a character vector of all possible fundamental
#'                          type property values.                              }
#' @param x An object.
#' @param ttt \link[=cmp_chr_scl]{Complete character scalar }containing one or
#'   more values from \code{ttt_vals()} separated by pipes and/or underscores
#'   ("."). Combinations of fundamental types can be specified by separating
#'   them with underscores. Separating fundamental types or combinations of
#'   fundamental types with pipes will result in a value of \code{TRUE} if any
#'   of them applies to \code{x}.
#' @inheritDotParams meets.
#' @inheritSection meets. Specifying Additional Property Requirements
#' @return \tabular{lll}{
#'   \code{ttt}                   \tab   \tab A character vector.            \cr
#'   \code{ttt_vals}              \tab   \tab A character vector.            \cr
#'   \code{ittt} and \code{i[ttt]}\tab   \tab A logical scalar.                }
#' @export
ttt. <- function() {help("ttt.", package = "uj")}

#' @rdname ttt.
#' @export
ttt <- function(x) {c(f0(is.atomic(x), 'atm', NULL), f0(is.null(x), 'nll', 'def'), f0(0 < length(x), 'pop', 'nil'), f0(is.recursive(x), 'rcr', NULL), f0( is.function(x), 'fun', f0(!isERR(match.fun(x)), 'fun', NULL)))}

#' @rdname ttt.
#' @export
iatm <- function(x) {is.atomic(x)}

#' @rdname ttt.
#' @export
idef <- function(x) {!is.null(x)}

#' @rdname ttt.
#' @export
ifun <- function(x) {if (is.function(x)) {T} else {!isERR(match.fun(x))}}

#' @rdname ttt.
#' @export
inil <- function(x) {length(x) == 0}

#' @rdname ttt.
#' @export
inll <- function(x) {is.null(x)}

#' @rdname ttt.
#' @export
ipop <- function(x) {length(x) > 0}

#' @rdname ttt.
#' @export
ircr <- function(x) {is.recursive(x)}

#' @rdname ttt.
#' @export
ttt_vals <- function() {c('atm', 'def', 'fun', 'nil', 'nll', 'pop', 'rcr')}

#' @rdname ttt.
#' @export
ittt <- function(x, ttt, ...) {
  if (!cmp_chr_scl(ttt)) {stop("\n \u2022 [ttt] must be a non-NA character scalar.")}
  valid.ttt <- ttt_vals()
  ttt.combos <- strsplit(ttt, "|", fixed = T)[[1]]
  new.ttt <- unlist(strsplit(ttt.combos, "_", fixed = T))
  if (!all(new.ttt %in% valid.ttt)) {stop("\n \u2022 [ttt = '", ttt, "'] specifies a 'fundamental type' property not in [uj::ttt_vals() = c(", paste0(paste0("'", ttt_vals(), "'"), collapse = ", "), ")].")}
  ippp(x, ttt, ...)
}
