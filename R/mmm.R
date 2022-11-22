#' @name mmm
#' @family props
#' @title Extended mode properties
#' @description NOTE: \code{MMM} is used as a wildcard representing any given
#'   extended mode.
#'   \cr\cr
#'   Extended modes are defined for non-empty atomic objects. For all
#'   other objects, the extended mode is \code{NULL}. These are not formally
#'   defined classes, but are evaluated dynamically based on the current
#'   characteristics of an object.
#'   \cr\cr
#'   Atomic objects that contain only \code{NA} values are of every extended
#'   mode, as they can be coerced to any mode without introducing new \code{NA}
#'   values. The following tables gives extended mode values, names, and
#'   requirements
#'   \cr\cr
#'   \strong{Character Extended Modes}\tabular{lll}{
#'     PROPERTY    \tab PROPERTY   \tab QUALIFYING                           \cr
#'     VALUE       \tab NAME       \tab CHARACTERISTICS                      \cr
#'     \code{'chr'}\tab Character  \tab Character.                           \cr
#'     \code{'clr'}\tab Color      \tab Valid character color representation.\cr
#'     \code{'ch1'}\tab Onechar    \tab Single character values.             \cr
#'     \code{'str'}\tab String     \tab No blank ("") values.                  }
#'   \strong{Categorical Extended Modes}\tabular{lll}{
#'     PROPERTY    \tab PROPERTY   \tab QUALIFYING                           \cr
#'     VALUE       \tab NAME       \tab CHARACTERISTICS                      \cr
#'     \code{'fac'}\tab Factor     \tab Factor.                              \cr
#'     \code{'lgl'}\tab Logical    \tab Logical.                             \cr
#'     \code{'ord'}\tab Ordered    \tab Ordered factor.                      \cr
#'     \code{'uno'}\tab Unordered  \tab Unordered factor.                      }
#'   \strong{Combination Extended Modes}\tabular{lll}{
#'     PROPERTY    \tab PROPERTY    \tab QUALIFYING                          \cr
#'     VALUE       \tab NAME        \tab CHARACTERISTICS                     \cr
#'     \code{'ind'}\tab Indexer     \tab Logical or positive whole number.   \cr
#'     \code{'srt'}\tab Sortable    \tab Character, logical, numeric, or
#'                                      ordered factor.                      \cr
#'     \code{'nst'}\tab non-sortable\tab Atomic, but not sortable.             }
#'   \strong{Numeric Extended Modes}\tabular{lll}{
#'     PROPERTY    \tab PROPERTY          \tab QUALIFYING                    \cr
#'     VALUE       \tab NAME              \tab CHARACTERISTICS               \cr
#'     \code{'num'}\tab Numeric           \tab Numeric.                      \cr
#'     \code{'frc'}\tab Fractional        \tab At least one non-\code{NA} value
#'                                             is fractional (i.e., not a
#'                                             whole number).                \cr
#'     \code{'pct'}\tab Percent           \tab Percentage numeric (in the
#'                                             interval \code{[0, 100]}).    \cr
#'     \code{'ppn'}\tab Proportion        \tab Proportion numeric (in the
#'                                             interval \code{[0, 1]}).      \cr
#'     \code{'pos'}\tab Positive          \tab Positive numeric.             \cr
#'     \code{'nng'}\tab Non-negative      \tab Non-negative numeric.         \cr
#'     \code{'nps'}\tab Non-positive      \tab Non-positive numeric.         \cr
#'     \code{'neg'}\tab Negative          \tab Negative numeric.             \cr
#'     \code{'whl'}\tab Whole             \tab Whole number.                 \cr
#'     \code{'evn'}\tab Even              \tab Even (whole) number.          \cr
#'     \code{'odd'}\tab Odd               \tab Odd (whole) number.           \cr
#'     \code{'psw'}\tab Positive whole    \tab Positive whole-number.        \cr
#'     \code{'nnw'}\tab Non-negative whole\tab Non-negative whole-number.    \cr
#'     \code{'npw'}\tab Non-positive whole\tab Non-positive whole-number.    \cr
#'     \code{'ngw'}\tab Negative whole    \tab Negative whole-number.          }
#' @section Functions in this Family:
#'   \strong{\code{iMMM}}
#'   \cr Evaluates whether \code{x} matches the extended mode property
#'   \code{MMM} (subject to any restrictions in \code{...}).
#'   \cr\cr
#'   \strong{\code{mmm}}
#'   \cr Gets a character vector containing all extended mode properties
#'   matching \code{x}.
#'   \cr\cr
#'   \strong{\code{immm}}
#'   \cr Evaluates \code{x} against the extended mode property specification in
#'   \code{spec} (subject to any restrictions in \code{...}).
#'   \cr\cr
#'   \strong{\code{mmm_props}}
#'   \cr Gets a character vector of all possible extended mode property values.
#'   \cr\cr
#'   \strong{\code{is_mmm_spec}}
#'   \cr Evaluates whether \code{spec} is a valid extended mode property
#'   specification.
#' @param x An R object.
#' @param spec \code{NULL} or a \link[=cmp_chr_scl]{complete character vec}
#'   containing one or more extended mode properties (i.e., from
#'   \code{mmm_vals()}). \strong{NOTE}: properties may be pipe-separated. If
#'   If there are multiple properties in \code{spec}, \code{x} is inspected for
#'   a match to any of the specified properties.
#' @inheritDotParams meets
#' @inheritSection meets Specifying Count and Value Restrictions
#' @return \strong{\code{mmm_vals}}
#'   \cr A character vector.
#'   \cr\cr
#'   \strong{\code{mmm}}
#'   \cr A character scalar or character vector.
#'   \cr\cr
#'   \strong{\code{iMMM, immm, is_mmm_spec}}
#'   \cr A logical scalar.
#' @export
mmm <- function(x) {
  props <- .mmm_props()
  out <- NULL
  for (prop in props) {out <- c(out, f0(run('.is_', prop, '(x)'), prop, NULL))}
  out
}

#' @rdname mmm
#' @export
mmm_props <- function() {.mmm_props()}

#' @rdname mmm
#' @export
is_mmm_spec <- function(spec) {spec <- .spec_vals(); f0(length(spec) == 0, F, all(spec %in% .mmm_props()))}

#' @rdname mmm
#' @export
immm <- function(x, spec, ...) {
  errs <- c(.meets_errs(x, ...), f0(is_mmm_spec(spec), NULL, '\n \u2022 [spec] must be a complete character vec (?cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from mmm_props().'))
  if (!is.null(errs)) {stop(errs)}
  if (!meets(x, ...)) {return(F)}
  for (prop in .spec_vals(spec)) {if (run('.is_', prop, '(x)')) {return(T)}}
  F
}

#' @rdname mmm
#' @export
ich1 <- function(x, ...) {immm(x, 'ch1', ...)}

#' @rdname mmm
#' @export
ich3 <- function(x, ...) {immm(x, 'ch3', ...)}

#' @rdname mmm
#' @export
ichr <- function(x, ...) {immm(x, 'chr', ...)}

#' @rdname mmm
#' @export
iclr <- function(x, ...) {immm(x, 'clr', ...)}

#' @rdname mmm
#' @export
ievn <- function(x, ...) {immm(x, 'evn', ...)}

#' @rdname mmm
#' @export
ifac <- function(x, ...) {immm(x, 'fac', ...)}

#' @rdname mmm
#' @export
ifrc <- function(x, ...) {immm(x, 'frc', ...)}

#' @rdname mmm
#' @export
iind <- function(x, ...) {immm(x, 'ind', ...)}

#' @rdname mmm
#' @export
ilgl <- function(x, ...) {immm(x, 'lgl', ...)}

#' @rdname mmm
#' @export
ineg <- function(x, ...) {immm(x, 'neg', ...)}

#' @rdname mmm
#' @export
ingw <- function(x, ...) {immm(x, 'ngw', ...)}

#' @rdname mmm
#' @export
inng <- function(x, ...) {immm(x, 'nng', ...)}

#' @rdname mmm
#' @export
innw <- function(x, ...) {immm(x, 'nnw', ...)}

#' @rdname mmm
#' @export
inps <- function(x, ...) {immm(x, 'nps', ...)}

#' @rdname mmm
#' @export
inpw <- function(x, ...) {immm(x, 'npw', ...)}

#' @rdname mmm
#' @export
inst <- function(x, ...) {immm(x, 'nst', ...)}

#' @rdname mmm
#' @export
inum <- function(x, ...) {immm(x, 'num', ...)}

#' @rdname mmm
#' @export
iodd <- function(x, ...) {immm(x, 'odd', ...)}

#' @rdname mmm
#' @export
iord <- function(x, ...) {immm(x, 'ord', ...)}

#' @rdname mmm
#' @export
ipct <- function(x, ...) {immm(x, 'pct', ...)}

#' @rdname mmm
#' @export
ipos <- function(x, ...) {immm(x, 'pos', ...)}

#' @rdname mmm
#' @export
ippn <- function(x, ...) {immm(x, 'ppn', ...)}

#' @rdname mmm
#' @export
ipsw <- function(x, ...) {immm(x, 'psw', ...)}

#' @rdname mmm
#' @export
isrt <- function(x, ...) {immm(x, 'srt', ...)}

#' @rdname mmm
#' @export
istr <- function(x, ...) {immm(x, 'str', ...)}

#' @rdname mmm
#' @export
iuno <- function(x, ...) {immm(x, 'uno', ...)}

#' @rdname mmm
#' @export
iwhl <- function(x, ...) {immm(x, 'whl', ...)}
