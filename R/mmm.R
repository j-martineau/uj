.is_mmm <- function(x) {!is.null(x) | !is.atomic(x)}
.is_MMM <- function(x) {f0(length(x) == 0, T, all(is.na(x)))}
.ich1 <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.character(x), F, all(nchar(x[!is.na(x)]) == 1))))}
.ich3 <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.character(x), F, all(nchar(x[!is.na(x)]) == 3))))}
.ichr <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, is.character(x)))}
.iclr <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.character(x), F, !any(c("error", "simpleError") %in% class(failsafe(col2rgb(x[!is.na(x)])))))))}
.ievn <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.numeric(x), F, {x <- x[!is.na(x)]; all(round(x / 2) == x / 2)})))}
.ifac <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, is.factor(x)))}
.ifrc <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.numeric(x), F, {x <- x[!is.na(x)]; any(round(x) != round(x))})))}
.iind <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, is.logical(x) | all()))}
.ilgl <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, is.logical(x)))}
.ineg <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.numeric(x), F, all(x[!is.na(x)] < 0))))}
.ingw <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.numeric(x), F, {x <- x[!is.na(x)]; all(x < 0 & round(x) == x)})))}
.inng <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.numeric(x), F, !any(x[!is.na(x)] < 0))))}
.innw <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.numeric(x), F, {x <- x[!is.na(x)]; all(x >= 0 & round(x) == x)})))}
.inps <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.numeric(x), F, !any(x[!is.na(x)] > 0))))}
.inpw <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.numeric(x), F, {x <- x[!is.na(x)]; all(x <= 0 & round(x) == x)})))}
.inst <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(is.factor(x), !is.ordered(x), !(is.character(x) | is.numeric(x) | is.logical(x)))))}
.inum <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, is.numeric(x)))}
.iodd <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.numeric(x), F, {x <- x[!is.na(x)] + 1; all(round(x / 2) == x / 2)})))}
.iord <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, is.ordered(x)))}
.ipct <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.numeric(x), F, {x <- x[!is.na(x)]; all(x >= 0 & x <= 100)})))}
.ipos <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.numeric(x), F, all(x[!is.na(x)] > 0))))}
.ippn <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.numeric(x), F, {x <- x[!is.na(x)]; all(x >= 0 & x <= 1)})))}
.ipsw <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.numeric(x), F, {x <- x[!is.na(x)]; all(x > 0 & round(x) == x)})))}
.isrt <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(is.factor(x), is.ordered(x), is.character(x) | is.numeric(x) | is.logical(x))))}
.istr <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.character(x), F, !any(x[!is.na(x)] == ""))))}
.iuno <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, is.factor(x) & !is.ordered(x)))}
.iwhl <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.numeric(x), F, {x <- x[!is.na(x)]; all(round(x) == x)})))}
.mmms <- c("ch1", "ch3", "chr", "clr", "evn", "fac", "frc", "ind", "lgl", "neg", "ngw", "nng", "nnw", "nps", "npw", "nst", "num", "odd", "ord", "pct", "pos", "ppn", "psw", "srt", "str", "uno", "whl")

#' @name mmm
#' @family props
#' @title Extended Mode Properties
#' @description Extended modes are defined for non-empty atomic objects. For all
#'   other objects, the extended mode is `NULL`. These are not formally defined
#'   classes, but are evaluated dynamically based on the current characteristics
#'   of an object.
#'   \cr\cr
#'   Atomic objects that contain only `NA` values are of every extended mode, as
#'   they can be coerced to any mode without introducing new `NA` values. The
#'   following tables gives extended mode values, names, and requirements.
#'   \cr\cr
#'   \strong{Character Extended Mode Properties}\tabular{lll}{
#'   VALUE   \tab NAME        \tab CHARACTERISTICS                           \cr
#'   `'chr'` \tab Character   \tab Character.                                \cr
#'   `'clr'` \tab Color       \tab Valid character color representation.     \cr
#'   `'ch1'` \tab Onechar     \tab Single character values.                  \cr
#'   `'str'` \tab String      \tab No blank ("") values.                       }
#'   \strong{Categorical Extended Mode Properties` \tabular{lll}{
#'   VALUE   \tab NAME        \tab CHARACTERISTICS                           \cr
#'   `'fac'` \tab Factor      \tab Factor.                                   \cr
#'   `'lgl'` \tab Logical     \tab Logical.                                  \cr
#'   `'ord'` \tab Ordered     \tab Ordered factor.                           \cr
#'   `'uno'` \tab Unordered   \tab Unordered factor.                           }
#'   \strong{Combination Extended Mode Properties` \tabular{lll}{
#'   VALUE   \tab NAME           \tab CHARACTERISTICS                        \cr
#'   `'ind'` \tab Indexer        \tab Logical or positive whole number.      \cr
#'   `'srt'` \tab Sortable       \tab Character, logical, numeric, or ordered
#'                                    factor.                                \cr
#'   `'nst'` \tab non-sortable   \tab Atomic, but not sortable.                }
#'   \strong{Numeric Extended Mode Properties` \tabular{lll}{
#'   VALUE   \tab NAME                 \tab CHARACTERISTICS                  \cr
#'   `'num'` \tab Numeric              \tab Numeric.                         \cr
#'   `'frc'` \tab Fractional           \tab At least `1` non-`NA` value is not a
#'                                          whole number .                   \cr
#'   `'pct'` \tab Percent              \tab Percentage numeric (in the interval
#'                                          `[0, 100]}).                     \cr
#'   `'ppn'` \tab Proportion           \tab Proportion numeric (in the interval
#'                                          `[0, 1]}).                       \cr
#'   `'pos'` \tab Positive             \tab Positive numeric.                \cr
#'   `'nng'` \tab Non-negative         \tab Non-negative numeric.            \cr
#'   `'nps'` \tab Non-positive         \tab Non-positive numeric.            \cr
#'   `'neg'` \tab Negative             \tab Negative numeric.                \cr
#'   `'whl'` \tab Whole                \tab Whole number.                    \cr
#'   `'evn'` \tab Even                 \tab Even (whole) number.             \cr
#'   `'odd'` \tab Odd                  \tab Odd (whole) number.              \cr
#'   `'psw'` \tab Positive whole       \tab Positive whole-number.           \cr
#'   `'nnw'` \tab Non-negative whole   \tab Non-negative whole-number.       \cr
#'   `'npw'` \tab Non-positive whole   \tab Non-positive whole-number.       \cr
#'   `'ngw'` \tab Negative whole       \tab Negative whole-number.             }
#'   Functions in this family are:\tabular{ll}{
#'   FUNCTION        \tab WHAT IT DOES                                       \cr
#'   `mmm`           \tab Get a character vector containing all extended mode
#'                        properties possessed by `x`.                       \cr
#'   `ixxx`          \tab Evaluate whether `x` possesses the extended mode
#'                        property `xxx` (a placeholder for any given extended
#'                        mode property value), subject to any restrictions in
#'                        `...`.                                             \cr
#'   `mmmi`          \tab Evaluate whether `x` possesses one or more (possibly
#'                        pipe-delimited) extended mode properties in `spec`,
#'                        subject to any restrictions in `...`.              \cr
#'   `mmm_props`     \tab Get a character vector of all possible extended mode
#'                        property values.                                   \cr
#'   `is_mmm_spec`   \tab Evaluate whether `spec` is a valid extended mode
#'                        property specification.                              }
#' @param x An R object.
#' @param spec `NULL` or a \link[=cmp_chr_scl]{complete character vec}
#'   containing one or more extended mode properties from `mmm_props()`.
#'   Extended mode properties may be pipe-delimited. If there are multiple
#'   properties in `spec`, `x` is inspected for a match to any of the specified
#'   properties.
#' @inheritDotParams meets
#' @inheritSection meets Specifying Count and Value Restrictions
#' @return \tabular{ll}{
#'   FUNCTIONS                       \tab RETURN VALUE                       \cr
#'   `mmm_vals`                      \tab A character vector.                \cr
#'   `mmm`                           \tab A character scalar or vector.      \cr
#'   `mmmi`, `ixxx`, `is_mmm_spec`   \tab A logical scalar.                    }
#' @export
mmm <- function(x) {
  out <- NULL
  for (m in .mmms) {out <- c(out, f0(run('.i', m, '(x)'), m, NULL))}
  out
}

#' @rdname mmm
#' @export
mmm_props <- function() {.mmms}

#' @rdname mmm
#' @export
is_mmm_spec <- function(spec) {spec <- .spec_vals(spec); f0(length(spec) == 0, F, all(spec %in% .mmms))}

#' @rdname mmm
#' @export
immm <- function(x, spec, ...) {
  errs <- c(.meets_errs(x, ...), f0(is_mmm_spec(spec), NULL, '\n \u2022 [spec] must be a complete character vec (?cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from mmm_props().'))
  if (!is.null(errs)) {stop(errs)}
  if (!meets(x, ...)) {return(F)}
  for (prop in .spec_vals(spec)) {if (run('.i', prop, '(x)')) {return(T)}}
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
