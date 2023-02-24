#' @encoding UTF-8
#' @family properties
#' @title Basic properties
#' @description An object's basic properties are defined by its fundamental structure as follows:
#' \tabular{lll}{  `'atm', 'ATM'`   \tab `atomic`      \tab Atomic (but not `NULL`). Both a basic and an \link[=mmm]{xmode} property.                                    \cr   \tab   \cr
#'                 `'fun', 'FUN'`   \tab `function`    \tab A function or a character scalar name of a function accessible from the environment of the calling function. \cr   \tab   \cr
#'                 `'rcr', 'RCR'`   \tab `recursive`   \tab data.frame or list.                                                                                                       \cr
#'                 `'pop', 'POP'`   \tab `populated`   \tab Length `1` or greater.                                                                                                    \cr
#'                 `'def', 'DEF'`   \tab `defined`     \tab Not `NULL`.                                                                                                               \cr
#'                 `'nll', 'NLL'`   \tab `null`        \tab `NULL`.                                                                                                                   \cr
#'                 `'nil', 'NIL'`   \tab `nil`         \tab Length `0` but not `NULL`.                                                                                                  }
#' @details
#' \tabular{ll}{  `is_bbb_spec`   \tab Is `spec` a basic property specification?                                                                   \cr   \tab   \cr
#'                `bbb_props`     \tab What basic properties are there?                                                                            \cr   \tab   \cr
#'                `bbb_funs`      \tab What basic properties functions are there?                                                                  \cr   \tab   \cr
#'                `{BBB}`         \tab Is `x` a match to the basic property `'{BBB}'` where `{BBB}` is a placeholder for any given basic property? \cr   \tab   \cr
#'                `BBB`           \tab Is `x` a match to the basic property specification `spec`?                                                  \cr   \tab   \cr
#'                `bbb`           \tab What are `x`'s basic properties?                                                                                           }
#' @param x An R object.
#' @param spec `NULL` or a \link[=cmp_chr_scl]{complete character vec} containing one or more basic properties from `bbb_props()`. basic properties may be pipe-delimited. If there are multiple properties in `spec`, `x` is inspected for a match to any of the specified properties.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return **A character vector** \cr\cr `bbb_props, bbb_funs, bbb`
#' \cr\cr  **A logical scalar**   \cr\cr `is_bbb_spec, {BBB}, BBB`
#' @examples
#' bbb_funs()
#' bbb_props()
#' is_bbb_spec("nil|nll")
#' BBB(NULL, "nil|nll")
#' POP(NA)
#' ATM(list(letters))
#' bbb(NA)
#' @export
bbb <- function(x) {
  y <- NULL
  for (bbb in uj:::.BBB) {y <- base::c(y, uj::f0(uj::run('uj::', bbb, '(x)'), bbb, NULL))}
  y
}

#' @rdname bbb
#' @export
bbb_funs <- function() {uj:::.BBB}

#' @rdname bbb
#' @export
bbb_props <- function() {uj:::.bbb}

#' @rdname bbb
#' @export
is_bbb_spec <- function(spec) {
  spec <- uj:::.spec2props(spec)
  uj::f0(uj::N0(spec), F, uj::allIN(spec, uj::bbb_props()))
}

#' @rdname bbb
#' @export
BBB <- function(x, spec, ...) {
  uj::errs_if_pop(base::c(uj:::.meets_errs(x, ...), uj::f0(uj::is_bbb_spec(spec), NULL, '[spec] must be a complete character vec (?cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from bbb_props().')), PKG = "uj")
  if (uj::meets(x, ...)) {for (prop in base::toupper(uj:::.spec2props(spec))) {if (uj::run('uj::', prop, '(x)')) {return(T)}}}
  F
}

#' @rdname bbb
#' @export
ATM <- function(x, ...) {uj::BBB(x, 'atm', ...)}

#' @rdname bbb
#' @export
DEF <- function(x, ...) {uj::BBB(x, 'def', ...)}

#' @rdname bbb
#' @export
FUN <- function(x, ...) {uj::BBB(x, 'fun', ...)}

#' @rdname bbb
#' @export
NIL <- function(x) {uj::BBB(x, 'nil')}

#' @rdname bbb
#' @export
NLL <- function(x) {uj::BBB(x, 'nll')}

#' @rdname bbb
#' @export
POP <- function(x, ...) {uj::BBB(x, 'pop', ...)}

#' @rdname bbb
#' @export
RCR <- function(x, ...) {uj::BBB(x, 'rcr', ...)}
