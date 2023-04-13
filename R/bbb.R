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
#' \tabular{ll}{  `is_bbb_spec`   \tab Is `Spec` a basic property specification?                                                                   \cr   \tab   \cr
#'                `bbb_props`     \tab What basic properties are there?                                                                            \cr   \tab   \cr
#'                `bbb_funs`      \tab What basic properties functions are there?                                                                  \cr   \tab   \cr
#'                `{BBB}`         \tab Is `X` a match to the basic property `'{BBB}'` where `{BBB}` is a placeholder for any given basic property? \cr   \tab   \cr
#'                `BBB`           \tab Is `X` a match to the basic property specification `Spec`?                                                  \cr   \tab   \cr
#'                `bbb`           \tab What are `X`'s basic properties?                                                                                           }
#' @param X An R object.
#' @param Spec `NULL` or a \link[=cmp_chr_scl]{complete character vec} containing one or more basic properties from `bbb_props()`. basic properties may be pipe-delimited. If there are multiple properties in `Spec`, `X` is inspected for a match to any of the specified properties.
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
bbb <- function(X) {
  Y <- NULL
  for (BBB in uj::v(BBB)) {
    Match <- uj::run("uj:::.", BBB, "(X)")
    if (Match) {Y <- base::c(Y, BBB)}
  }
  Y
}

#' @rdname bbb
#' @export
bbb_funs <- function() {uj::v(BBB)}

#' @rdname bbb
#' @export
bbb_props <- function() {uj::v(bbb)}

#' @rdname bbb
#' @export
is_bbb_spec <- function(Spec) {
  Spec <- uj:::.spec2props(Spec)
  if (base::length(Spec) == 0) {F} else {base::all(Spec %in% uj::v(bbb))}
}

#' @rdname bbb
#' @export
BBB <- function(X, Spec, ...) {
  Errors <- uj:::.meets_errs(X, ...)
  if (!is_bbb_spec(Spec)) {Errors <- base::c(Errors, '[Spec] must be a complete character vec (?cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from bbb_props().')}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  if (uj::meets(X, ...)) {
    Props <- base::toupper(uj:::.spec2props(Spec))
    for (Prop in Props) {
      if (uj::run('uj:::.', Prop, '(X)')) {return(T)}
    }
  }
  F
}

#' @rdname bbb
#' @export
ATM <- function(X, ...) {uj::BBB(X, 'atm', ...)}

#' @rdname bbb
#' @export
DEF <- function(X, ...) {uj::BBB(X, 'def', ...)}

#' @rdname bbb
#' @export
FUN <- function(X, ...) {uj::BBB(X, 'fun', ...)}

#' @rdname bbb
#' @export
NIL <- function(X) {uj::BBB(X, 'nil')}

#' @rdname bbb
#' @export
NLL <- function(X) {uj::BBB(X, 'nll')}

#' @rdname bbb
#' @export
POP <- function(X, ...) {uj::BBB(X, 'pop', ...)}

#' @rdname bbb
#' @export
RCR <- function(X, ...) {uj::BBB(X, 'rcr', ...)}
