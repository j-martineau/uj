#' @encoding UTF-8
#' @family properties
#' @title Basic properties
#' @description An object's basic properties are defined by its fundamental structure as follows:
#' \tabular{rll}{
#'       `'atm'` \tab   `atomic`      \tab Atomic (but not `NULL`). Both a basic and an \link[=mmm]{xmode} property.
#'   \cr `'rcr'` \tab   `recursive`   \tab data.frame or list.
#'   \cr `'pop'` \tab   `populated`   \tab Length `1` or greater.
#'   \cr `'def'` \tab   `defined`     \tab Not `NULL`.
#'   \cr `'nll'` \tab   `null`        \tab `NULL`.
#'   \cr `'nil'` \tab   `nil`         \tab Length `0` but not `NULL`.
#'   \cr `'fun'` \tab   `function`    \tab A function or a character scalar name of a function accessible from the environment of the calling function.
#' }
#' **Functions**
#' \tabular{rl}{
#'     `is_bbb_spec`   \tab Is `spec` a basic property specification?
#'   \cr `bbb_props`   \tab What basic properties are there?
#'   \cr  `bbb_funs`   \tab What basic properties functions are there?
#'   \cr      `ibbb`   \tab Is `x` a match to the basic property specification `spec`?
#'   \cr      `iBBB`   \tab Is `x` a match to the basic property `'BBB'`?
#'   \cr       `bbb`   \tab What are `x`'s basic properties?
#' }
#' @param x An R object.
#' @param spec `NULL` or a \link[=cmp_chr_scl]{complete character vec} containing one or more basic properties from `bbb_props()`. basic properties may be pipe-delimited. If there are multiple properties in `spec`, `x` is inspected for a match to any of the specified properties.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return *A character vector*
#'  \cr    `bbb_props`
#'  \cr    `bbb_funs`
#'  \cr    `bbb`
#'  \cr\cr *A logical scalar*
#'  \cr    `is_bbb_spec`
#'  \cr    `iBBB`
#'  \cr    `ibbb`
#' @examples
#' bbb_funs()
#' bbb_props()
#' is_bbb_spec("nil|nll")
#' ibbb(NULL, "nil|nll")
#' ipop(NA)
#' iatm(list(letters))
#' bbb(NA)
#' @export
bbb <- function(x) {
  out <- NULL
  for (b in .bbbs) {out <- c(out, f0(run('.i', b, '(x)'), b, NULL))}
  out
}

#' @rdname bbb
#' @export
bbb_funs <- function() {paste0("i", .bbbs)}

#' @rdname bbb
#' @export
bbb_props <- function() {.bbbs}

#' @rdname bbb
#' @export
is_bbb_spec <- function(spec) {
  spec <- .spec_vals(spec)
  f0(length(spec) == 0, F, all(spec %in% bbb_props()))
}

#' @rdname bbb
#' @export
ibbb <- function(x, spec, ...) {
  errs <- c(.meets_errs(x, ...), f0(is_bbb_spec(spec), NULL, '[spec] must be a complete character vec (?cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from bbb_props().'))
  if (!is.null(errs)) {stop(.errs(errs))}
  if (!meets(x, ...)) {return(F)}
  for (prop in .spec_vals(spec)) {if (run('.i', prop, '(x)')) {return(T)}}
  F
}

#' @rdname bbb
#' @export
iatm <- function(x, ...) {ibbb(x, 'atm', ...)}

#' @rdname bbb
#' @export
idef <- function(x, ...) {ibbb(x, 'def', ...)}

#' @rdname bbb
#' @export
ifun <- function(x, ...) {ibbb(x, 'fun', ...)}

#' @rdname bbb
#' @export
inil <- function(x) {ibbb(x, 'nil')}

#' @rdname bbb
#' @export
inll <- function(x) {ibbb(x, 'nll')}

#' @rdname bbb
#' @export
ipop <- function(x, ...) {ibbb(x, 'pop', ...)}

#' @rdname bbb
#' @export
ircr <- function(x, ...) {ibbb(x, 'rcr', ...)}
