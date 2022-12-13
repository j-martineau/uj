#' @family props
#' @title Base properties
#' @description **Base property values**
#' An object's base properties are defined by its most basic structural properties as described in the following table:
#' \tabular{rll}{
#'       `'atm'`   \tab Atomic      \tab Atomic objects.
#'   \cr `'def'`   \tab Defined     \tab Non-`NULL` objects.
#'   \cr `'nil'`   \tab Nil         \tab `0`-length, non-`NULL` objects..
#'   \cr `'nll'`   \tab Null        \tab The `NULL` object.
#'   \cr `'pop'`   \tab Populated   \tab Length-`1+` objects.
#'   \cr `'rcr'`   \tab Recursive   \tab Data.frames and lists.
#'   \cr           \tab             \tab   
#'   \cr `'fun'`   \tab Function    \tab Function objects or character scalar names of functions accessible to the calling function.
#' }
#' **Base property functions**
#' \tabular{rl}{
#'     `is_bbb_spec`   \tab Is `spec` is a base property spec?
#'   \cr `bbb_props`   \tab Gets all base property values.
#'   \cr      `ibbb`   \tab Does `x` match base property spec `spec`?
#'   \cr      `iBBB`   \tab Does `x` match property `BBB`?
#'   \cr       `bbb`   \tab Gets all of `x`'s base properties.
#' }
#' @param x An R object.
#' @param spec `NULL` or a \link[=cmp_chr_scl]{complete character vec} containing one or more base properties (i.e., from `bbb_props()`). Base properties may be pipe-delimited. If there are multiple properties in `spec`, `x` is inspected for a match to any of the specified properties.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return \tabular{rl}{
#'     `is_bbb_spec`   \tab A logical
#'   \cr      `ibbb`   \tab scalar.
#'   \cr      `iBBB`   \tab   
#'   \cr               \tab   
#'   \cr `bbb_props`   \tab A character
#'   \cr       `bbb`   \tab vector.
#' }
#' \eqn{^{2.}} `xxx` represents a base property.
#' @examples
#' is_bbb_spec("nil|nll")
#' bbb_props()
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
bbb_props <- function() {.bbbs}

#' @rdname bbb
#' @export
is_bbb_spec <- function(spec) {spec <- .spec_vals(spec); f0(length(spec) == 0, F, all(spec %in% .bbbs))}

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
inil <- function(x, ...) {ibbb(x, 'nill', ...)}

#' @rdname bbb
#' @export
inll <- function(x, ...) {ibbb(x, 'nll', ...)}

#' @rdname bbb
#' @export
ipop <- function(x, ...) {ibbb(x, 'pop', ...)}

#' @rdname bbb
#' @export
ircr <- function(x, ...) {ibbb(x, 'rcr', ...)}
