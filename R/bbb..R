#' @family props
#' @title Base properties
#' @description **Base property values**
#' An object's base properties are defined by its most basic structural properties as described in the following table:
#' \tabular{rll}{
#'       `'atm'`   \tab Atomic      \tab Atomic objects.
#'   \cr           \tab             \tab   
#'   \cr `'def'`   \tab Defined     \tab Non-`NULL` objects.
#'   \cr           \tab             \tab   
#'   \cr `'fun'`   \tab Function    \tab Function objects or character scalar names of functions accessible to the calling function.
#'   \cr           \tab             \tab   
#'   \cr `'nil'`   \tab Nil         \tab Objects of nil length (i.e., `0`-length).
#'   \cr           \tab             \tab   
#'   \cr `'nll'`   \tab Null        \tab The `NULL` object.
#'   \cr           \tab             \tab   
#'   \cr `'pop'`   \tab Populated   \tab Objects of length `1` or greater.
#'   \cr           \tab             \tab   
#'   \cr `'rcr'`   \tab Recursive   \tab Data.frames and lists.
#' }
#' **Base property functions**
#' \tabular{rl}{
#'     `is_bbb_spec`   \tab Evaluates whether `spec` is a valid base property specification.
#'   \cr               \tab   
#'   \cr `bbb_props`   \tab Gets a character vector of all possible base property values.
#'   \cr               \tab   
#'   \cr      `ibbb`   \tab Evaluates whether `x` possesses one or more (possibly pipe-delimited) base properties in `spec` (subject to any restrictions in `...`).
#'   \cr               \tab   
#'   \cr      `ixxx`   \tab Evaluates whether `x` matches property `xxx`\eqn{^1} (subject to any restrictions in `...`).
#'   \cr               \tab   
#'   \cr       `bbb`   \tab Gets a character vector containing all base properties possessed by `x`.
#' }
#' \eqn{^{1.}} A base property.
#' @param x An R object.
#' @param spec `NULL` or a \link[=cmp_chr_scl]{complete character vec} containing one or more base properties (i.e., from `bbb_props()`). Base properties may be pipe-delimited. If there are multiple properties in `spec`, `x` is inspected for a match to any of the specified properties.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return \tabular{rl}{
#'     `is_bbb_spec`   \tab A logical scalar.
#'   \cr `bbb_props`   \tab A character vector.
#'   \cr      `ibbb`   \tab A logical scalar.
#'   \cr      `ixxx`   \tab A logical scalar\eqn{^2}.
#'   \cr       `bbb`   \tab A character vector.
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
  errs <- c(.meets_errs(x, ...), f0(is_bbb_spec(spec), NULL, '\n \u2022 [spec] must be a complete character vec (?cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from bbb_props().'))
  if (!is.null(errs)) {stop(errs)}
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
