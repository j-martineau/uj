.iatm <- function(x) {is.atomic(x) & !is.null(x)}
.idef <- function(x) {!is.null(x)}
.ifun <- function(x) {f0(is.function(x), T, f0(length(x) != 1 | !is.character(x), F, f0(is.na(x), F, !isERR(match.fun(x)))))}
.inil <- function(x) {length(x) == 0}
.inll <- function(x) {is.null(x)}
.ipop <- function(x) {length(x) > 0}
.ircr <- function(x) {is.recursive(x)}
.bbbs <- c("atm", "def", "fun", "nil", "nll", "pop", "rcr")

#' @family props
#' @title Base properties
#' @description An object's base properties are defined by its most basic structural properties as described in the following table:
#' \itemize{
#'   \item **`'atm'`** (atomic): atomic objects.
#'   \item **`'def'`** (defined): non-`NULL` objects.
#'   \item **`'fun'`** (function): function objects or character scalar names of functions accessible to the calling function.
#'   \item **`'nil'`** (nil): objects of nil length (i.e., `0`-length).
#'   \item **`'nll'`** (null): the `NULL` object.
#'   \item **`'pop'`** (populated): objects of length `1` or greater.
#'   \item **`'rcr'`** (recursive): data.frames and lists.
#' }
#' **Functions**
#' \itemize{
#'   \item **`bbb`**: gets a character vector containing all base properties possessed by `x`.
#'   \item **`ixxx`**: evaluates whether `x` possesses the base property `xxx` (a placeholder for any given base property value), subject to any restrictions in `...`.
#'   \item **`ibbb`**: evaluates whether `x` possesses one or more (possibly pipe-delimited) base properties in `spec`, subject to any restrictions in `...`.
#'   \item **`bbbs_props`**: gets a character vector of all possible base property values.
#'   \item **`is_bbb_spec`**: evaluates whether `spec` is a valid base property specification.
#' }
#' @param x An R object.
#' @param spec `NULL` or a \link[=cmp_chr_scl]{complete character vec} containing one or more base properties (i.e., from `bbb_props()`). Base properties may be pipe-delimited. If there are multiple properties in `spec`, `x` is inspected for a match to any of the specified properties.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return \itemize{
#'   \item **`bbb`**: a character scalar/vector.
#'   \item **`bbb_vals`**: a character vector.
#'   \item **`ibbb, ixxx, is_bbb_spec`**: a logical scalar.
#' }
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
