#' @family props
#' @title Integrity properties
#' @description Integrity properties are defined for \link[=ipop]{populated} \link[=atm_dtf]{atomic data.frame}, populated \link[=atm_vls]{atomic vlists}, populated atomic vectors, and populated atomic arrays. For all others, all integrity properties are considered \code{FALSE}. The following table summarizes valid integrity properties.
#' \cr\cr **Integrity properties** \tabular{rll}{
#'       `'cmp'`   \tab Complete    \tab Populated and atomic vectors, arrays, \link[=atm_dtf]{data.frames}, and \link[=atm_vls]{vlists} containing **no** `NA` values.
#'   \cr           \tab             \tab  
#'   \cr `'mss'`   \tab Missing     \tab Populated and atomic vectors, arrays, \link[=atm_dtf]{data.frames}, and \link[=atm_vls]{vlists} containing **only** `NA` values.
#'   \cr           \tab             \tab  
#'   \cr `'prt'`   \tab Partial     \tab Populated and atomic vectors, arrays, \link[=atm_dtf]{data.frames}, and \link[=atm_vls]{vlists} containing **both** `NA` and non-`NA` values.
#'   \cr           \tab             \tab  
#'   \cr `'nas'`   \tab NA scalar   \tab Atomic scalar `NA`.
#'   \cr           \tab             \tab  
#'   \cr `'oks'`   \tab OK scalar   \tab Non-`NA` atomic scalar.
#' }
#' **Integrity property functions** \tabular{rl}{
#'     `is_iii_spec`   \tab Evaluates whether `spec` is a valid integrity property specification.
#'   \cr               \tab  
#'   \cr `iii_props`   \tab Gets a character vector of all possible integrity property values.
#'   \cr               \tab  
#'   \cr      `iiii`   \tab Evaluates whether `x` possesses one or more (possibly pipe-delimited) integrity properties in `spec` (subject to any restrictions in `...`).
#'   \cr               \tab  
#'   \cr      `ixxx`   \tab Evaluates whether `x`matdches property `xxx`\eqn{^2} (subject to any restrictions in `...`).
#'   \cr               \tab  
#'   \cr       `iii`   \tab Gets a character vector containing all integrity properties possessed by `x`.
#' }
#' \eqn{^{1.}} An integrity property.
#' @param x An R object.
#' @param spec `NULL` or a \link[=cmp_chr_scl]{complete character vec} containing one or more integrity properties (i.e., from `iii_props()`). Integrity properties may be pipe-delimited. If there are multiple properties in `spec`, `x` is inspected for a match to any of the specified properties.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return \tabular{rl}{
#'     `is_iii_spec`   \tab A logical scalar.
#'   \cr `iii_props`   \tab A character vector.
#'   \cr      `iiii`   \tab A logical scalar.
#'   \cr      `ixxx`   \tab A logical scalar.
#'   \cr       `iii`   \tab A character scalar or vector.
#' }
#' @examples
#' is_iii_spec("nas|mss")
#' iii_props()
#' icmp(NA)
#' icmp(NULL)
#' inas(NA)
#' ioks(letters)
#' ioks(letters[1])
#' imss(rep(NA, 10))
#' iii(NA)
#' iii(letters)
#' iii(letters[1])
#' @export
iii <- function(x) {
  out <- NULL
  for (i in .iiis) {out <- c(out, f0(run('.i', i, '(x)'), i, NULL))}
  out
}

#' @rdname iii
#' @export
iii_props <- function() {.iiis}

#' @rdname iii
#' @export
is_iii_spec <- function(spec) {spec <- .spec_vals(spec); f0(length(spec) == 0, F, all(spec %in% .iiis))}

#' @rdname iii
#' @export
iiii <- function(x, spec, ...) {
  errs <- c(.meets_errs(x, ...), f0(is_iii_spec(spec), NULL, '\n \u2022 [spec] must be a complete character vec (?cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from iii_props().'))
  if (!is.null(errs)) {stop(errs)}
  if (!meets(x, ...)) {return(F)}
  for (prop in .spec_vals(spec)) {if (run('.i', prop, '(x)')) {return(T)}}
  F
}

#' @rdname iii
#' @export
icmp <- function(x, ...) {iiii(x, 'cmp', ...)}

#' @rdname iii
#' @export
imss <- function(x, ...) {iiii(x, 'mss', ...)}

#' @rdname iii
#' @export
inas <- function(x, ...) {iiii(x, 'nas', ...)}

#' @rdname iii
#' @export
ioks <- function(x, ...) {iiii(x, 'oks', ...)}

#' @rdname iii
#' @export
iprt <- function(x, ...) {iiii(x, 'prt', ...)}
