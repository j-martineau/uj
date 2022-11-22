#' @name iii
#' @family props
#' @title Integrity (state of completeness) Properties
#' @description NOTE: \code{III} is used as a wildcard representing any given
#'   integrity property.
#'   \cr\cr
#'   Integrity are defined for \link[=ipop]{populated} \link[=atm_dtf]{atomic
#'   data.frame}, populated \link[=atm_vls]{atomic vlists}, populated atomic
#'   vectors, and populated atomic arrays. For all others, all integrity
#'   properties are considered \code{FALSE}. The following table summarizes
#'   valid state properties.
#'   \tabular{lll}{
#'     PROPERTY    \tab PROPERTY   \tab QUALIFYING                           \cr
#'     NAME        \tab VALUE      \tab OBJECTS                              \cr
#'     Complete    \tab\code{'cmp'}\tab Populated and atomic vectors, arrays,
#'                                      \link[=atm_dtf]{data.frames}, and
#'                                      \link[=atm_vls]{vlists} containing
#'                                      \strong{no} \code{NA} values.        \cr
#'     Missing     \tab\code{'mss'}\tab Populated and atomic vectors, arrays,
#'                                      \link[=atm_dtf]{data.frames}, and
#'                                      \link[=atm_vls]{vlists} containing
#'                                      \strong{only} \code{NA} values.      \cr
#'     Partial     \tab\code{'prt'}\tab Populated and atomic vectors, arrays,
#'                                      \link[=atm_dtf]{data.frames}, and
#'                                      \link[=atm_vls]{vlists} containing
#'                                      \strong{both} \code{NA} and
#'                                      non-\code{NA} values.                \cr
#'     NA scalar   \tab\code{'nas'}\tab An atomic scalar of value \code{NA}.
#'                                      May be a vector of array.            \cr
#'     OK scalar   \tab\code{'oks'}\tab An atomic scalar of value not equal to
#'                                      \code{NA}. May be a vector of array.   }
#' @section Functions in this Family:
#'   \strong{\code{iIII}}
#'   \cr Evaluates whether \code{x} matches the integrity property \code{III}
#'   (subject to any restrictions in \code{...}).
#'   \cr\cr
#'   \strong{\code{iii}}
#'   \cr Gets a character vector containing all integrity properties matching
#'   \code{x}.
#'   \cr\cr
#'   \strong{\code{iiii}}
#'   \cr Evaluates \code{x} against the integrity property specification in
#'   \code{spec} (subject to any restrictions in \code{...}).
#'   \cr\cr
#'   \strong{\code{iii_props}}
#'   \cr Gets a character vector of all possible integrity property values.
#'   \cr\cr
#'   \strong{\code{is_iii_spec}}
#'   \cr Evaluates whether \code{spec} is a valid integrity property
#'   specification.
#' @param x An R object.
#' @param spec \code{NULL} or a \link[=cmp_chr_scl]{complete character vec}
#'   containing one or more integrity properties (i.e., from
#'   \code{iii_vals()}). \strong{NOTE}: properties may be pipe-separated. If
#'   If there are multiple properties in \code{spec}, \code{x} is inspected for
#'   a match to any of the specified properties.
#' @inheritDotParams meets
#' @inheritSection meets Specifying Count and Value Restrictions
#' @return \strong{\code{iii_vals}}
#'   \cr A character vector.
#'   \cr\cr
#'   \strong{\code{iii}}
#'   \cr A character scalar or character vector.
#'   \cr\cr
#'   \strong{\code{iIII, iiii, is_iii_spec}}
#'   \cr A logical scalar.
#' @export
iii <- function(x) {
  props <- .iii_props()
  out <- NULL
  for (prop in props) {out <- c(out, f0(run('.is_', prop, '(x)'), prop, NULL))}
  out
}

#' @rdname iii
#' @export
iii_props <- function() {.iii_props()}

#' @rdname iii
#' @export
is_iii_spec <- function(spec) {spec <- .spec_vals(); f0(length(spec) == 0, F, all(spec %in% .iii_props()))}

#' @rdname iii
#' @export
iiii <- function(x, spec, ...) {
  errs <- c(.meets_errs(x, ...), f0(is_iii_spec(spec), NULL, '\n \u2022 [spec] must be a complete character vec (?cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from iii_props().'))
  if (!is.null(errs)) {stop(errs)}
  if (!meets(x, ...)) {return(F)}
  for (prop in .spec_vals(spec)) {if (run('.is_', prop, '(x)')) {return(T)}}
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
