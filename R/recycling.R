#' @name recycling
#' @family extensions
#' @family environments
#' @title Recycling and recyclability
#' @description \tabular{ll}{
#'   \code{recyclable_n}   \tab Checks whether the vector of lengths in
#'                              \code{n.} represents recyclable arguments
#'                              subject to the setting in \code{targ.}.      \cr
#'   \code{recyclable}     \tab Evaluates whether arguments in \code{...} are
#'                              recyclable subject to settings in the arguments
#'                              \code{n.}, \code{min.}, \code{max.}, and
#'                              \code{targ.}.                                \cr
#'   \code{recycle}        \tab Recycle arguments in \code{...} in the
#'                              environment of the calling function subject to
#'                              settings in the arguments \code{n.},
#'                              \code{min.}, \code{max.}, and \code{targ.}.    }
#' @param ... Named arguments to be recycled in the environment of the calling
#'   function.
#' @param lengths. \link[=cmp_psw_vec]{Complete positive whole-number vec} of
#'   lengths to check for recyclability.
#' @param targ. \link[=cmp_psw_scl]{Complete positive whole-number scalar}
#'   giving the target length of recycled arguments.
#' @param n. For \code{recyclable_n}, a \link[=cmp_psw_vec]{complete positive
#'   whole-number vec} giving the lengths of arguments to be recycled; for
#'   \code{recyclable} and \code{recycle}, either \code{NULL} or a
#'   \link[=cmp_psw_vec]{complete positive whole-number vec} giving the set of
#'   valid recycled argument lengths.
#' @param min. \code{NULL} or \link[=cmp_psw_scl]{complete positive whole-number
#'   scalar} giving the minimum valid recycled argument length.
#' @param max. \code{NULL} or \link[=cmp_psw_scl]{complete positive whole-number
#'   scalar} giving the maximum valid recycled argument length.
#' @param err. \link[=cmp_lgl_scl]{Complete logical scalar} indicating whether
#'   to throw an error if the arguments in \code{...} are not recyclable.
#' @return \tabular{ll}{
#'   \code{recyclable_n}   \tab Logical scalar                               \cr
#'   \code{recyclable}     \tab Logical scalar                               \cr
#'   \code{recycle}        \tab \code{NULL} (called for side effect).          }
#' @export
recyclable_n <- function(n, targ = max(n)) {
  errs <- c(f0(cmp_psw_vec(n)   , NULL, "\n \u2022 [n] must be a complete positive whole-number vec (?cmp_psw_vec)."),
            f0(cmp_psw_scl(targ), NULL, "\n \u2022 [targ] must be a complete positive whole-number scalar (?cmp_psw_scl)."))
  if (idef(errs)) {stop(errs)}
  out <- targ / n
  all(out == round(out))
}

#' @rdname recycling
#' @export
recyclable <- function(..., n. = NULL, min. = 1, max. = NULL, targ. = NULL, err. = F) {
  errs <- c(f0(all(sapply(list(...), ivec))    , NULL, "\n \u2022 Arguments in [...] must be atomic vecs (?ivec)."),
            f0(...length() > 0                 , NULL, "\n \u2022 [...] is empty."),
            f0(inll(n.) | cmp_psw_vec(n.)      , NULL, "\n \u2022 [n.] must be NULL or a complete positive whole-number vec (?cmp_psw_vec)."),
            f0(cmp_psw_scl(min.)               , NULL, "\n \u2022 [min.] must be a complete positive whole-number scalar (?cmp_psw_scl)."),
            f0(inll(max.) | cmp_psw_scl(max.)  , NULL, "\n \u2022 [max.] must be NULL or a complete positive whole-number scalar (?cmp_psw_scl)."),
            f0(inll(targ.) | cmp_psw_scl(targ.), NULL, "\n \u2022 [targ.] must be NULL or a complete positive whole-number scalar (?cmp_psw_scl)."),
            f0(isTF(err.)                      , NULL, "\n \u2022 [err.] must be TRUE or FALSE."))
  if (idef(errs)) {stop(errs)}
  dots <- list(...)
  ns <- lengths(dots)
  errs <- c(f0(!err. | f0(inll(n.  ), T, all(ns %in% n.)), NULL, "\n \u2022 Arguments in [...] must have length in [n.]."),
            f0(!err. | f0(inll(min.), T, all(ns >= min.)), NULL, "\n \u2022 Arguments in [...] must have length ≥ [min.]."),
            f0(!err. | f0(inll(max.), T, all(ns <= max.)), NULL, "\n \u2022 Arguments in [...] must have length ≤ [max.]."))
  if (idef(errs)) {stop(errs)}
  if (inll(targ.)) {targ. <- max(n.)}
  out <- recyclable_n(n., targ.)
  if (err. & !out) {stop("\n \u2022 Arguments in [...] are not recyclable.")}
  out
}

#' @rdname recycling
#' @export
recycle <- function(..., n. = NULL, min. = 1, max. = NULL, targ. = NULL) {
  recyclable(..., n. = n., min. = min., max. = max., targ. = targ., err. = T)
  x <- list(...)
  out <- max(lengths(x) / lengths(x))
  labs <- ...names()
  for (i in 1:length(x)) {if (out[i] > 1) {vset(labs[i], rep(...elt(i), out[i]))}}
  NULL
}
