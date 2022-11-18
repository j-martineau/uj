#' @name ipat.
#' @family strings
#' @title Simplified string indexing for fixed patterns
#' @description \tabular{ll}{
#'   \code{has_pat}   \tab Evaluate whether each element of \code{x} contains
#'                         the string in \code{pat}.                         \cr
#'   \code{ipat}      \tab Logically indexes character vector elements
#'                         containing a pattern as \code{TRUE} or
#'                         \code{FALSE}.                                     \cr
#'   \code{wpat}      \tab Numerically indexes character vector elements
#'                         containing a pattern as integer element numbers.  \cr
#'   \code{npat}      \tab Counts the number of times a pattern appears in each
#'                         element of a character vector.                    \cr
#'   \code{ppat}      \tab Identifies the position of \emph{the first} character
#'                         of the first instance of a pattern in each element of
#'                         a character vector.                               \cr
#'   \code{ppats}     \tab Identifies the position of the first character of
#'                         \emph{every} instance of a pattern in each element of
#'                         a character vector.                                 }
#' @details Always \link[=a]{atomizes} \code{...} via \code{av(...)} to create a
#'   single character vector to search for a fixed pattern.
#' @param x \link[=chr_vec]{Character vector}.
#' @param pat A fixed (not regular expression) \link[=cmp_chr_scl]{complete
#'   character scalar} pattern to search for in the atomic vector resulting from
#'   collapsing all elements of every argument supplied via \code{...}.
#' @return \tabular{lll}{
#'   \code{has_pat}, \code{ipat}          \tab   
#'         \tab Logical vector.                                              \cr
#'   \code{wpat}, \code{npat}, \code{ppat}\tab  
#'         \tab \link[=whl_vec]{Whole-number vector}.                        \cr
#'   \code{ppats}                         \tab  
#'         \tab \link[=whl_vls]{Whole-number vlist}.                           }
#' @export
ipat. <- function() {help("ipat.", package = "uj")}

#' @rdname ipat.
#' @export
has_pat <- function(x, pat) {
  errs <- c(f0(cmp_chr_vec(x)  , NULL, " \u2022 [x] must be a complete character vec (?cmp_chr_vec)."     ),
            f0(cmp_chr_scl(pat), NULL, " \u2022 [pat] must be a complete character scalar (?cmp_chr_vec)."))
  if (idef(errs)) {stop(errs)}
  nchar(x) != nchar(gsub(x, pat, "", fixed = T))
}

#' @rdname ipat.
#' @export
ipat <- function(x, pat) {
  errs <- c(f0(cmp_chr_vec(x)  , NULL, " \u2022 [x] must be a complete character vec (?cmp_chr_vec)."     ),
            f0(cmp_chr_scl(pat), NULL, " \u2022 [pat] must be a complete character scalar (?cmp_chr_vec)."))
  if (idef(errs)) {stop(errs)}
  grepl(pat, x, fixed = T)
}

#' @rdname ipat.
#' @export
wpat <- function(x, pat) {
  errs <- c(f0(cmp_chr_vec(x)  , NULL, " \u2022 [x] must be a complete character vec (?cmp_chr_vec)."     ),
            f0(cmp_chr_scl(pat), NULL, " \u2022 [pat] must be a complete character scalar (?cmp_chr_vec)."))
  if (idef(errs)) {stop(errs)}
  grep(pat, x, fixed = T)
}

#' @rdname ipat.
#' @export
npat <- function(x, pat) {
  errs <- c(f0(cmp_chr_vec(x)  , NULL, " \u2022 [x] must be a complete character vec (?cmp_chr_vec)."     ),
            f0(cmp_chr_scl(pat), NULL, " \u2022 [pat] must be a complete character scalar (?cmp_chr_vec)."))
  if (idef(errs)) {stop(errs)}
  lengths(gregexpr(pat, x, fixed = T))
}

#' @rdname ipat.
#' @export
ppat <- function(x, pat) {
  errs <- c(f0(cmp_chr_vec(x)  , NULL, " \u2022 [x] must be a complete character vec (?cmp_chr_vec)."     ),
            f0(cmp_chr_scl(pat), NULL, " \u2022 [pat] must be a complete character scalar (?cmp_chr_vec)."))
  if (idef(errs)) {stop(errs)}
  out <- regexpr(pat, x, fixed = T)
  out[out == -1] <- NA
  if (all(is.na(out))) {out <- NA}
  return(out)
}

#' @rdname ipat.
#' @export
ppats <- function(x, pat) {
  errs <- c(f0(cmp_chr_vec(x)  , NULL, " \u2022 [x] must be a complete character vec (?cmp_chr_vec)."     ),
            f0(cmp_chr_scl(pat), NULL, " \u2022 [pat] must be a complete character scalar (?cmp_chr_vec)."))
  if (idef(errs)) {stop(errs)}
  out <- gregexpr(pat, x, fixed = T)
  out[out == -1] <- NA
  out
}
