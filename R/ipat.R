#' @name ipat
#' @family strings
#' @title Simplified string indexing for fixed patterns
#' @section Functions in This Family:
#'   \strong{\code{ipat}}
#'   \cr Logically indexes character vector elements containing a pattern as
#'   \code{TRUE} or \code{FALSE}.
#'   \cr\cr
#'   \strong{\code{has_pat}}
#'   \cr Evaluate whether each element of \code{x} contains the string in
#'   \code{pat}.
#'   \cr\cr
#'   \strong{\code{wpat}}
#'   \cr Numerically indexes character vector elements containing a pattern as
#'   integer element numbers.
#'   \cr\cr
#'   \strong{\code{npat}}
#'   \cr Counts the number of times a pattern appears in each element of a
#'   character vector.
#'   \cr\cr
#'   \strong{\code{ppat}}
#'   \cr Identifies the position of \emph{the first} character of the first
#'   instance of a pattern in each element of a character vector.
#'   \cr\cr
#'   \strong{\code{ppats}}
#'   \cr Identifies the position of the first character of
#'   \emph{every} instance of a pattern in each element of a character vector.
#' @details Always \link[=a]{atomizes} \code{...} via \code{av(...)} to create a
#'   single character vector to search for a fixed pattern.
#' @param x \link[=chr_vec]{Character vector}.
#' @param pat A fixed (not regular expression) \link[=cmp_chr_scl]{complete
#'   character scalar} pattern to search for in the atomic vector resulting from
#'   collapsing all elements of every argument supplied via \code{...}.
#' @return \strong{\code{has_pat, ipat}}
#'   \cr A logical vector.
#'   \cr\cr
#'   \strong{\code{wpat, npat, ppat}}
#'   \cr A \link[=whl_vec]{whole-number vector}.
#'   \cr\cr
#'   \strong{\code{ppats}}
#'   \cr A \link[=whl_vls]{whole-number vlist}.
#' @export
#' @rdname ipat
#' @export
ipat <- function(x, pat) {
  errs <- c(f0(cmp_chr_vec(x)  , NULL, " \u2022 [x] must be a complete character vec (?cmp_chr_vec)."     ),
            f0(cmp_chr_scl(pat), NULL, " \u2022 [pat] must be a complete character scalar (?cmp_chr_vec)."))
  if (!is.null(errs)) {stop(errs)}
  grepl(pat, x, fixed = T)
}

#' @rdname ipat
#' @export
has_pat <- function(x, pat) {
  errs <- c(f0(cmp_chr_vec(x)  , NULL, " \u2022 [x] must be a complete character vec (?cmp_chr_vec)."     ),
            f0(cmp_chr_scl(pat), NULL, " \u2022 [pat] must be a complete character scalar (?cmp_chr_vec)."))
  if (!is.null(errs)) {stop(errs)}
  nchar(x) != nchar(gsub(x, pat, "", fixed = T))
}

#' @rdname ipat
#' @export
wpat <- function(x, pat) {
  errs <- c(f0(cmp_chr_vec(x)  , NULL, " \u2022 [x] must be a complete character vec (?cmp_chr_vec)."     ),
            f0(cmp_chr_scl(pat), NULL, " \u2022 [pat] must be a complete character scalar (?cmp_chr_vec)."))
  if (!is.null(errs)) {stop(errs)}
  grep(pat, x, fixed = T)
}

#' @rdname ipat
#' @export
npat <- function(x, pat) {
  errs <- c(f0(cmp_chr_vec(x)  , NULL, " \u2022 [x] must be a complete character vec (?cmp_chr_vec)."     ),
            f0(cmp_chr_scl(pat), NULL, " \u2022 [pat] must be a complete character scalar (?cmp_chr_vec)."))
  if (!is.null(errs)) {stop(errs)}
  lengths(gregexpr(pat, x, fixed = T))
}

#' @rdname ipat
#' @export
ppat <- function(x, pat) {
  errs <- c(f0(cmp_chr_vec(x)  , NULL, " \u2022 [x] must be a complete character vec (?cmp_chr_vec)."     ),
            f0(cmp_chr_scl(pat), NULL, " \u2022 [pat] must be a complete character scalar (?cmp_chr_vec)."))
  if (!is.null(errs)) {stop(errs)}
  out <- regexpr(pat, x, fixed = T)
  out[out == -1] <- NA
  if (all(is.na(out))) {out <- NA}
  return(out)
}

#' @rdname ipat
#' @export
ppats <- function(x, pat) {
  errs <- c(f0(cmp_chr_vec(x)  , NULL, " \u2022 [x] must be a complete character vec (?cmp_chr_vec)."     ),
            f0(cmp_chr_scl(pat), NULL, " \u2022 [pat] must be a complete character scalar (?cmp_chr_vec)."))
  if (!is.null(errs)) {stop(errs)}
  out <- gregexpr(pat, x, fixed = T)
  out[out == -1] <- NA
  out
}
