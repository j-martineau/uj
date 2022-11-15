#' @name ipat.
#' @family strings
#' @title Simplified string indexing for fixed patterns
#' @details Always \link[a]{atomizes} \code{...} via \code{av(...)} to create a
#'   single character vector to search for a fixed pattern.
#' @param x \link[chr_vec]{Character vector}.
#' @param pat A fixed (not regular expression) \link[cmp_chr_scl]{complete
#'   character scalar} pattern to search for in the atomic vector resulting from
#'   collapsing all elements of every argument supplied via \code{...}.
#' @return A logical vector (\code{ipat}), an empty or numeric vector
#'   (\code{wpat, npat, ppat}), or a list of numeric vectors (\code{ppats}).
#' @export
ipat. <- function() {help("ipat.", package = "uj")}

#' @describeIn ipat. Evaluate whether each element of \code{x} contains the
#'   string in \code{pat}.
#' @export
has_pat <- function(x, pat) {
  errs <- c(f0(cmp_chr_vec(x)  , NULL, " \u2022 [x] must be a complete character vec (?cmp_chr_vec)."     ),
            f0(cmp_chr_scl(pat), NULL, " \u2022 [pat] must be a complete character scalar (?cmp_chr_vec)."))
  if (idef(errs)) {stop(errs)}
  nchar(x) != nchar(gsub(x, pat, "", fixed = T))
}

#' @describeIn ipat. Logically indexes character vector elements containing a
#'   pattern as \code{TRUE} or \code{FALSE}.
#' @export
ipat <- function(x, pat) {
  errs <- c(f0(cmp_chr_vec(x)  , NULL, " \u2022 [x] must be a complete character vec (?cmp_chr_vec)."     ),
            f0(cmp_chr_scl(pat), NULL, " \u2022 [pat] must be a complete character scalar (?cmp_chr_vec)."))
  if (idef(errs)) {stop(errs)}
  grepl(pat, x, fixed = T)
}

#' @describeIn ipat. Numerically indexes character vector elements containing
#'   a pattern as integer element numbers.
#' @export
wpat <- function(x, pat) {
  errs <- c(f0(cmp_chr_vec(x)  , NULL, " \u2022 [x] must be a complete character vec (?cmp_chr_vec)."     ),
            f0(cmp_chr_scl(pat), NULL, " \u2022 [pat] must be a complete character scalar (?cmp_chr_vec)."))
  if (idef(errs)) {stop(errs)}
  grep(pat, x, fixed = T)
}

#' @describeIn ipat. Counts the number of times a pattern appears in each
#'   element of a character vector.
#' @export
npat <- function(x, pat) {
  errs <- c(f0(cmp_chr_vec(x)  , NULL, " \u2022 [x] must be a complete character vec (?cmp_chr_vec)."     ),
            f0(cmp_chr_scl(pat), NULL, " \u2022 [pat] must be a complete character scalar (?cmp_chr_vec)."))
  if (idef(errs)) {stop(errs)}
  lengths(gregexpr(pat, x, fixed = T))
}

#' @describeIn ipat. Identifies the position of \emph{the first} character of
#'   the first instance of a pattern in each element of a character vector.
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

#' @describeIn ipat. Identifies the position of the first character of
#'   \emph{every} instance of a pattern in each element of a character vector.
#' @export
ppats <- function(x, pat) {
  errs <- c(f0(cmp_chr_vec(x)  , NULL, " \u2022 [x] must be a complete character vec (?cmp_chr_vec)."     ),
            f0(cmp_chr_scl(pat), NULL, " \u2022 [pat] must be a complete character scalar (?cmp_chr_vec)."))
  if (idef(errs)) {stop(errs)}
  out <- gregexpr(pat, x, fixed = T)
  out[out == -1] <- NA
  out
}
