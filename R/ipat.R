#' @name ipat
#' @family strings
#' @title Simplified string indexing for fixed patterns
#' @description \tabular{ll}{
#'   FUNCTION    \tab WHAAT IT DOES                                          \cr
#'   `ipat`      \tab Logically indexes character vector elements containing a
#'                    pattern as `TRUE` or `FALSE`.                          \cr
#'   `has_pat`   \tab Evaluate whether each element of `x` contains the string
#'                    in `pat`.                                              \cr
#'   `wpat`      \tab Numerically indexes character vector elements containing
#'                    a pattern as integer element numbers.                  \cr
#'   `npat`      \tab Counts the number of times a pattern appears in each
#'                    element of a character vector.                         \cr
#'   `ppat`      \tab Identifies the position of *the first* character of the
#'                    first instance of a pattern in each element of a character
#'                    vector.                                                \cr
#'   `ppats`     \tab Identifies the position of the first character of *every*
#'                    instance of a pattern in each element of a characte
#'                    vector.                                                  }
#' @details Always \link[=a]{atomizes} `...` to create a single character vector
#'   to search for a fixed pattern.
#' @param x A \link[=chr_vec]{character vector}.
#' @param pat A fixed value \link[=cmp_chr_scl]{complete character scalar}
#'   pattern to search for in the atomic vector resulting from collapsing all
#'   elements of every argument supplied via `...`.
#' @return \tabular{ll}{
#'   FUNCTIONS                \tab RETURN VALUE                              \cr
#'   `has_pat`, `ipat`        \tab A logical vector.                         \cr
#'   `wpat`, `npat`, `ppat`   \tab A \link[=whl_vec]{whole-number vector}.   \cr
#'   `ppats`                  \tab A \link[=whl_vls]{whole-number vlist}.      }
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
