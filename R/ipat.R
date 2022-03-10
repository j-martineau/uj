#' @name ipat
#' @family strings
#' @title Simplified string indexing for fixed patterns
#' @details \strong{\code{ipat}}
#'   \cr Logically indexes character vector elements containing a pattern as
#'   \code{TRUE} or \code{FALSE}.
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
#'   \cr Identifies the position of the first character of \emph{every} instance
#'   of a pattern in each element of a character vector.
#' @details Always atomizes \code{...} via \code{a(...)} to create a single
#'   character vector to search for a fixed pattern.
#' @param x A character vector.
#' @param pattern A fixed (not regular expression) character scalar pattern to
#'   search for in the atomic vector resulting from collapsing all elements of
#'   every argument supplied via \code{...}.
#' @return A logical vector (\code{ipat}), an empty or numeric vector
#'   (\code{wpat, npat, ppat}), or a list of numeric vectors (\code{ppats}).
#' @export
ipat <- function(x, pat) {
  VX <- cmp_chr_vec(x)
  VP <- cmp_chr_scl(pat)
  E <- NULL
  if (!VX) {E <- c(E, "\n  * [x] must be a complete character scalar/vector.")}
  if (!VP) {E <- c(E, "\n  * [pat] must be a complete character scalar.")}
  if (xdef(E)) {stop(E)}
  grepl(pat, x, fixed = T)                                                       # logically index each atomic element of [x] containing pat
}

#' @rdname ipat
#' @export
wpat <- function(x, pat) {
  VX <- cmp_chr_vec(x)
  VP <- cmp_chr_scl(pat)
  E <- NULL
  if (!VX) {E <- c(E, "\n  * [x] must be a complete character scalar/vector.")}
  if (!VP) {E <- c(E, "\n  * [pat] must be a complete character scalar.")}
  if (xdef(E)) {stop(E)}
  grep(pat, x, fixed = T)                                                        # numerically index each atomic element of [x] containing pat
}

#' @rdname ipat
#' @export
npat <- function(x, pat) {
  VX <- cmp_chr_vec(x)
  VP <- cmp_chr_scl(pat)
  E <- NULL
  if (!VX) {E <- c(E, "\n  * [x] must be a complete character scalar/vector.")}
  if (!VP) {E <- c(E, "\n  * [pat] must be a complete character scalar.")}
  if (xdef(E)) {stop(E)}
  lengths(gregexpr(pat, x, fixed = T))                                           # count the number of times each atomic element of [x] containing pat
}

#' @rdname ipat
#' @export
ppat <- function(x, pat) {
  VX <- cmp_chr_vec(x)
  VP <- cmp_chr_scl(pat)
  E <- NULL
  if (!VX) {E <- c(E, "\n  * [x] must be a complete character scalar/vector.")}
  if (!VP) {E <- c(E, "\n  * [pat] must be a complete character scalar.")}
  if (xdef(E)) {stop(E)}
  R <- regexpr(pat, x, fixed = T)                                                # get the character position of the start of [pat] in each element of [x]
  R[R == -1] <- NA                                                               # replace -1 with NA to represent no match
  if (all(is.na(R))) {R <- NA}
  return(R)
}

#' @rdname ipat
#' @export
ppats <- function(x, pat) {
  VX <- cmp_chr_vec(x)
  VP <- cmp_chr_scl(pat)
  E <- NULL
  if (!VX) {E <- c(E, "\n  * [x] must be a complete character scalar/vector.")}
  if (!VP) {E <- c(E, "\n  * [pat] must be a complete character scalar.")}
  if (xdef(E)) {stop(E)}
  R <- gregexpr(pat, x, fixed = T)                                               # get the character position of every instance of [pat] in each element of [x]
  R[R == -1] <- NA                                                               # replace -1 with NA to represent no match
  R
}
