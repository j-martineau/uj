#' @name ipat.
#' @family strings
#' @title Simplified string indexing for fixed patterns
#' @details Always atomizes \code{...} via \code{av(...)} to create a single
#'   character vector to search for a fixed pattern.
#' @param x A character vector.
#' @param pat A fixed (not regular expression) character scalar pattern to
#'   search for in the atomic vector resulting from collapsing all elements of
#'   every argument supplied via \code{...}.
#' @return A logical vector (\code{ipat}), an empty or numeric vector
#'   (\code{wpat, npat, ppat}), or a list of numeric vectors (\code{ppats}).
#' @export
ipat. <- function() {help("ipat.", package = "uj")}

#' @describeIn ipat. Logically indexes character vector elements containing a
#'   pattern as \code{TRUE} or \code{FALSE}.
#' @export
ipat <- function(x, pat) {
  vx <- cmp_chr_vec(x)
  vp <- cmp_chr_scl(pat)
  err <- NULL
  if (!vx) {err <- c(err, "\n • [x] must be a complete character scalar/vec (?cmp_chr_scl, ?cmp_chr_vec).")}
  if (!vp) {err <- c(err, "\n • [pat] must be a complete character scalar (?cmp_chr_scl).")}
  if (idef(err)) {stop(err)}
  grepl(pat, x, fixed = T)                                                       # logically index each atomic element of [x] containing pat
}

#' @describeIn ipat. Numerically indexes character vector elements containing
#'   a pattern as integer element numbers.
#' @export
wpat <- function(x, pat) {
  vx <- cmp_chr_vec(x)
  vp <- cmp_chr_scl(pat)
  err <- NULL
  if (!vx) {err <- c(err, "\n • [x] must be a complete character scalar/vec (?cmp_chr_scl, ?cmp_chr_vec).")}
  if (!vp) {err <- c(err, "\n • [pat] must be a complete character scalar (?cmp_chr_scl).")}
  if (idef(err)) {stop(err)}
  grep(pat, x, fixed = T)                                                        # numerically index each atomic element of [x] containing pat
}

#' @describeIn ipat. Counts the number of times a pattern appears in each
#'   element of a character vector.
#' @export
npat <- function(x, pat) {
  vx <- cmp_chr_vec(x)
  vp <- cmp_chr_scl(pat)
  err <- NULL
  if (!vx) {err <- c(err, "\n • [x] must be a complete character scalar/vec (?cmp_chr_scl, ?cmp_chr_vec).")}
  if (!vp) {err <- c(err, "\n • [pat] must be a complete character scalar (?cmp_chr_scl).")}
  if (idef(err)) {stop(err)}
  lengths(gregexpr(pat, x, fixed = T))                                           # count the number of times each atomic element of [x] containing pat
}

#' @describeIn ipat. Identifies the position of \emph{the first} character of
#'   the first instance of a pattern in each element of a character vector.
#' @export
ppat <- function(x, pat) {
  vx <- cmp_chr_vec(x)
  vp <- cmp_chr_scl(pat)
  err <- NULL
  if (!vx) {err <- c(err, "\n • [x] must be a complete character scalar/vec (?cmp_chr_scl, ?cmp_chr_vec).")}
  if (!vp) {err <- c(err, "\n • [pat] must be a complete character scalar (?cmp_chr_scl).")}
  if (idef(err)) {stop(err)}
  out <- regexpr(pat, x, fixed = T)                                              # get the character position of the start of [pat] in each element of [x]
  out[out == -1] <- NA                                                           # replace -1 with NA to represent no match
  if (all(is.na(out))) {out <- NA}
  return(out)
}

#' @describeIn ipat. Identifies the position of the first character of
#'   \emph{every} instance of a pattern in each element of a character vector.
#' @export
ppats <- function(x, pat) {
  vx <- cmp_chr_vec(x)
  vp <- cmp_chr_scl(pat)
  err <- NULL
  if (!vx) {err <- c(err, "\n • [x] must be a complete character scalar/vec (?cmp_chr_scl, ?cmp_chr_vec).")}
  if (!vp) {err <- c(err, "\n • [pat] must be a complete character scalar (?cmp_chr_scl).")}
  if (idef(err)) {stop(err)}
  out <- gregexpr(pat, x, fixed = T)                                             # get the character position of every instance of [pat] in each element of [x]
  out[out == -1] <- NA                                                           # replace -1 with NA to represent no match
  out
}
