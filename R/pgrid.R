#' @name pgrid.
#' @family strings
#' @title \code{expand.grid} for \code{paste} and \code{paste0}
#' @param ... Non-empty atomic objects.
#' @param p. \link[cmp_chr_scl]{Complete character scalar} to use as the
#'   'paste'.
#' @param ch.,blank.,na.err. \link[cmp_lgl_scl]{Complete logical scalars}
#'   indicating, respectively, whether to split arguments in \code{...} into
#'   constituent characters after conversion to mode 'character', whether to add
#'   a blank string element to each argument in \code{...}, and whether to throw
#'   an error if an argument in \code{...} contains an \code{NA} value.
#' @return A character vector.
#' @export
pgrid. <- function() {help("pgrid.", package = "uj")}

#' @describeIn pgrid. Convert arguments in \code{...} to character, separating
#'   them into their constituent characters (if \code{ch = T}), add a blank
#'   string (if \code{add.blank = T}), and create a character vector containing
#'   all possible combinations of elements in \code{...} pasted together using
#'   the 'paste' \code{p}. The first part of the character scalar in each
#'   element of the result is from \code{...1}, the second from \code{..2}, and
#'   so on.
#' @export
pgrid <- function(..., p. = " ", ch. = F, blank. = F, na.err. = T) {
  dots <- list(...)
  errs <- c(f0(length(dots) > 0                     , NULL, "\n \u2022 [...] is empty."),
            f0(all(sapply(dots, cmp_vec))           , NULL, "\n \u2022 All arguments in [...] must be complete atomic vecs."),
            f0(all(sapply(dots, length) > 0)        , NULL, "\n \u2022 [...] contains an empty element."),
            f0(cmp_chr_scl(p.)                      , NULL, "\n \u2022 [p.] must be a complete character scalar."),
            f0(isTF(ch.)                            , NULL, "\n \u2022 [ch.] must be TRUE or FALSE."),
            f0(isTF(blank.)                         , NULL, "\n \u2022 [blank.] must be TRUE or FALSE."),
            f0(isTF(na.err.)                        , NULL, "\n \u2022 [na.err.] must be TRUE or FALSE."),
            f0(!isT(na.err.) | !any(is.na(av(dots))), NULL, "\n \u2022 Arguments in [...] may not contain [NA] values when [na.err. = TRUE]."))
  if (idef(errs)) {stop(errs)}
  call <- paste0("dots[[", 1:length(dots), "]]")
  call <- paste0(call, collapse = ", ")
  call <- paste0("expand.grid(", call, ", stringsAsFactors = F)")
  out <- run(call)
  av(apply(out, 2, paste, sep = p.))
}

#' @describeIn pgrid. Call \code{pgrid} with \code{p. = ""} (a blank string).
#' @export
pgrid0 <- function(..., ch. = F, blank. = F, na.err. = T) {pgrid(..., p. = "", ch. = ch., blank. = blank., na.err. = na.err.)}
