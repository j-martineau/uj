#' @name wrap_tibble
#' @family wraps
#' @title Thin wraps of functions from package \code{tibble}.
#' @description The following table describes wrapper functions in this group of
#'   functions:\tabular{ll}{
#'   WRAPPER       \tab TIBBLE FUNCTION                                      \cr
#'   \code{tb}     \tab \code{\link[tibble]{tibble}}                         \cr
#'   \code{trb}    \tab \code{\link[tibble]{tibble_row}}                     \cr
#'   \code{tbr}    \tab \code{\link[tibble]{tribble}}                        \cr
#'   \code{tbas}   \tab \code{\link[tibble]{as_tibble}}                      \cr
#'   \code{tbis}   \tab \code{\link[tibble]{is_tibble}}                        }
#' @param file Either a character scalar or vector that resolves to a file path
#'   or \code{NULL} (in which case the user is asked to select a file using a
#'   system dialog box.
#' @return A tibble.
#' @export
wrap_tibble <- NULL

#' @rdname wrap_tibble
#' @inherit tibble::tibble
#' @export
tb <- function(..., .rows = NULL, .name_repair = c("check_unique", "unique", "universal", "minimal")) {tibble::tibble(..., .rows = .rows, .name_repair = .name_repair)}

#' @rdname wrap_tibble
#' @inherit tibble::tribble
#' @export
trb <- function(...) {tibble::tribble(...)}

#' @rdname wrap_tibble
#' @inherit tibble::tibble_row
#' @export
tbr <- function(..., .name_repair = c("check_unique", "unique", "universal", "minimal")) {tibble::tibble_row(..., .name_repair = .name_repair)}

#' @rdname wrap_tibble
#' @inherit tibble::as_tibble
#' @export
tbas <- function(x, ..., .rows = NULL, .name_repair = c("check_unique", "unique", "universal", "minimal"), rownames = pkgconfig::get_config("tibble::rownames", NULL)) {tibble::as_tibble(x, ..., .rows = .rows, .name_repair = .name_repair, rownames = rownames)}

#' @rdname wrap_tibble
#' @inherit tibble::is_tibble
#' @export
tbis <- function(x) {tibble::is_tibble(x)}
