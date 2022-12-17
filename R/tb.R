#' @family wraps
#' @title Thin wrappers of `tibble` functions
#' @description \tabular{rl}{
#'       `tbas` \tab   Thinly wraps \code{\link[tibble]{as_tibble}}.
#'   \cr `tbis` \tab   Thinly wraps \code{\link[tibble]{is_tibble}}.
#'   \cr  `tbr` \tab   Thinly wraps \code{\link[tibble]{tribble}}.
#'   \cr  `trb` \tab   Thinly wraps \code{\link[tibble]{tibble_row}}.
#'   \cr   `tb` \tab   Thinly wraps \code{\link[tibble]{tibble}}.
#' }
#' @inherit tibble::tibble
#' @return A tibble.
#' @export
tb <- function(..., .rows = NULL, .name_repair = c("check_unique", "unique", "universal", "minimal")) {tibble::tibble(..., .rows = .rows, .name_repair = .name_repair)}

#' @rdname tb
#' @inherit tibble::tribble
#' @export
trb <- function(...) {tibble::tribble(...)}

#' @rdname tb
#' @inherit tibble::tibble_row
#' @export
tbr <- function(..., .name_repair = c("check_unique", "unique", "universal", "minimal")) {tibble::tibble_row(..., .name_repair = .name_repair)}

#' @rdname tb
#' @inherit tibble::as_tibble
#' @export
tbas <- function(x, ..., .rows = NULL, .name_repair = c("check_unique", "unique", "universal", "minimal"), rownames = pkgconfig::get_config("tibble::rownames", NULL)) {tibble::as_tibble(x, ..., .rows = .rows, .name_repair = .name_repair, rownames = rownames)}

#' @rdname tb
#' @inherit tibble::is_tibble
#' @export
tbis <- function(x) {tibble::is_tibble(x)}
