#' @name wraps_tibble
#' @family wraps
#' @title Thin wrappers of `tibble` functions
#' @description \itemize{
#'   \item **`tb`**: thinly wraps \code{\link[tibble]{tibble}}.
#'   \item **`tbr`**: thinly wraps \code{\link[tibble]{tribble}}.
#'   \item **`trb`**: thinly wraps \code{\link[tibble]{tibble_row}}.
#'   \item **`tbas`**: thinly wraps \code{\link[tibble]{as_tibble}}.
#'   \item **`tbis`**: thinly wraps \code{\link[tibble]{is_tibble}}.
#' }
#' @return A tibble.
#' @export
wraps_tibble <- NULL

#' @rdname wraps_tibble
#' @inherit tibble::tibble
#' @export
tb <- function(..., .rows = NULL, .name_repair = c("check_unique", "unique", "universal", "minimal")) {tibble::tibble(..., .rows = .rows, .name_repair = .name_repair)}

#' @rdname wraps_tibble
#' @inherit tibble::tribble
#' @export
trb <- function(...) {tibble::tribble(...)}

#' @rdname wraps_tibble
#' @inherit tibble::tibble_row
#' @export
tbr <- function(..., .name_repair = c("check_unique", "unique", "universal", "minimal")) {tibble::tibble_row(..., .name_repair = .name_repair)}

#' @rdname wraps_tibble
#' @inherit tibble::as_tibble
#' @export
tbas <- function(x, ..., .rows = NULL, .name_repair = c("check_unique", "unique", "universal", "minimal"), rownames = pkgconfig::get_config("tibble::rownames", NULL)) {tibble::as_tibble(x, ..., .rows = .rows, .name_repair = .name_repair, rownames = rownames)}

#' @rdname wraps_tibble
#' @inherit tibble::is_tibble
#' @export
tbis <- function(x) {tibble::is_tibble(x)}
