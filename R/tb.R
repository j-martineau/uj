#' @encoding UTF-8
#' @family wraps
#' @title `tibble` wrappers
#' @description Thin wrappers of functions from package `tibble`.
#' @return A tibble.
#' @export
#' @examples
#' tb(letters = letters, numbers = 1:26, constant = pi)
#' trb(~letters, ~numbers, ~constant,
#'     "a"     , 1       , pi       ,
#'     "b"     , 2       , pi       ,
#'     "c"     , 3       , pi       )
#' tbr("a", 1, pi)
#' as_tb(list(letters = letters, numbers = 1:26, constant = pi))
#' is_tb(tb(letters))
tb_help <- function() {utils::help("tb", package = "uj")}

#' @describeIn tb_help Thinly wraps \code{\link[tibble]{tibble}}.
#' @export
tb <- tibble::tibble

#' @describeIn tb_help Thinly wraps \code{\link[tibble]{tribble}}.
#' @export
trb <- tibble::tribble

#' @describeIn tb_help Thinly wraps \code{\link[tibble]{tibble_row}}.
#' @export
tbr <- tibble::tibble_row

#' @describeIn tb_help Thinly wraps \code{\link[tibble]{as_tibble}}.
#' @export
as_tb <- tibble::as_tibble

#' @describeIn tb_help Thinly wraps \code{\link[tibble]{is_tibble}}.
#' @export
is_tb <- tibble::is_tibble
