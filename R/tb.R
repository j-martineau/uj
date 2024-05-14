#' @encoding UTF-8
#' @family wraps
#' @title `tibble` wrappers
#' @description Thin wrappers of functions from package `tibble`. See links in *details* for function-specific documentation.
#' @details
#' \tabular{ll}{  `as_tb`   \tab Thinly wraps \code{\link[tibble]{as_tibble}}.  \cr
#'                `is_tb`   \tab Thinly wraps \code{\link[tibble]{is_tibble}}.  \cr
#'                `trb`     \tab Thinly wraps \code{\link[tibble]{tribble}}.    \cr
#'                `tbr`     \tab Thinly wraps \code{\link[tibble]{tibble_row}}. \cr
#'                `tb`      \tab Thinly wraps \code{\link[tibble]{tibble}}.       }
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
tb <- tibble::tibble

#' @rdname tb
#' @export
trb <- tibble::tribble

#' @rdname tb
#' @export
tbr <- tibble::tibble_row

#' @rdname tb
#' @export
as_tb <- tibble::as_tibble

#' @rdname tb
#' @export
is_tb <- tibble::is_tibble
