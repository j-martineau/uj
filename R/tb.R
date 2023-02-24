#' @encoding UTF-8
#' @family wraps
#' @title `tibble` wrappers
#' @description Thin wrappers of functions from package `tibble`. See links in *details* for function-specific documentation.
#' @details
#' \tabular{ll}{  `astb`   \tab Thinly wraps \code{\link[tibble]{as_tibble}}.  \cr
#'                `istb`   \tab Thinly wraps \code{\link[tibble]{is_tibble}}.  \cr
#'                `tbr`    \tab Thinly wraps \code{\link[tibble]{tribble}}.    \cr
#'                `trb`    \tab Thinly wraps \code{\link[tibble]{tibble_row}}. \cr
#'                `tb`     \tab Thinly wraps \code{\link[tibble]{tibble}}.       }
#' @return A tibble.
#' @export
#' @examples
#' tb(letters = letters, numbers = 1:26, constant = pi)
#' trb(~letters, ~numbers, ~constant,
#'     "a"     , 1,      , pi       ,
#'     "b"     , 2,      , pi       ,
#'     "c"     , 3,      , pi       )
#' tbr("a", 1, pi)
#' asTB(list(letters = letters, numbers = 1:26, constant = pi))
#' isTB(tb(letters))
tb <- function(...) {uj::run_alias("tibble", "tibble")}

#' @rdname tb
#' @export
trb <- function(...) {uj::run_alias("tibble", "tribble")}

#' @rdname tb
#' @export
tbr <- function(...) {uj::run_alias("tibble", "tibble_row")}

#' @rdname tb
#' @export
astb <- function(...) {uj::run_alias("tibble", "as_tibble")}

#' @rdname tb
#' @export
istb <- function(x) {tibble::is_tibble(x)}
