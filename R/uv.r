#' @encoding UTF-8
#' @family extensions
#' @family values
#' @title Unique atomic values
#' @description \tabular{rl}{
#'     `uv, uav`   \tab Gets unique atomic values in all `...` arguments.
#'   \cr           \tab  
#'   \cr   `suv`   \tab Gets unique sorted atomic values in all `...` arguments.
#' }
#' @param ... One or more objects to be reduced to a single atomic vector and then further reduced to unique atomic values.
#' @return An atomic vector.
#' @examples
#' abc <- letters[1:3]
#' bcd <- data.frame(letters[2:4])
#' num <- list(1, 2, 3)
#'
#' abc
#' bcd
#' num
#'
#' uv(0:3, 3:6, 6:9)
#' uav(abc, bcd, num)
#' suv(abc, bcd, num)
#' @export
uv <- function(...) {base::unique(uj::av(...))}

#' @rdname uv
#' @export
uav <- uv

#' @rdname uv
#' @export
suv <- function(...) {base::sort(base::unique(uj::av(...)))}

