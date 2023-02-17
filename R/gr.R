#' @name gr
#' @encoding UTF-8
#' @family strings
#' @family plots
#' @family wraps
#' @title Thin wrappers of `graphics` functions.
#' @details
#' \tabular{ll}{  `strh`   \tab \code{\link[graphics]{strheight}}. \cr
#'                `strw`   \tab \code{\link[graphics]{strwidth}}.    }
#' @export
#' @examples
#' sent1 <- "First sentence."
#' sent2 <- "Second sentence."
#' sent12 <- paste0(sent1, "\n", sent2)
#' strh(sent1)
#' strh(sent2)
#' strh(sent12)
#' strw(sent1)
#' strw(sent2)
#' strw(sent12)
strh <- function(...) {uj::run_alias("graphics", "strheight")}

#' @rdname gr
#' @export
strw <- function(...) {uj::run_alias("graphics", "strheight")}
