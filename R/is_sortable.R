#' @name is_sortable
#' @family is_functions
#' @title Is an object and sortable or non-sortable?
#' @description Sortable objects are of non-empty atomic objects of mode
#'   'character', 'logical', 'numeric', or 'ordered'. Non-sortable objects are
#'   atomic objects of modes that are not sortable.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}
#' @export
is_sortable <- function(x) {f0(xnil(x), F, f0(!xatm(x), F, f0(all(is.na(x)), T, any(is.character(x), is.logical(x), is.numeric(x), is.ordered(x)))))}

#' @rdname is_sortable
#' @export
is_nonsortable <- function(x) {f0(xnil(x) | !xatm(x), F, f0(all(is.na(x)), T, !any(is.character(x), is.logical(x), is.numeric(x), is.ordered(x))))}
