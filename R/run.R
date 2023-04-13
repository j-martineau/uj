#' @encoding UTF-8
#' @family meta
#' @title Build and evaluate code
#' @description `run(...)` Collapses `...` into a character scalar and evaluates it as code in the environment of the calling function.
#' @param ... Arguments to be pasted into a character scalar command to be run (i.e., parsed and evaluated) in the environment where `run(...)` was called.
#' @return The value returned upon evaluating the character scalar created from collapsing `...` arguments.
#' @examples
#' egCallVec <- paste0("'", 0:3, "'")
#' egCallVec <- c("paste0(", egCallVec, ", collapse = '')")
#' egCallVec <- c("egResultVec <- ", egCallVec)
#' run(egCallVec)
#'
#' egCallScl <- paste0(egCallVec, collapse = "")
#' egCallScl <- paste0("egResultScl <- ", egCallScl)
#' run(egCallScl)
#'
#' print(egCallVec)
#' print(egCallScl)
#' print(egResultVec)
#' print(egResultScl)
#'
#' egRUNalias1 <- function(x, y) {egDTFalias(var.x = x, var.y = y)}
#' egRUNalias2 <- function(x, y) {egRUNalias1(x, y)}
#'
#' @export
run <- function(...) {
  Code <- base::paste0(uj::av(...), collapse = "")
  base::eval.parent(base::parse(text = Code), n = 1)
}
