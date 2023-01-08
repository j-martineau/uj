#' @encoding UTF-8
#' @family meta
#' @title Build and evaluate code
#' @description Collapses `...` into a character scalar and evaluates it as code in the environment of the calling function.
#' @param ... Arguments to be pasted into a character scalar command to be run (i.e., parsed and evaluated) in the environment where `run(...)` was called.
#' @return `NULL`. Called for its side effect of executing code in the environment where `run(...)` was called.
#' @export
#' @return The value returned upon evaluating the character scalar created from collapsing `...` arguments.
#' @examples
#' CallVec <- paste0("'", 0:3, "'")
#' CallVec <- c("paste0(", CallVec, ", collapse = '')")
#' CallVec <- c("ResultVec <- ", CallVec)
#' run(CallVec)
#'
#' CallScl <- paste0(CallVec, collapse = "")
#' CallScl <- paste0("ResultScl <- ", CallScl)
#' run(CallScl)
#'
#' print(CallVec)
#' print(CallScl)
#' print(ResultVec)
#' print(ResultScl)
#' @export
run <- function(...) {base::eval.parent(base::parse(text = base::paste0(uj::av(...), collapse = "")), n = 1)}
