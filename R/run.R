#' @title Build and Evaluate Code
#' @family code
#' @family extensions
#' @family environments
#' @description Collapses `...` into a character scalar and evaluates
#'   it as code in the environment of the calling function.
#' @param ... Arguments to be pasted into a character scalar command to be run
#'   (i.e., parsed and evaluated) in the environment where `run(...)` was
#'   called.
#' @return `NULL`. Called for its side effect of executing code in the
#'   environment where `run(...)` was called.
#' @examples
#' Join <- ""
#' CallVector <- c("paste0(", 0:3, ", collapse = Join)")
#' CallScalar <- paste0(CallVector, collapse = Join)
#' CallVector <- c("Vector <- ", CallVector)
#' CallScalar <- paste0("Scalar <- ", CallScalar)
#' run(CallVector)
#' run(CallScalar)
#' print(CallVector)
#' print(CallScalar)
#' @export
run <- function(...) {eval.parent(parse(text = paste0(av(...), collapse = "")), n = 1)}
