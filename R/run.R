#' @name run.
#' @family code
#' @family extensions
#' @family environments
#' @title Build and evaluate code
#' @param ... Arguments to be pasted into a character scalar command to be run
#'   (i.e., parsed and evaluated) in the environment where \code{run(...)} was
#'   called.
#' @return \code{NULL}. Called for its side effect of executing code in the
#'   environment where \code{run(...)} was called.
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
run. <- function() {help("run.", package = "uj")}

#' @describeIn run. Collapses \code{...} into a character scalar and evaluates
#'   it as code in the calling environment.
#' @export
run <- function(...) {eval.parent(parse(text = daw00(...)), n = 1)}
