#' @encoding UTF-8
#' @family meta
#' @title Build and evaluate code
#' @description `run(...)` Collapses `...` into a character scalar and evaluates it as code in the environment of the calling function.
#' \cr\cr `run_alias(pkg, fun)` constructs a call to `{pkg}::{fun}(.)` with arguments being `...` args available in the environment from which `run_alias` is called. See **examples**.
#' @param ... Arguments to be pasted into a character scalar command to be run (i.e., parsed and evaluated) in the environment where `run(...)` was called.
#' @param pkg Complete scalar character (?cmp_chr_scl) package name (may be a blank string).
#' @param fun Complete scalar string (?cmp_str_scl) function name.
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
#' egDTFalias <- function(...) {run_alias("base", "data.frame")}
#' egRUNalias1 <- function(x, y) {egDTFalias(var.x = x, var.y = y)}
#' egRUNalias2 <- function(x, y) {egRUNalias1(x, y)}
#'
#' @export
run <- function(...) {
  code <- base::paste0(uj::av(...), collapse = "")
  base::eval.parent(base::parse(text = code), n = 1)
}

#' @rdname run
#' @export
run_alias <- function(pkg, fun) {
  args <- base::as.list(base::sys.call(which = 2))
  vals <- base::as.character(base::sys.call(which = 2))
  args <- args[2:base::length(args)]
  vals <- vals[2:base::length(vals)]
  char <- base::sapply(args, base::is.character)
  vals[char] <- base::paste0("\"", vals[char], "\"")
  names <- base::names(args)
  if (base::length(names) > 0) {
    named <- names != ""
    names[named] <- base::paste0(names[named], " = ")
  }
  args <- base::paste0(names, vals)
  args <- base::paste0(args, collapse = ", ")
  call <- base::paste0(pkg, "::", fun, "(", args, ")")
  base::eval.parent(base::parse(text = call), n = 2)
}
