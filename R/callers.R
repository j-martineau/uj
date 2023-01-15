#' @encoding UTF-8
#' @family environments
#' @family meta
#' @title Generations of functions in the call stack
#' @description \tabular{rl}{
#'      `ncallers`   \tab Gets the number of functions in the call stack.
#'   \cr `callers`   \tab Gets names of all calling functions, with the immediate calling function in position `1`, optionally selecting the `gens.`-th element(s).
#'   \cr  `caller`   \tab Gets the name of the immediate calling function.
#' }
#' @param gens A \link[cmp_psw_vec]{complete positive whole-number vec} giving the number(s) of generations back in the function call stack to go.
#' @return *A character vector* \cr   `callers`
#'  \cr\cr *A character scalar* \cr   `caller`
#'  \cr\cr *A numeric scalar* \cr   `ncallers`
#' @examples
#' fun.a <- function() {A <- 0; fun.b()}
#' fun.b <- function() {B <- 1; fun.c()}
#' fun.c <- function() {
#'   cat("\n callers(): " , paste0("'", callers(), "'"))
#'   cat("\n callers(1): '", callers(1), "'")
#'   cat("\n callers(2): '", callers(2), "'")
#'   cat("\n")
#'   cat("\n vexists('A', err = F, gens = 1): ", vexists('A', err = F, gens = 1))
#'   cat("\n vexists('A', err = F, gens = 2): ", vexists('A', err = F, gens = 2))
#'   cat("\n vget('A', err = F, gens = 2): ", vget('A', err = F, gens = 2))
#'   cat("\n")
#'   cat("\n vexists('B', err = F, gens = 1): ", vexists('B', err = F, gens = 1))
#'   cat("\n vexists('B', err = F, gens = 2): ", vexists('B', err = F, gens = 2))
#'   cat("\n vget('B', err = F, gens = 1): ", vget('B', err = F, gens = 1))
#' }
#' fun.a()
#' @export
callers <- function(gens = NULL) {
  if (!uj::inll(gens) & !uj::cmp_psw_vec(gens)) {stop(uj::format_errs(pkg = "uj", "[gens] must be NULL or a positive whole-number vec (?cmp_psw_vec)."))}
  x <- uj::xpkg(base::rev(base::as.character(base::sys.calls())))                # get the call stack
  if (base::length(x) >= 3) {                                                    # if the length of the call stack is > 4, it was not called from the command line, so
    paren <- uj::av(base::regexpr("(", x, fixed = T))                            # : find the position of the parenthesis in each line of the call stack
    x     <- base::substr(x, 1, paren - 1)                                       # : extract the function names (i.e., before the open parentheses)
    x <- x[3:base::length(x)]                                                    # : select the relevant parts of the call stack
  } else {x <- NULL}                                                             # otherwise, the function was called at the command line
  if (uj::inll(gens)) {gens <- 1:base::length(x)}
  if (base::max(gens) > base::length(x)) {stop(uj::format_errs(pkg = "uj", "One or more elements of [gens] is greater than the number of calling functions."))}
  x[gens]                                                                        # otherwise, if the specified generations exist, return just those gens
}

#' @rdname callers
#' @export
caller <- function() {
  calls <- uj::callers()
  calls[2]
}

#' @rdname callers
#' @export
ncallers <- function() {base::length(uj::callers())}
