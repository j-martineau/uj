#' @name envir_vals
#' @family environments
#' @title Objects in environments of calling functions
#' @description All functions in this family operate in the environment of the calling function `gens` generations back in the call stack.
#' \tabular{rl}{
#'    `vexists`   \tab Checks for the existence of an object in the environment of the `gens`-specified calling function.
#'   \cr `vget`   \tab Gets the value of an object in the environment of the `gens`-specified calling function.
#'   \cr `vset`   \tab Sets the value of an object in the environment of the `gens`-specified calling function.
#' }
#' @param name A \link[=cmp_chr_scl]{complete character scalar} giving the name of an object.
#' @param val A value to place into the object specified by `name`.
#' @param err A non`NA` logical scalar indicating whether to throw an error if the object specified by `name` does not exist (i.e., rather than returning `FALSE`).
#' @param gens A \link[=cmp_psw_scl]{complete positive whole-number scalar} giving the number of generations back in the function call stack to go.
#' @return \tabular{rl}{
#'    `vexists` \tab   A logical scalar.
#'   \cr `vget` \tab   An R object.
#'   \cr `vset` \tab   `NULL`
#' }
#' @examples
#' fun.a <- function() {A <- 0; fun.b()}
#' fun.b <- function() {B <- 1; fun.c()}
#' fun.c <- function() {
#'   parent <- callers(1)
#'   grand  <- callers(2)
#'
#'   say("\n Parent Function: '", parent, "'")
#'   say("\n Grandparent Function: '", grand , "'")
#'   say("\n")
#'   say("\n Variable 'A' exists in parent function '", parent, "': ", vexists('A', err = FALSE, gens = 1))
#'   say("\n Variable 'B' exists in parent function '", parent, "': ", vexists('B', err = FALSE, gens = 1))
#'   say("\n")
#'   say("\n Variable 'A' exists in grandparent function '", grand, "': ", vexists('A', err = FALSE, gens = 2))
#'   say("\n Variable 'B' exists in grandparent function '", grand, "': ", vexists('B', err = FALSE, gens = 2))
#'   say("\n")
#'   say("\n Value of variable 'B' in parent function '", parent, "': ", vget('B', err = FALSE, gens = 1))
#'   say("\n Value of variable 'A' in grandparent function '", grand, "': ", vget('A', err = FALSE, gens = 2))
#'
#'   vset('A', 'A', gens = 1)
#'   vset('B', 'B', gens = 2)
#'
#'   say("\n")
#'   say("\n vset('A', 'A', gens = 1)")
#'   say("\n vset('B', 'B', gens = 2)")
#'   say("\n")
#'   say("\n Variable 'A' exists in parent function '", parent, "': ", vexists('A', err = FALSE, gens = 1))
#'   say("\n Variable 'B' exists in parent function '", parent, "': ", vexists('B', err = FALSE, gens = 1))
#'   say("\n")
#'   say("\n Variable 'A' exists in grandparent function '", grand, "': ", vexists('A', err = FALSE, gens = 2))
#'   say("\n Variable 'B' exists in grandparent function '", grand, "': ", vexists('B', err = FALSE, gens = 2))
#'   say("\n")
#'   say("\n Value of variable 'A' in parent function '", parent, "': ", vget('A', err = FALSE, gens = 1))
#'   say("\n Value of variable 'B' in parent function '", parent, "': ", vget('B', err = FALSE, gens = 1))
#'   say("\n")
#'   say("\n Value of variable 'A' in grandparent function '", grand, "': ", vget('A', err = FALSE, gens = 2))
#'   say("\n Value of variable 'B' in grandparent function '", grand, "': ", vget('B', err = FALSE, gens = 2))
#' }
#' fun.a()
#' @export
vexists <- function(name, err = T, gens = 1) {
  errs <- c(f0(cmp_chr_scl(name), NULL, "[name] must be a complete character scalar (?cmp_chr_scl)."),
            f0(isTF(err)        , NULL, "[err] must be TRUE or FALSE."),
            f0(cmp_psw_scl(gens), NULL, "[gens] must be a positive whole number scalar (?cmp_psw_scl)."))
  if (!is.null(errs)) {stop(.errs(errs))}
  if (exists(name, parent.frame(gens + 1), inherits = F)) {return(T)}
  if (!err) {return(F)}
  if (gens > 1) {targ <- c("target function (", callers(gens + 1), ") ", gens, " generations back from the ")}
  else {targ <- ""}
  call <- callers(1)
  stop(.errs(p0("No object named [name = '", name, "'] exists in the environment of the ", targ, "calling function (", call, ").")))
}

#' @rdname envir_vals
#' @export
vget <- function(name, err = T, gens = 1) {
  errs <- c(f0(cmp_chr_scl(name), NULL, "[name] must be a complete character scalar (?cmp_chr_scl)."),
            f0(isTF(err)        , NULL, "[err] must be TRUE or FALSE."),
            f0(cmp_psw_scl(gens), NULL, "[gens] must be a positive whole number scalar (?cmp_psw_scl)."))
  if (!is.null(errs)) {stop(.errs(errs))}
  if (!vexists(name, err, gens + 1)) {NULL}
  else {get(name, envir = parent.frame(gens + 1), inherits = F)}
}

#' @rdname envir_vals
#' @export
vset <- function(name, val, gens = 1) {
  errs <- c(f0(cmp_chr_scl(name), NULL, "[name] must be a complete character scalar (?cmp_chr_scl)."),
            f0(cmp_psw_scl(gens), NULL, "[gens] must be a positive whole number scalar (?cmp_psw_scl)."))
  if (!is.null(errs)) {stop(.errs(errs))}
  assign(name, val, envir = parent.frame(gens + 1), inherits = F)
}
