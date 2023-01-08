#' @name envir_vals
#' @encoding UTF-8
#' @family environments
#' @family extensions
#' @title Objects in environments of calling functions
#' @description All functions in this family operate in the environment of the calling function `gens` generations back in the call stack.
#' \tabular{rl}{
#'     `vexists`   \tab Checks for the existence of an object in the environment of the `gens`-specified calling function.
#'   \cr           \tab  
#'   \cr  `vget`   \tab Gets the value of an object in the environment of the `gens`-specified calling function.
#'   \cr           \tab  
#'   \cr  `vset`   \tab Sets the value of an object in the environment of the `gens`-specified calling function.
#' }
#' @param name A \link[=cmp_chr_scl]{complete character scalar} giving the name of an object.
#' @param val A value to place into the object specified by `name`.
#' @param err A non`NA` logical scalar indicating whether to throw an error if the object specified by `name` does not exist (i.e., rather than returning `FALSE`).
#' @param gens A \link[=cmp_psw_scl]{complete positive whole-number scalar} giving the number of generations back in the function call stack to go.
#' @return *A logical scalar*
#' \cr   `vexists`
#' \cr
#' \cr *An* R *object*
#' \cr   `vget`
#' \cr
#' \cr `NULL`
#' \cr   `vset`
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
  errs <- base::c(uj::f0(uj::cmp_chr_scl(name), NULL, "[name] must be a complete character scalar (?cmp_chr_scl)."),
                  uj::f0(uj::isTF(err)        , NULL, "[err] must be TRUE or FALSE."),
                  uj::f0(uj::cmp_psw_scl(gens), NULL, "[gens] must be a positive whole number scalar (?cmp_psw_scl)."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  if (base::exists(name, base::parent.frame(gens + 1), inherits = F)) {return(T)}
  if (!err) {return(F)}
  if (gens > 1) {targ <- c("target function (", uj::callers(gens + 1), ") ", gens, " generations back from the ")}
  else {targ <- ""}
  call <- uj::callers(1)
  stop(uj:::.errs(uj::p0("No object named [name = '", name, "'] exists in the environment of the ", targ, "calling function (", call, ").")))
}

#' @rdname envir_vals
#' @export
vget <- function(name, err = T, gens = 1) {
  errs <- base::c(uj::f0(uj::cmp_chr_scl(name), NULL, "[name] must be a complete character scalar (?cmp_chr_scl)."),
                  uj::f0(uj::isTF(err)        , NULL, "[err] must be TRUE or FALSE."),
                  uj::f0(uj::cmp_psw_scl(gens), NULL, "[gens] must be a positive whole number scalar (?cmp_psw_scl)."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  if (!uj::vexists(name, err, gens + 1)) {NULL}
  else {base::get(name, envir = base::parent.frame(gens + 1), inherits = F)}
}

#' @rdname envir_vals
#' @export
vset <- function(name, val, gens = 1) {
  errs <- base::c(uj::f0(uj::cmp_chr_scl(name), NULL, "[name] must be a complete character scalar (?cmp_chr_scl)."),
                  uj::f0(uj::cmp_psw_scl(gens), NULL, "[gens] must be a positive whole number scalar (?cmp_psw_scl)."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  base::assign(name, val, envir = base::parent.frame(gens + 1), inherits = F)
}
