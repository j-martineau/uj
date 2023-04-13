#' @encoding UTF-8
#' @family environments
#' @family extensions
#' @title Objects in environments of calling functions
#' @description All functions in this family operate in the environment of the calling function `Gens` generations back in the call stack.
#' \tabular{ll}{  `value_exists, val_exists, vexists`   \tab Checks for the existence of an object in the environment of the `Gens`-specified calling function. \cr   \tab   \cr
#'                `get_value, get_val, getv`            \tab Gets the value of an object in the environment of the `Gens`-specified calling function.           \cr   \tab   \cr
#'                `set_value, set_val, setv`            \tab Sets the value of an object in the environment of the `Gens`-specified calling function.                          }
#' @param Name A \link[=cmp_chr_scl]{complete character scalar} giving the Name of an object.
#' @param Val A value to place into the object specified by `Name`.
#' @param Err A non`NA` logical scalar indicating whether to throw an error if the object specified by `Name` does not exist (i.e., rather than returning `FALSE`).
#' @param Gens A \link[=cmp_psw_scl]{complete positive whole-number scalar} giving the number of generations back in the function call stack to go.
#' @return **A logical scalar**      \cr\cr `value_exists, val_exists, vexists`
#' \cr\cr  **The** `NULL` **object** \cr\cr `set_value, set_val, setv`
#' \cr\cr  **An object**             \cr\cr `get_value, get_val, getv`
#' @examples
#' egFunC <- function() {
#'   parent <- callers(1)
#'   grand  <- callers(2)
#'
#'   say("\n Parent Function: '", parent, "'")
#'   say("\n Grandparent Function: '", grand , "'")
#'   say("\n")
#'   say("\n Variable 'A' exists in parent function '", parent, "': ", val_exists('A', Err = FALSE, Gens = 1))
#'   say("\n Variable 'B' exists in parent function '", parent, "': ", val_exists('B', Err = FALSE, Gens = 1))
#'   say("\n")
#'   say("\n Variable 'A' exists in grandparent function '", grand, "': ", val_exists('A', Err = FALSE, Gens = 2))
#'   say("\n Variable 'B' exists in grandparent function '", grand, "': ", val_exists('B', Err = FALSE, Gens = 2))
#'   say("\n")
#'   say("\n Value of variable 'B' in parent function '", parent, "': ", get_value('B', Err = FALSE, Gens = 1))
#'   say("\n Value of variable 'A' in grandparent function '", grand, "': ", get_value('A', Err = FALSE, Gens = 2))
#'
#'   vSET('A', 'A', Gens = 1)
#'   vSET('B', 'B', Gens = 2)
#'
#'   say("\n")
#'   say("\n vSET('A', 'A', Gens = 1)")
#'   say("\n vSET('B', 'B', Gens = 2)")
#'   say("\n")
#'   say("\n Variable 'A' exists in parent function '", parent, "': ", val_exists('A', Err = FALSE, Gens = 1))
#'   say("\n Variable 'B' exists in parent function '", parent, "': ", val_exists('B', Err = FALSE, Gens = 1))
#'   say("\n")
#'   say("\n Variable 'A' exists in grandparent function '", grand, "': ", val_exists('A', Err = FALSE, Gens = 2))
#'   say("\n Variable 'B' exists in grandparent function '", grand, "': ", val_exists('B', Err = FALSE, Gens = 2))
#'   say("\n")
#'   say("\n Value of variable 'A' in parent function '", parent, "': ", get_value('A', Err = FALSE, Gens = 1))
#'   say("\n Value of variable 'B' in parent function '", parent, "': ", get_value('B', Err = FALSE, Gens = 1))
#'   say("\n")
#'   say("\n Value of variable 'A' in grandparent function '", grand, "': ", get_value('A', Err = FALSE, Gens = 2))
#'   say("\n Value of variable 'B' in grandparent function '", grand, "': ", get_value('B', Err = FALSE, Gens = 2))
#' }
#' egFunB <- function() {B <- 1; egFunC()}
#' egFunA <- function() {A <- 0; egFunB()}
#' egFunA()
#' @export
value_exists <- function(Name, Err = T, Gens = 1) {
  uj:::.value_errs(Name = Name, Err = Err, Gens = Gens)
  if (!base::exists(Name, base::parent.frame(Gens + 1), inherits = F)) {
    Call <- uj::caller()
    Targ <- uj::callers(Gens + 1)
    if (Gens > 1) {Targ <- base::c("target function (", Targ, ") ", Gens, " generations back from the ")} else {Targ <- ""}
    if (Err) {uj::stopperr(base::paste0("No object named [Name = '", Name, "'] exists in the environment of the ", Targ, "calling function (", Call, ")."), PKG = "uj")}
    F
  } else {T}
}

#' @rdname value_exists
#' @export
get_value <- function(Name, Err = T, Gens = 1) {
  uj:::.value_errs(Name = Name, Err = Err, Gens = Gens)
  if (base::exists(Name, base::parent.frame(Gens + 1), inherits = F)) {base::get(Name, envir = base::parent.frame(Gens + 1), inherits = F)} else {NULL}
}

#' @rdname value_exists
#' @export
set_value <- function(Name, Val, Gens = 1) {
  uj:::.value_errs(Name = Name, Err = F, Gens = Gens)
  base::assign(Name, Val, envir = base::parent.frame(Gens + 1), inherits = F)
}

#' @rdname value_exists
#' @export
val_exists <- value_exists

#' @rdname value_exists
#' @export
get_val <- get_value

#' @rdname value_exists
#' @export
set_val <- set_value

#' @rdname value_exists
#' @export
vexists <- value_exists

#' @rdname value_exists
#' @export
getv <- get_value

#' @rdname value_exists
#' @export
setv <- set_value
