#' @encoding UTF-8
#' @family environments
#' @title Objects in environments of calling functions
#' @description All functions in this family operate in the environment of the calling function `gens` generations back in the call stack.
#' \tabular{ll}{  `value_exists, val_exists, vexists`   \tab Checks for the existence of an object in the          \cr
#'                `exists_value, exists_val, existsv`   \tab environment of the `gens`-specified calling function. \cr   \tab   \cr
#'                `get_value, get_val, getv`            \tab Gets the value of an object in the environment of the \cr
#'                `value_get, val_get, vget`            \tab `gens`-specified calling function.                    \cr   \tab   \cr
#'                `set_value, set_val, setv`            \tab Sets the value of an object in the environment of the \cr
#'                `value_set, val_set, vset`            \tab `gens`-specified calling function.                      }
#' @param name A \link[=cmp_chr_scl]{complete character scalar} giving the name of an object.
#' @param val A value to place into the object specified by `name`.
#' @param err A non`NA` logical scalar indicating whether to throw an error if the object specified by `name` does not exist (i.e., rather than returning `FALSE`).
#' @param gens A \link[=cmp_psw_scl]{complete positive whole-number scalar} giving the number of generations back in the function call stack to go.
#' @return **A logical scalar**      \cr\cr `value_exists, val_exists, vexists, ...`
#' \cr\cr  **The** `NULL` **object** \cr\cr `set_value, set_val, setv, ...`
#' \cr\cr  **An object**             \cr\cr `get_value, get_val, getv, ...`
#' @examples
#' egValueExists <- function() {
#'   egValueExistsC <- function() {
#'     parent <- uj::caller1()
#'     grand  <- uj::caller2()
#'     aInParent  <- uj::value_exists('A', err = FALSE, gens = 1)
#'     bInParent  <- uj::val_exists('B', err = FALSE, gens = 1)
#'     aInGrand   <- uj::vexists('A', err = FALSE, gens = 2)
#'     bInGrand   <- uj::existsv('B', err = FALSE, gens = 2)
#'     aValParent <- uj::get_value('A', err = FALSE, gens = 1)
#'     bValParent <- uj::get_val('B', err = FALSE, gens = 1)
#'     aValGrand  <- uj::getv('A', err = FALSE, gens = 2)
#'     bValGrand  <- uj::vget('B', err = FALSE, gens = 2)
#'     uj::say("\nParent Function     : '", parent, "'")
#'     uj::say("\nGrandparent Function: '", grand , "'")
#'     uj::say("\n")
#'     uj::say("\nDoes variable 'A' exist in parent function '", parent, "'? ", aInParent)
#'     uj::say("\nDoes variable 'B' exist in parent function '", parent, "'? ", bInParent)
#'     uj::say("\n")
#'     uj::say("\nDoes variable 'A' exist in grandparent function '", grand, "'? ", aInGrand)
#'     uj::say("\nDoes variable 'B' exist in grandparent function '", grand, "'? ", bInGrand)
#'     uj::say("\n")
#'     uj::say("\nValue of variable 'A' in parent function '", parent, "': ", aValParent)
#'     uj::say("\nValue of variable 'B' in parent function '", parent, "': ", bValParent)
#'     uj::say("\n")
#'     uj::say("\nValue of variable 'A' in grandparent function '", grand, "': ", aValGrand)
#'     uj::say("\nValue of variable 'B' in grandparent function '", grand, "': ", bValGrand)
#'     uj::vset('A', 'A', gens = 1)
#'     uj::vset('B', 'B', gens = 2)
#'     aInParent  <- uj::exists_val('A', err = FALSE, gens = 1)
#'     bInParent  <- uj::exists_value('B', err = FALSE, gens = 1)
#'     aInGrand   <- uj::value_exists('A', err = FALSE, gens = 2)
#'     bInGrand   <- uj::val_exists('B', err = FALSE, gens = 2)
#'     aValParent <- uj::val_get('A', err = FALSE, gens = 1)
#'     bValParent <- uj::value_get('B', err = FALSE, gens = 1)
#'     aValGrand  <- uj::get_value('A', err = FALSE, gens = 2)
#'     bValGrand  <- uj::get_val('B', err = FALSE, gens = 2)
#'     uj::say("\nParent Function     : '", parent, "'")
#'     uj::say("\nGrandparent Function: '", grand , "'")
#'     uj::say("\n")
#'     uj::say("\nDoes variable 'A' exist in parent function '", parent, "'? ", aInParent)
#'     uj::say("\nDoes variable 'B' exist in parent function '", parent, "'? ", bInParent)
#'     uj::say("\n")
#'     uj::say("\nDoes variable 'A' exist in grandparent function '", grand, "'? ", aInGrand)
#'     uj::say("\nDoes variable 'B' exist in grandparent function '", grand, "'? ", bInGrand)
#'     uj::say("\n")
#'     uj::say("\nValue of variable 'A' in parent function '", parent, "': ", aValParent)
#'     uj::say("\nValue of variable 'B' in parent function '", parent, "': ", bValParent)
#'     uj::say("\n")
#'     uj::say("\nValue of variable 'A' in grandparent function '", grand, "': ", aValGrand)
#'     uj::say("\nValue of variable 'B' in grandparent function '", grand, "': ", bValGrand)
#'   }
#'   egValueExistsB <- function() {B <- 1; egValueExistsC()}
#'   egValueExistsA <- function() {A <- 0; egValueExistsB()}
#'   egValueExistsA()
#' }
#' egValueExists()
#' @export
value_exists <- function(name, err = T, gens = 1) {
  uj:::.value_errs(name = name, err = err, gens = gens)
  if (!base::exists(name, base::parent.frame(gens + 1), inherits = F)) {
    call <- uj::caller()
    targ <- uj::callersN(gens + 1)
    if (gens > 1) {Targ <- base::c("target function (", targ, ") ", gens, " generations back from the ")} else {targ <- ""}
    if (err) {uj::stopperr(base::paste0("No object named [name = '", name, "'] exists in the environment of the ", targ, "calling function (", call, ")."))}
    F
  } else {T}
}

#' @rdname value_exists
#' @export
get_value <- function(name, err = T, gens = 1) {
  uj:::.value_errs(name = name, err = err, gens = gens)
  if (base::exists(name, base::parent.frame(gens + 1), inherits = F)) {base::get(name, envir = base::parent.frame(gens + 1), inherits = F)} else {NULL}
}

#' @rdname value_exists
#' @export
set_value <- function(name, val, gens = 1) {
  uj:::.value_errs(name = name, err = F, gens = gens)
  base::assign(name, val, envir = base::parent.frame(gens + 1), inherits = F)
}

#' @rdname value_exists
#' @export
exists_value <- value_exists

#' @rdname value_exists
#' @export
exists_val <- value_exists

#' @rdname value_exists
#' @export
existsv <- value_exists

#' @rdname value_exists
#' @export
vexists <- value_exists

#' @rdname value_exists
#' @export
val_exists <- value_exists

#' @rdname value_exists
#' @export
value_get <- get_value

#' @rdname value_exists
#' @export
val_get <- get_value

#' @rdname value_exists
#' @export
vget <- get_value

#' @rdname value_exists
#' @export
getv <- get_value

#' @rdname value_exists
#' @export
get_val <- get_value

#' @rdname value_exists
#' @export
value_set <- set_value

#' @rdname value_exists
#' @export
val_set <- set_value

#' @rdname value_exists
#' @export
vset <- set_value

#' @rdname value_exists
#' @export
setv <- set_value

#' @rdname value_exists
#' @export
set_val <- set_value

#' @rdname value_exists
#' @export
set_value <- set_value
