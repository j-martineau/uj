#' @encoding UTF-8
#' @family extensions
#' @title Flexibly row bind two data frames
#' @description Checks for compatibility of variables with the same name in both variables. Adds `NA`-populated variables to each data frame as needed to allow for row binding. Variables in the resulting data.frame are sorted left to right in ascending order of variable name.
#' @param X,Y Atomic data.frames.
#' @return A data.frame.
#' @export
flex_rbind <- function(X, Y) {
  Errors <- NULL
  if (!uj:::.atm_dtf(X)) {Errors <- base::c(Errors, "[X] must be an atomic data.frame (?uj::atm_dtf).")}
  if (!uj:::.atm_dtf(Y)) {Errors <- base::c(Errors, "[Y] must be an atomic data.frame (?uj::atm_dtf).")}
  if (uj::.DEF(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  xNames <- base::colnames(X)
  yNames <- base::colnames(Y)
  SharedNames <- base::intersect(xNames, yNames)
  if (base::length(SharedNames) == 0) {uj::stopperr("No variables in [X] and [Y] have the same name.", PKG = "uj")}
  AllNames <- base::sort(base::unique(base::c(xNames, yNames)))
  UniqueNamesY <- AllNames[!(AllNames %in% xNames)]
  UniqueNamesX <- AllNames[!(AllNames %in% yNames)]
  for (Name in SharedNames) {if (!uj:::.compat(X[[Name]], Y[[Name]])) {uj::stopperr("Variables contained in both [X] and [Y] must be compatible.", PKG = "uj")}}
  for (Name in UniqueNamesX) {
    NewCol <- tibble::tibble(Y = base::rep.int(NA, base::nrow(Y)))
    base::colnames(NewCol) <- Name
    Y <- base::cbind(Y, NewCol)
  }
  for (Name in UniqueNamesY) {
    NewCol <- tibble::tibble(X = base::rep.int(NA, base::nrow(X)))
    base::colnames(NewCol) <- Name
    X <- base::cbind(X, NewCol)
  }
  Y <- Y[ , AllNames]
  X <- X[ , AllNames]
  base::rbind(X, Y)
}
