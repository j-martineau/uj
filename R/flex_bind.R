#' @encoding UTF-8
#' @family extensions
#' @title Flexibly row bind two data frames
#' @description Checks for compatibility of variables with the same name in both variables. Adds `NA`-populated variables to each data frame as needed to allow for row binding. Variables in the resulting data.frame are sorted left to right in ascending order of variable name.
#' @param x,y Atomic data.frames.
#' @return A data.frame.
#' @export
flex_rbind <- function(x, y) {
  Errors <- NULL
  if (!uj:::.atm_dtf(x)) {Errors <- base::c(Errors, "[x] must be an atomic data.frame (?uj::atm_dtf).")}
  if (!uj:::.atm_dtf(y)) {Errors <- base::c(Errors, "[y] must be an atomic data.frame (?uj::atm_dtf).")}
  if (uj::.DEF(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  xNames <- base::colnames(x)
  yNames <- base::colnames(y)
  SharedNames <- base::intersect(xNames, yNames)
  if (base::length(SharedNames) == 0) {uj::stopperr("No variables in [x] and [y] have the same name.", .PKG = "uj")}
  AllNames <- base::unique(base::c(xNames, yNames))
  UniqueNamesY <- AllNames[!(AllNames %in% xNames)]
  UniqueNamesX <- AllNames[!(AllNames %in% yNames)]
  for (Name in SharedNames) {if (!uj:::.compat(x[[Name]], y[[Name]])) {uj::stopperr("Variables contained in both [x] and [y] must be compatible.", .PKG = "uj")}}
  for (Name in UniqueNamesX) {
    NewCol <- tibble::tibble(y = base::rep.int(NA, base::nrow(y)))
    base::colnames(NewCol) <- Name
    y <- base::cbind(y, NewCol)
  }
  for (Name in UniqueNamesY) {
    NewCol <- tibble::tibble(x = base::rep.int(NA, base::nrow(x)))
    base::colnames(NewCol) <- Name
    x <- base::cbind(x, NewCol)
  }
  y <- y[ , AllNames]
  x <- x[ , AllNames]
  base::rbind(x, y)
}
