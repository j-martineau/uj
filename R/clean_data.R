#' @title Functions to Interactively Clean Data Frames
#' @description This family of functions clean a data frame as follows:
#' \tabular{ll}{  `clean_remode`        \tab Change variable modes.                      \cr
#'                `clean_recode`        \tab Recode variables.                           \cr
#'                `clean_rename`        \tab Rename variables.                           \cr
#'                `clean_select`        \tab Select variables to retain.                 \cr
#'                `clean_data`          \tab Select, rename, recode, and remode variables. }
#' @param x A data frame.
#' @param .CLEAR A logical scalar indicating whether to CLEAR the console at the start of each data cleaning step.
#' @return A data frame.
#' @export
clean_data <- function(x, .CLEAR = TRUE) {
  base::gc()
  Errors <- NULL
  if (!uj::.atm_rct_dtf(x)) {Errors <- base::c(Errors, "[x] must have 2+ rows, 2+ columns, and all columns must be atomic.")}
  if (!uj::.cmp_lgl_scl(.CLEAR)) {Errors <- base::c(Errors, "[.CLEAR] must TRUE or FALSE.")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  x <- uj::clean_select(x, .CLEAR = .CLEAR)
  x <- uj::clean_rename(x, .CLEAR = .CLEAR)
  x <- uj::clean_recode(x, .CLEAR = .CLEAR)
  uj::clean_remode(x)
}

#' @rdname clean_data
#' @export
clean_select <- function(x, .CLEAR = TRUE) {
  base::gc()
  Errors <- NULL
  if (!uj::.atm_rct_dtf(x)) {Errors <- base::c(Errors, "[x] must have 2+ rows, 2+ columns, and all columns must be atomic.")}
  if (!uj::.cmp_lgl_scl(.CLEAR)) {Errors <- base::c(Errors, "[.CLEAR] must TRUE or FALSE.")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  KeepVars <- uj::chooseN(uj::cn(x), "What variables do you want to retain?", .CLEAR = .CLEAR)
  x[ , KeepVars]
}

#' @rdname clean_data
#' @export
clean_rename <- function(x, .CLEAR = TRUE) {
  base::gc()
  Errors <- NULL
  if (!uj::.atm_rct_dtf(x)) {Errors <- base::c(Errors, "[x] must have 2+ rows, 2+ columns, and all columns must be atomic.")}
  if (!uj::.cmp_lgl_scl(.CLEAR)) {Errors <- base::c(Errors, "[.CLEAR] must TRUE or FALSE.")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  OldNames <- uj::chooseN(uj::cn(x), "What variables do you want to rename?", .NONE = T, .CLEAR = .CLEAR)
  nOld <- uj::N(OldNames)
  if (uj::not_BL(OldNames) & nOld != 0) {
    NewNames <- uj::ask_new(OldNames, .CLEAR = .CLEAR)
    AllNames <- uj::cn(x)
    for (i in 1:nOld) {AllNames[AllNames == OldNames[i]] <- NewNames[i]}
    x <- uj::name_cols(x, AllNames)
  }
  x
}

#' @rdname clean_data
#' @export
clean_recode <- function(x, .CLEAR = TRUE) {
  base::gc()
  Errors <- NULL
  if (!uj::.atm_rct_dtf(x)) {Errors <- base::c(Errors, "[x] must have 2+ rows, 2+ columns, and all columns must be atomic.")}
  if (!uj::.cmp_lgl_scl(.CLEAR)) {Errors <- base::c(Errors, "[.CLEAR] must TRUE or FALSE.")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  VarsToRecode <- uj::chooseN(uj::cn(x), "What variables do you want to recode?", .NONE = T, .CLEAR = .CLEAR)
  for (VarToRecode in VarsToRecode) {
    OriginalVar <- uj::av(x[ , VarToRecode])
    AllUniqueVals <- uj::suv(OriginalVar)
    RecodedVar <- uj::r(uj::N(OriginalVar), NA)
    Message <- uj::p0("What values of [", VarToRecode, "] do you want to keep, if any?")
    ValsToKeep <- uj::chooseN(AllUniqueVals, Message, .NONE = T, .CLEAR = .CLEAR)
    AvailableVals <- AllUniqueVals[uj::not_in(AllUniqueVals, ValsToKeep)]
    if (uj::n1p(AvailableVals)) {
      Message <- uj::p0("What values of [", VarToRecode, "] do you want to recode?")
      ValsToRecode <- uj::chooseN(AvailableVals, Message, .NONE = uj::DEF(ValsToKeep), .CLEAR = .CLEAR)
    } else {ValsToRecode <- NULL}
    if (uj::n1p(ValsToRecode)) {
      RecodedVals <- uj::ask_new(ValsToRecode, .U = F, .CLEAR = .CLEAR)
      if (uj::n1p(ValsToRecode)) {for (i in 1:uj::N(ValsToRecode)) {
        OldVal <- ValsToRecode[i]
        NewVal <- RecodedVals[i]
        RecodedVar[OriginalVar == OldVal] <- NewVal
      }}}
    if (uj::n1p(ValsToKeep)) {for (i in 1:uj::N(ValsToKeep)) {
      KeepVal <- ValsToKeep[i]
      RecodedVar[OriginalVar == KeepVal] <- KeepVal}
    }
    x[ , VarToRecode] <- RecodedVar
    x <- x[uj::ok(RecodedVar), ]
  }
  x
}

#' @rdname clean_data
#' @export
clean_remode <- function(x, .CLEAR = TRUE) {
  base::gc()
  Errors <- NULL
  if (!uj::.atm_rct_dtf(x)) {Errors <- base::c(Errors, "[x] must have 2+ rows, 2+ columns, and all columns must be atomic.")}
  if (!uj::.cmp_lgl_scl(.CLEAR)) {Errors <- base::c(Errors, "[.CLEAR] must TRUE or FALSE.")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  VarsToRemode <- uj::chooseN(uj::cn(x), "What variables do you want to re-MODE?", .NONE = T, .CLEAR = .CLEAR)
  AllModes <- c("character", "integer", "logical", "numeric")
  for (VarToRemode in VarsToRemode) {
    OriginalVar <- uj::av(x[, VarToRemode])
    NewMode <- uj::choose1(AllModes, "What should [", VarToRemode, "]'s new mode be?", .CLEAR = .CLEAR)
    if (NewMode == "character") {RemodedVar <- uj::as_chr(OriginalVar)}
    else if (NewMode == "integer") {RemodedVar <- uj::as_int(OriginalVar)}
    else if (NewMode == "logical") {RemodedVar <- uj::as_lgl(OriginalVar)}
    else {RemodedVar <- uj::as_num(OriginalVar)}
    x[ , VarToRemode] <- RemodedVar
  }
  x
}
