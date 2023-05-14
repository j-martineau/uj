#' @title Functions to Interactively Clean Data Frames
#' @description This family of functions clean a data frame as follows:
#' \tabular{ll}{  `clean_remode`        \tab Change variable modes.                      \cr
#'                `clean_recode`        \tab Recode variables.                           \cr
#'                `clean_rename`        \tab Rename variables.                           \cr
#'                `clean_select`        \tab Select variables to retain.                 \cr
#'                `clean_data`          \tab Select, rename, recode, and remode variables. }
#' @param X A data frame.
#' @param .clear A logical scalar indicating whether to clear the console at the start of each data cleaning step.
#' @return A data frame.
#' @export
clean_data <- function(X, .clear = TRUE) {
  base::gc()
  if (!uj::.atm_rct_dtf(X)) {uj::bankerr("[X] must have 2+ rows, 2+ columns, and all columns must be atomic.")}
  uj::check_tf(.clear = .clear)
  uj::checkerr(PKG = "uj")
  X <- uj::clean_select(X, .clear = .clear)
  X <- uj::clean_rename(X, .clear = .clear)
  X <- uj::clean_recode(X, .clear = .clear)
  uj::clean_remode(X)
}

#' @rdname clean_data
#' @export
clean_select <- function(X, .clear = TRUE) {
  base::gc()
  if (!uj::.atm_rct_dtf(X)) {uj::bankerr("[X] must have 2+ rows, 2+ columns, and all columns must be atomic.")}
  uj::check_tf(.clear = .clear)
  uj::checkerr(PKG = "uj")
  KeepVars <- uj::chooseN(uj::cn(X), "What variables do you want to retain?", .clear = .clear)
  X[ , KeepVars]
}

#' @rdname clean_data
#' @export
clean_rename <- function(X, .clear = TRUE) {
  base::gc()
  if (!uj::.atm_rct_dtf(X)) {uj::bankerr("[X] must have 2+ rows, 2+ columns, and all columns must be atomic.")}
  uj::check_tf(.clear = .clear)
  uj::checkerr(PKG = "uj")
  OldNames <- uj::chooseN(uj::cn(X), "What variables do you want to rename?", None = T, .clear = .clear)
  nOld <- uj::N(OldNames)
  if (uj::not_BL(OldNames) & nOld != 0) {
    NewNames <- uj::ask_new(OldNames, .clear = .clear)
    AllNames <- uj::cn(X)
    for (i in 1:nOld) {AllNames[AllNames == OldNames[i]] <- NewNames[i]}
    X <- uj::name_cols(X, AllNames)
  }
  X
}

#' @rdname clean_data
#' @export
clean_recode <- function(X, .clear = TRUE) {
  base::gc()
  if (!uj::.atm_rct_dtf(X)) {uj::bankerr("[Data] must have 2+ rows, 2+ columns, and all columns must be atomic.")}
  uj::check_tf(.clear = .clear)
  uj::checkerr(PKG = "uj")
  VarsToRecode <- uj::chooseN(uj::cn(X), "What variables do you want to recode?", None = T, .clear = .clear)
  for (VarToRecode in VarsToRecode) {
    OriginalVar <- uj::av(X[ , VarToRecode])
    AllUniqueVals <- uj::suv(OriginalVar)
    RecodedVar <- uj::r(uj::N(OriginalVar), NA)
    Message <- uj::p0("What values of [", VarToRecode, "] do you want to keep, if any?")
    ValsToKeep <- uj::chooseN(AllUniqueVals, Message, None = T, .clear = .clear)
    AvailableVals <- AllUniqueVals[uj::not_in(AllUniqueVals, ValsToKeep)]
    if (uj::n1p(AvailableVals)) {
      Message <- uj::p0("What values of [", VarToRecode, "] do you want to recode?")
      ValsToRecode <- uj::chooseN(AvailableVals, Message, None = uj::DEF(ValsToKeep), .clear = .clear)
    } else {ValsToRecode <- NULL}
    if (uj::n1p(ValsToRecode)) {
      RecodedVals <- uj::ask_new(ValsToRecode, Unq = F, .clear = .clear)
      if (uj::n1p(ValsToRecode)) {for (i in 1:uj::N(ValsToRecode)) {
        OldVal <- ValsToRecode[i]
        NewVal <- RecodedVals[i]
        RecodedVar[OriginalVar == OldVal] <- NewVal
      }}}
    if (uj::n1p(ValsToKeep)) {for (i in 1:uj::N(ValsToKeep)) {
      KeepVal <- ValsToKeep[i]
      RecodedVar[OriginalVar == KeepVal] <- KeepVal}
    }
    X[ , VarToRecode] <- RecodedVar
    X <- X[uj::ok(RecodedVar), ]
  }
  X
}

#' @rdname clean_data
#' @export
clean_remode <- function(X, .clear = TRUE) {
  base::gc()
  if (!uj::.atm_rct_dtf(X)) {uj::bankerr("[X] must have 2+ rows, 2+ columns, and all columns must be atomic.")}
  uj::check_tf(.clear = .clear)
  uj::checkerr(PKG = "uj")
  VarsToRemode <- uj::chooseN(uj::cn(X), "What variables do you want to re-MODE?", None = T, .clear = .clear)
  AllModes <- c("character", "integer", "logical", "numeric")
  for (VarToRemode in VarsToRemode) {
    OriginalVar <- uj::av(X[, VarToRemode])
    NewMode <- uj::choose1(AllModes, "What should [", VarToRemode, "]'s new mode be?", .clear = .clear)
    if (NewMode == "character") {RemodedVar <- uj::as_chr(OriginalVar)}
    else if (NewMode == "integer") {RemodedVar <- uj::as_int(OriginalVar)}
    else if (NewMode == "logical") {RemodedVar <- uj::as_lgl(OriginalVar)}
    else {RemodedVar <- uj::as_num(OriginalVar)}
    X[ , VarToRemode] <- RemodedVar
  }
  X
}
