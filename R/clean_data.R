.atmrctdtf <- function(x) {
  if (base::is.data.frame(x)) {
    if (base::nrow(x) > 1) {
      if (base::ncol(x) > 1) {
        for (i in 1:base::ncol(x)) {if (!base::is.atomic(x[[i]])) {return(F)}}
        return(T)
      }
    }
  }
  F
}

#' @title Functions to Interactively Clean Data Frames
#' @description This family of functions clean a data frame.
#' @param x A data frame.
#' @param clear A logical scalar indicating whether to clear the console at the start of each data cleaning step.
#' @return A data frame.
#' @export
clean_data_help <- function() {utils::help("clean_data_help", package = "uj")}

#' @describeIn clean_data_help Clean a data frame by selecting variables to retain, optionally renaming variables, optionally recoding `NA` and other values, optionally coercing variables to a specific mode, and optionally dropping cases with `NA` values.
#' @export
clean_data <- function(x, clear = TRUE) {
  errs <- NULL
  if (!uj:::.atmrctdtf(x)) {errs <- base::c(errs, "[x] must have 2+ rows, 2+ columns, and all columns must be atomic.")}
  if (!uj::.cmp_lgl_scl(clear)) {errs <- base::c(errs, "[clear] must [TRUE] or [FALSE].")}
  if (!base::is.null(errs)) {uj::stopperr(errs)}
  x <- uj::clean_select(x, clear = clear)
  x <- uj::clean_rename(x, clear = clear)
  x <- uj::clean_recode(x, clear = clear)
  x <- uj::clean_remode(x, clear = clear)
  x <- uj::clean_na(x, clear = clear)
  x
}

#' @describeIn clean_data_help Select variables from a data frame to be retained.
#' @export
clean_select <- function(x, clear = TRUE, .log = F) {
  base::gc()
  errs <- NULL
  if (!uj:::.atmrctdtf(x)) {errs <- base::c(errs, "[x] must have 2+ rows, 2+ columns, and all columns must be atomic.")}
  if (!uj::.cmp_lgl_scl(clear)) {errs <- base::c(errs, "[clear] must [TRUE] or[ FALSE.]")}
  if (!base::is.null(errs)) {uj::stopperr(errs)}
  log <- base::attr(x, ".clean.log")
  varsToView <- base::colnames(x)
  varsToKeep <- uj::chooseN(varsToView, "What variables do you want to RETAIN?", clear = clear)
  varsToDrop <- varsToView[!(varsToView %in% varsToKeep)]
  if (!base::length(varsToDrop) == 0) {log <- base::c(log, uj::p0("Dropped variable [", varsToDrop, "]"))}
  x <- x[ , varsToKeep]
  base::attr(x, ".clean.log") <- log
  x
}

#' @describeIn clean_data_help Optionally rename variables in a data frame.
#' @export
clean_rename <- function(x, clear = TRUE) {
  base::gc()
  errs <- NULL
  if (!uj:::.atmrctdtf(x     )) {errs <- base::c(errs, "[x] must have 2+ rows, 2+ columns, and all columns must be atomic.")}
  if (!uj::.cmp_lgl_scl(clear)) {errs <- base::c(errs, "[clear] must [TRUE] or [FALSE].")}
  if (!base::is.null(errs    )) {uj::stopperr(errs)}
  oldNames <- uj::chooseN(base::colnames(x), "What variables do you want to RENAME?", none = T, clear = clear)
  nOld     <- base::length(oldNames)
  if (uj::not_BL(oldNames) & nOld != 0) {
    log <- base::attr(x, ".clean.log")
    newNames <- uj::ask_new(oldNames, clear = clear)
    allNames <- base::colnames(x)
    for (i in 1:nOld) {allNames[allNames == oldNames[i]] <- newNames[i]}
    base::colnames(x) <- allNames
    log <- base::c(log, uj::p0("Variable [", oldNames, "] renamed to [", newNames, "]."))
    base::attr(x, ".clean.log") <- log
  }
  x
}

#' @describeIn clean_data_help Optionally recode `NA` and other values of variables.
#' @export
clean_recode <- function(x, clear = TRUE) {
  base::gc()
  errs <- NULL
  if (!uj:::.atmrctdtf(x     )) {errs <- base::c(errs, "[x] must have 2+ rows, 2+ columns, and all columns must be atomic.")}
  if (!uj::.cmp_lgl_scl(clear)) {errs <- base::c(errs, "[clear] must [TRUE] or [FALSE].")}
  if (!base::is.null(errs    )) {uj::stopperr(errs)}
  varsToRecode <- uj::chooseN(base::colnames(x), "What variables do you want to RECODE?", none = T, clear = clear)
  for (var in varsToRecode) {
    log <- base::attr(x, ".clean.log")
    oldVar <- uj::av(x[ , var])
    newVar <- base::rep(NA, base::length(oldVar))
    if (base::any(base::is.na(oldVar))) {if (uj::YES("Do you want to RECODE [NA] values of the variable [", var, "]?")) {
      varNum <- base::is.numeric(oldVar)
      varLgl <- base::is.logical(oldVar)
      newVal <- uj::ask("To what value should [NA] values of [", var, "] be RECODED?")
      if      (varNum) {newVal <- base::as.numeric(newVal)}
      else if (varLgl) {newVal <- base::as.logical(newVal)}
      else             {newVal <- base::as.character(newVal)}
      newVar[base::is.na(oldVar)] <- newVal
      log <- base::c(log, uj::p0("[NA] values of variable [", var, "] recoded to [", newVal, "]."))
    }}
    oldVals <- uj::suv(oldVar)
    message <- base::paste0("What values of [ ", var, " ] do you want to KEEP UNCHANGED, if any?")
    toCopy <- uj::chooseN(oldVals, message, none = T, clear = clear)
    if (base::length(toCopy) > 0) {for (val in toCopy) {newVar[oldVar == val] <- val}}
    valsLeft <- oldVals[!(oldVals %in% toCopy)]
    if (base::length(valsLeft) > 0) {
      message <- base::paste0("What values of [ ", var, " ] do you want to DISCARD, if any?")
      toDrop <- uj::chooseN(valsLeft, message, all = T, none = T, clear = clear)
      log <- base::c(log, uj::p0("Variable [", var, "] value [", toDrop, "] dropped."))
      toRecode <- valsLeft[!(valsLeft %in% toDrop)]
      if (base::length(toRecode) > 0) {for (val in toRecode) {
        message <- base::paste0("Enter the new value for [ ", val, " ]:")
        newVal <- uj::ask(message, clear = clear)
        newVar[oldVar == val] <- newVal
        log <- base::c(log, uj::p0("Variable [", var, "] value [", val, "] recoded to [", newVal, "]."))
      }}
    }
    x[ , var] <- newVar
    base::attr(x, ".clean.log") <- log
  }
  x
}

#' @describeIn clean_data_help Optionally coerce variables in a data frame to a specific mode.
#' @export
clean_remode <- function(x, clear = TRUE) {
  base::gc()
  errs <- NULL
  if (!uj:::.atmrctdtf(x     )) {errs <- base::c(errs, "[x] must have 2+ rows, 2+ columns, and all columns must be atomic.")}
  if (!uj::.cmp_lgl_scl(clear)) {errs <- base::c(errs, "[clear] must [TRUE] or [FALSE].")}
  if (!base::is.null(errs    )) {uj::stopperr(errs)}
  toRemode <- uj::chooseN(base::colnames(x), "What variables do you want to REMODE?", none = T, clear = clear)
  allModes <- c("character", "integer", "logical", "numeric")
  log <- base::attr(x, ".clean.log")
  for (var in toRemode) {
    original <- uj::av(x[, var])
    newMode  <- uj::choose1(allModes, "What should [", var, "]'s new mode be?", clear = clear)
    if      (newMode == "character") {remoded <- base::as.character(original)}
    else if (newMode == "integer"  ) {remoded <- base::as.integer(original)}
    else if (newMode == "logical"  ) {remoded <- base::as.logical(original)}
    else                             {remoded <- base::as.numeric(original)}
    x[ , var] <- remoded
    log <- base::c(log, uj::p0("Variable [", var, "] mode coerced to [", newMode, "]."))
  }
  base::attr(x, ".clean.log") <- log
  x
}

#' @describeIn clean_data_help Optionally drop cases with `NA` values.
#' @export
clean_na <- function(x, clear = TRUE) {
  base::gc()
  errs <- NULL
  if (!uj:::.atmrctdtf(x     )) {errs <- base::c(errs, "[x] must have 2+ rows, 2+ columns, and all columns must be atomic.")}
  if (!uj::.cmp_lgl_scl(clear)) {errs <- base::c(errs, "[clear] must [TRUE] or [FALSE].")}
  if (!base::is.null(errs    )) {uj::stopperr(errs)}
  if (uj::NO("Do you want to DROP ALL INCOMPLETE CASES?", clear = clear)) {
    if (uj::YES("Do you want to DROP CASES with specific variables containing [NA] values?")) {
      dropVars <- uj::chooseN(base::colnames(x), "For which variables do you want to DROP CASES with [NA] values?", all = T, none = T, clear = clear)
    } else {dropVars <- NULL}
  } else {dropVars <- base::colnames(x)}
  if (!base::is.null(dropVars)) {
    log <- base::attr(x, ".clean.log")
    nrPre <- base::nrow(x)
    keep <- base::rep(T, nrPre)
    for (var in dropVars) {
      keep <- keep & !base::is.na(x[[var]])
      nNA <- base::length(base::which(base::is.na(x[[var]])))
      log <- base::c(log, uj::p0("Dropped [", nNA , " of ", nrPre, "] cases with [is.na(", var, ")]."))
    }
    x <- x[keep, ]
    nrPost <- base::nrow(x)
    log <- base::c(log, uj::p0("Dropped [", nrPre - nrPost, "] cases of a total of [", nrPre, "]."))
    base::attr(x, ".clean.log") <- log
  }
  x
}
