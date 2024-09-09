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
  errs <- NULL                                                                                                           # init error bank
  if (!uj::.atmrctdtf(x)) {errs <- base::c(errs, "[x] must have 2+ rows, 2+ columns, and all columns must be atomic.")} # IF [x] does not qualify as data: add to the error bank
  if (!uj::.cmp_lgl_scl(clear)) {errs <- base::c(errs, "[clear] must [TRUE] or [FALSE].")}                               # IF [clear] is not a logical scalar: add to the error bank
  if (!base::is.null(errs)) {uj::stopperr(errs)}                                                                         # IF there are any errors THEN throw a combined error
  x <- uj::clean_select(x, clear = clear)                                                                                # select variables
  x <- uj::clean_rename(x, clear = clear)                                                                                # rename variables
  x <- uj::clean_recode(x, clear = clear)                                                                                # recode variables
  x <- uj::clean_remode(x, clear = clear)                                                                                # remode variables
  x <- uj::clean_na(x, clear = clear)                                                                                    # remove cases
  x
}

#' @describeIn clean_data_help Select variables from a data frame to be retained.
#' @export
clean_select <- function(x, clear = TRUE, .log = F) {
  base::gc()                                                                                                             # garbage collection
  errs <- NULL                                                                                                           # init error bank
  if (!uj::.atmrctdtf(x)) {errs <- base::c(errs, "[x] must have 2+ rows, 2+ columns, and all columns must be atomic.")}  # IF [x] does not qualify as data: add to the error bank
  if (!uj::.cmp_lgl_scl(clear)) {errs <- base::c(errs, "[clear] must [TRUE] or[ FALSE.]")}                               # IF [clear] is not a logical scalar: add to the error bank
  if (!base::is.null(errs)) {uj::stopperr(errs)}                                                                         # IF there are any errors THEN throw a combined error
  varsToView <- base::colnames(x)                                                                                        # get names of all variables in [x]
  uj::say("The following is the list of all variables in the data:\n\n", lev = 1, clear = clear)                         # notify user of variables in their data
  uj::say(base::paste0(varsToView, collapse = ", "))                                                                     #   ...
  uj::say("\n\n")                                                                                                        #   ...
  opts <- base::c("I want to KEEP ALL variables in my data",                                                             # user options
                  "I want to KEEP fewer variables than I want to DROP",                                                  #   ...
                  "I want to DROP fewer variables than I want to KEEP")                                                  #   ...
  choice <- uj::choose1(opts, "Which is true of your data?", clear = F)                                                  # ask user to choose from the options
  if (choice != "I want to KEEP ALL variables in my data") {                                                             # IF choice is NOT to keep all variables
    log <- base::attr(x, ".clean.log")                                                                                   # : get any data cleaning log entries
    if (choice == "I want to KEEP fewer variables than I want to DROP") {                                                # : IF the choice is to drop more variables than to keep
      varsToKeep <- uj::chooseN(varsToView, "What variables do you want to KEEP?", all = F, clear = clear)               # : : choose variables of [x] to keep
      varsToDrop <- varsToView[!(varsToView %in% varsToKeep)]                                                            # : : identify variables of [x] to be dropped
    } else {                                                                                                             # : ELSE (keep more variables than drop)
      varsToDrop <- uj::chooseN(varsToView, "What variables do you want to DROP?", all = F, clear = clear)               # : : choose variables of [x] to drop
      varsToKeep <- varsToView[!(varsToView %in% varsToDrop)]                                                            # : : identify variables of [x] to be kept
    }                                                                                                                    # : END IF
    log <- base::c(log, uj::p0("Dropped variable [", varsToDrop, "]"))                                                   # : add dropped variables to the cleaning log
    x <- x[ , varsToKeep, drop = F]                                                                                      # : keep only the selected variables
    base::attr(x, ".clean.log") <- log                                                                                   # : store the cleaning log
  }                                                                                                                      # END IF
  x                                                                                                                      # return the result
}

#' @describeIn clean_data_help Optionally rename variables in a data frame.
#' @export
clean_rename <- function(x, clear = TRUE) {
  base::gc()                                                                                                             # garbage collection
  errs <- NULL                                                                                                           # init error bank
  if (!uj::.atmrctdtf(x)) {errs <- base::c(errs, "[x] must have 2+ rows, 2+ columns, and all columns must be atomic.")}  # IF [x] does not qualify as data: add to the error bank
  if (!uj::.cmp_lgl_scl(clear)) {errs <- base::c(errs, "[clear] must [TRUE] or[ FALSE.]")}                               # IF [clear] is not a logical scalar: add to the error bank
  if (!base::is.null(errs)) {uj::stopperr(errs)}                                                                         # IF there are any errors THEN throw a combined error
  oldNames <- uj::chooseN(base::colnames(x), "What variables do you want to RENAME?", none = T, clear = clear)           # user selects variables to be renamed
  nOld     <- base::length(oldNames)                                                                                     # get the number of variables to be renamed
  if (uj::not_BL(oldNames) & nOld != 0) {                                                                                # IF old names is not a blank string and the number to be renamed is non-zero THEN
    log <- base::attr(x, ".clean.log")                                                                                   # : get the data cleaning log
    newNames <- uj::ask_new(oldNames, clear = clear)                                                                     # : user inputs new variable
    allNames <- base::colnames(x)                                                                                        # : get all variable names
    for (i in 1:nOld) {allNames[allNames == oldNames[i]] <- newNames[i]}                                                 # : FOR each variable to be renamed: replace that name with the new one
    base::colnames(x) <- allNames                                                                                        # : set column names with renamed variables
    log <- base::c(log, uj::p0("Variable [", oldNames, "] renamed to [", newNames, "]."))                                # : add variable renaming to the cleaning log
    base::attr(x, ".clean.log") <- log                                                                                   # : update the cleaning log
  }                                                                                                                      # END IF
  x                                                                                                                      # return the result
}

#' @describeIn clean_data_help Optionally recode `NA` and other values of variables.
#' @export
clean_recode <- function(x, clear = TRUE) {
  base::gc()                                                                                                             # garbage collection
  errs <- NULL                                                                                                           # init error bank
  if (!uj::.atmrctdtf(x)) {errs <- base::c(errs, "[x] must have 2+ rows, 2+ columns, and all columns must be atomic.")}  # IF [x] does not qualify as data: add to the error bank
  if (!uj::.cmp_lgl_scl(clear)) {errs <- base::c(errs, "[clear] must [TRUE] or[ FALSE.]")}                               # IF [clear] is not a logical scalar: add to the error bank
  if (!base::is.null(errs)) {uj::stopperr(errs)}                                                                         # IF there are any errors THEN throw a combined error
  varsToRecode <- uj::chooseN(base::colnames(x), "What variables do you want to RECODE?", none = T, clear = clear)       # user selects the variables to be recoded
  for (var in varsToRecode) {                                                                                            # FOR each variable to be recoded
    log <- base::attr(x, ".clean.log")                                                                                   # : get the data cleaning log
    oldVar <- uj::av(x[ , var])                                                                                          # : get the original variable values
    newVar <- base::rep(NA, base::length(oldVar))                                                                        # : init the new variable values
    naRecoded <- F                                                                                                       # : NA values have not been recoded
    if (base::any(base::is.na(oldVar))) {                                                                                # : IF there are any original [NA] values
      if (uj::YES("Do you want to RECODE [NA] values of the variable [", var, "]?")) {                                   # : : IF the user wants to recode [NA] values
        varNum <- base::is.numeric(oldVar)                                                                               # : : : whether the original is numeric
        varLgl <- base::is.logical(oldVar)                                                                               # : : : whether the original variable is logical
        newVal <- uj::ask("To what value should [NA] values of [", var, "] be RECODED?")                                 # : : : ask the user what value to replace [NA]s with
        if      (varNum) {newVal <- base::as.numeric(newVal)}                                                            # : : : IF numeric THEN coerce user answer to numeric
        else if (varLgl) {newVal <- base::as.logical(newVal)}                                                            # : : : ELSE IF logical THEN coerce user answer to logical
        else             {newVal <- base::as.character(newVal)}                                                          # : : : ELSE coerce user answer to character
        newVar[base::is.na(oldVar)] <- newVal                                                                            # : : : store user answer in corresponding locations of new variable
        log <- base::c(log, uj::p0("[NA] values of variable [", var, "] recoded to [", newVal, "]."))                    # : : : log the recoding of [NA] values
        naRecoded <- T                                                                                                   # : : : NA values have been recoded
      }                                                                                                                  # : : END IF
    }                                                                                                                    # : END IF
    oldVals <- uj::suv(oldVar)                                                                                           # : get sorted unique original values
    if (naRecoded) {oldVals <- oldVals[!base::is.na(oldVals)]}                                                           # : IF [NA] values have been recoded THEN remove NA from sorted unique original values
    message <- base::paste0("What values of [ ", var, " ] do you want to KEEP UNCHANGED, if any?")                       # : build message to user
    toCopy <- uj::chooseN(oldVals, message, none = T, clear = clear)                                                     # : user selects original values to be retained
    if (base::length(toCopy) > 0) {for (val in toCopy) {newVar[oldVar == val] <- val}}                                   # : IF there are any THEN copy them over
    valsLeft <- oldVals[!(oldVals %in% toCopy)]                                                                          # : get the values still not addressed
    keep <- base::rep(T, base::nrow(x))                                                                                  # : initialize indexing vector of cases to be kept
    if (base::length(valsLeft) > 0) {                                                                                    # : IF there are any values still not addressed
      message <- base::paste0("What values of [ ", var, " ] do you want to DISCARD, if any?")                            # : : build user message
      toDrop <- uj::chooseN(valsLeft, message, all = T, none = T, clear = clear)                                         # : : ask user what values to discard, if any
      if (base::length(toDrop) > 0) {                                                                                    # : : IF there are any values to drop
        message <- "How should discarded values be handled?"                                                             # : : : message to user
        opts <- base::c("Cases should be discarded", "Values should be recoded as [NA]")                                 # : : : options for handling discarded values
        ans <- uj::choose1(opts, message, clear = clear)                                                                 # : : : ask user how to handle discarded values
        if (ans == "Values should be recoded as [NA]") {                                                                 # : : : IF discarded values should be recoded to [NA]
          drop2NA <- T                                                                                                   # : : : : indicate discarded values are to be recoded to [NA]
          for (drop in toDrop) {newVar[oldVar = drop] <- NA}                                                             # : : : : FOR each discarded value: recode to NA
          log <- base::c(log, uj::p0("Variable [", var, "] value [", toDrop, "] recoded as [NA]."))                      # : : : : log dropped values
        } else {drop2NA <- F}                                                                                            # : : : ELSE discarded values become dropped cases
      } else {drop2NA <- T}                                                                                              # : : END IF
      toRecode <- valsLeft[!(valsLeft %in% toDrop)]                                                                      # : : values to be recoded are the remaining values
      if (drop2NA) {toDrop <- NULL}                                                                                      # : : if values to drop have already been recoded, they're taken care of
      if (base::length(toRecode) > 0) {                                                                                  # : : IF there are any values to recode
        for (val in toRecode) {                                                                                          # : : : FOR each value to be recoded
          message <- base::paste0("Enter the new value for [ ", val, " ]:")                                              # : : : : build user message
          newVal <- uj::ask(message, clear = clear)                                                                      # : : : : ask user for replacement value
          newVar[oldVar == val] <- newVal                                                                                # : : : : recode the value
          log <- base::c(log, uj::p0("Variable [", var, "] value [", val, "] recoded to [", newVal, "]."))               # : : : : log the recoding
        }                                                                                                                # : : : END FOR
      }                                                                                                                  # : : END IF
      if (!base::is.null(toDrop)) {                                                                                      # : : IF there are any cases to be dropped
        for (drop in toDrop) {keep <- keep & oldVar != toDrop}                                                           # : : : FOR each value whose cases are to be dropped: index them to be dropped
        keep[base::is.na(keep)] <- T                                                                                     # : : : any remaining [NA] values should be kept (they have not been recoded or dropped)
      }                                                                                                                  # : : END IF
    }                                                                                                                    # : END IF
    x[ , var] <- newVar                                                                                                  # : store new values
    x <- x[keep, ]                                                                                                       # : keep only the values indexed for keeping
    base::attr(x, ".clean.log") <- log                                                                                   # : store the cleaning log
  }                                                                                                                      # END FOR
  x                                                                                                                      # return the result
}

#' @describeIn clean_data_help Optionally coerce variables in a data frame to a specific mode.
#' @export
clean_remode <- function(x, clear = TRUE) {
  base::gc()                                                                                                             # garbage collection
  errs <- NULL                                                                                                           # init error bank
  if (!uj::.atmrctdtf(x)) {errs <- base::c(errs, "[x] must have 2+ rows, 2+ columns, and all columns must be atomic.")}  # IF [x] does not qualify as data: add to the error bank
  if (!uj::.cmp_lgl_scl(clear)) {errs <- base::c(errs, "[clear] must [TRUE] or[ FALSE.]")}                               # IF [clear] is not a logical scalar: add to the error bank
  if (!base::is.null(errs)) {uj::stopperr(errs)}                                                                         # IF there are any errors THEN throw a combined error
  toRemode <- uj::chooseN(base::colnames(x), "What variables do you want to REMODE?", none = T, clear = clear)           # ask the user what variables they want to remode
  allModes <- c("character", "integer", "logical", "numeric")                                                            # the complete set of modes for this purpose
  log <- base::attr(x, ".clean.log")                                                                                     # get the cleaning log
  for (var in toRemode) {                                                                                                # FOR each variable to remode
    original <- uj::av(x[, var])                                                                                         # : get the original values
    newMode  <- uj::choose1(allModes, "What should [", var, "]'s new mode be?", clear = clear)                           # : ask user what the new mode should be
    if      (newMode == "character") {remoded <- base::as.character(original)}                                           # : IF      character THEN coerce to character
    else if (newMode == "integer"  ) {remoded <- base::as.integer(original)}                                             # : ELSE IF integer   THEN coerce to integer
    else if (newMode == "logical"  ) {remoded <- base::as.logical(original)}                                             # : ELSE IF logical   THEN coerce to logical
    else                             {remoded <- base::as.numeric(original)}                                             # : ELSE                   coerce to numeric
    x[ , var] <- remoded                                                                                                 # : store the remoded variable
    log <- base::c(log, uj::p0("Variable [", var, "] mode coerced to [", newMode, "]."))                                 # : log the remoding
  }                                                                                                                      # END FOR
  base::attr(x, ".clean.log") <- log                                                                                     # store the updated cleaning log
  x                                                                                                                      # return the result
}

#' @describeIn clean_data_help Optionally drop cases with `NA` values.
#' @export
clean_na <- function(x, clear = TRUE) {
  base::gc()                                                                                                             # garbage collection
  errs <- NULL                                                                                                           # init error bank
  if (!uj::.atmrctdtf(x)) {errs <- base::c(errs, "[x] must have 2+ rows, 2+ columns, and all columns must be atomic.")}  # IF [x] does not qualify as data: add to the error bank
  if (!uj::.cmp_lgl_scl(clear)) {errs <- base::c(errs, "[clear] must [TRUE] or[ FALSE.]")}                               # IF [clear] is not a logical scalar: add to the error bank
  if (!base::is.null(errs)) {uj::stopperr(errs)}                                                                         # IF there are any errors THEN throw a combined error
  log <- base::attr(x, ".clean.log")                                                                                     # get the data cleaning log
  nr  <- base::nrow(x)                                                                                                   # count the total number of cases
  if (uj::YES("Do you want to DROP ALL INCOMPLETE CASES?", clear = clear)) {                                             # IF the user wants to drop all incomplete cases
    i   <- stats::complete.cases(x)                                                                                      # : index the complete cases
    n   <- base::length(base::which(!i))                                                                                 # : count the incomplete cases
    if (n > 0) {                                                                                                         # : IF there are any incomplete cases
      x   <- x[i, ]                                                                                                      # : : subset just the complete cases
      log <- base::c(log, uj::p0("Dropped all [", n, "] incomplete cases of [", nr, "] total cases."))                   # : : update the data cleaning log
    }                                                                                                                    # : END IF
  } else if (uj::YES("Do you want to DROP CASES with specific variables containing [NA] values?", clear = clear)) {      # ELSE IF the user wants to drop cases just for specific variables
    dropNA <- uj::chooseN(base::colnames(x), "For which variables do you want to DROP CASES with [NA] values?",          # : ask the user what variables they want to drop NA cases for
                          all = F, none = T, clear = clear)                                                              # :   ...
    if (!base::is.null(dropNA)) {                                                                                        # : IF there are any variables for which to drop NA cases
      tmp <- x[ , dropNA, drop = F]                                                                                      # : : subset just those variables
      i   <- stats::complete.cases(tmp)                                                                                  # : : index that subset's complete cases
      n   <- base::length(base::which(!i))                                                                               # : : count the cases to be dropped
      for (var in dropNA) {                                                                                              # : : FOR each variable for which to drop NA cases
        nn <- base::length(base::which(base::is.na(x[[var]])))                                                           # : : : count the number of NA cases
        log <- base::c(log, uj::p0("Dropped [", nn , "] cases with is.na(", var, ")."))                                  # : : : update the cleaning log
      }                                                                                                                  # : : END FOR
      x <- x[i, ]                                                                                                        # : : keep just the complete cases
      log <- base::c(log, uj::p0("Dropped [", n, "] cases of [", nr, "] total cases."))                                  # : : update the cleaning log with the overall number dropped
    }                                                                                                                    # : END IF
  }                                                                                                                      # END IF
  base::attr(x, ".clean.log") <- log                                                                                     # store the updated cleaning log
  x                                                                                                                      # return the result
}
