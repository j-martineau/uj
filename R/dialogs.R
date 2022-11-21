#' @name dialogs
#' @family dialogs
#' @title Dialog boxes using package \code{svDialogs}.
#' @description Launch a dialog box
#' @param x \link[=cmp_chr_scl]{Complete character scalar} message.
#' @param d \link[=cmp_chr_scl]{Complete character scalar} default directory.
#' @param t \link[=cmp_chr_scl]{Complete character scalar} type of dialog box
#'   (valid values are \code{c('ok', 'okcancel', 'yesno', 'yesnocancel'}).
#' @param def \link[=cmp_chr_scl]{Complete character scalar} default value.
#' @param type, \link[=cmp_chr_scl]{Complete character scalar} type of directory
#'   or document to select
#' @param ... Objects to be atomized to create the question.
#' @param cancel \link[=cmp_chr_scl]{Complete character scalar} value to return
#'   if user clicks the \code{cancel} button The value \code{cancel = '.stop'}
#'   is an indicator that an error should be thrown if the user clicks the
#'   \code{cancel} button.
#' @param opts \link[=cmp_chr_vec]{Complete character vector} of options to
#'   choose from.
#' @param what \link[=cmp_chr_scl]{Complete character scalar} describing type of
#'   option being presented.
#' @param more \link[cmp_lgl_scl]{Complete logical scalar} indicating whether
#'   there are more options than could be presented in a single dialog box that
#'   still could be presented.
#' @param all,none \link[=cmp_lgl_scl]{Complete logical scalars} indicating
#'   whether the user should be able to select all options or none of the
#'   options, respectively.
#' @param per \link[=cmp_psw_scl]{Complete positive whole-number scalar}
#'   indicating the maximum number of options to present in a single dialog box
#'   interaction.
#' @param unq \link[=cmp_lgl_scl]{Complete logical scalar} indicating whether
#'   new values must be unique.
#' @return \link[=cmp_chr_scl]{Complete character scalar}
#' @export
dialogs <- ""

#' @describeIn dialogs Collapse \code{...} into a character scalar message and
#'   launch a simple dialog box to give the message.
#' @export
msgbox <- function(..., t = "ok") {
  msg <- paste0(av(...), collapse = "")
  vals <- c("ok", "okcancel", "yesno", "yesnocancel")
  errs <- c(f0(idef(msg)     , NULL, "\n \u2022 No user update message was provided in [...]."),
            f0(cmp_chr_scl(t), NULL, "\n \u2022 [t] must be a character scalar in c('ok', 'okcancel', 'yesno', 'yesnocancel')."),
            f0(isIN(t, vals) , NULL, "\n \u2022 [t] must be a character scalar in c('ok', 'okcancel', 'yesno', 'yesnocancel')."))
  if (idef(errs)) {stop(errs)}
  ans <- svDialogs::dlg_message(msg, type = t)
  if (t == "ok") {NULL} else {ans}
}

#' @describeIn dialogs Collapse {...} into a character scalar prompt. Ask user
#'   to select a directory/folder. Fail safe feature launches a message dialog
#'   box prompting the use to take an action in the next dialog box After user
#'   acknowledges, launches the directory/folder selection dialog to avoid
#'   problems with the prompt not showing up on all operating systems.
#' @export
dirbox <- function(..., d = getwd()) {
  msg <- paste0(av(...), collapse = "")
  d <- paste0(av(d), collapse = "")
  if (inll(msg)) {msg <- "choose a directory/folder"}
  errs <- c(f0(cmp_chr_scl(msg), NULL, "\n \u2022 No message to the user was provided in [...]."),
            f0(cmp_chr_scl(d)  , NULL, "\n \u2022 No default directory provided in [d]."),
            f0(dir.exists(d)   , NULL, da0("\n \u2022 [d = '", d, "'] is not an existing directory/folder.")))
  if (idef(errs)) {stop(errs)}
  msgbox(da0("In the next dialog box, ", msg)); svDialogs::dlg_dir(default = d, title = msg)
}

#' @describeIn dialogs Collapse \code{...} into a character scalar prompt. Ask
#'   user to select a document Fail safe feature launches a message dialog box
#'   prompting the use to take an action in the next dialog box After user
#'   acknowledges, launches the file selection dialog to avoid problems with the
#'   prompt not showing up on all operating systems.
#' @export
docbox <- function(...) {
  msg <- paste0(av(...), collapse = "")
  if (inll(msg)) {msg <- "choose a document"}
  if (!cmp_chr_scl(msg)) {stop("\n \u2022 [...] must be empty or must be collapsible to a character scalar.")}
  msgbox(da0("In the next dialog box, ", msg)); svDialogs::dlg_open(title = msg)
}

#' @describeIn dialogs Ask user to input a text response.
#' @export
ansbox <- function(x = "Enter a value", def = "") {svDialogs::dlg_input(message = x, default = def)}

#' @describeIn dialogs Does user select the 'no' button (rather than 'yes') in
#'   response to the prompt?
#' @export
no <- function(...) {msgbox(paste0(av(...), collapse = ""), t = "yesno")$res != "yes"}

#' @describeIn dialogs Does user select the 'yes' button (rather than 'no') in
#'   response to the prompt?
#' @export
yes <- function(...) {msgbox(paste0(av(...), collapse = ""), t = "yesno")$res == "yes"}

#' @describeIn dialogs Display a message in a dialog box, returning
#'   the value of the button clicked.
#' @export
msg <- function(x, t = "ok") {msgbox(x, t = t)$res}

#' @describeIn dialogs Does user select the 'ok' (rather than 'cancel') button
#'   in response to the prompt?
#' @export
okx <- function(x) {x <- msg(x, "okcancel"); if (x != "ok") {stop("canceled")}}

#' @describeIn dialogs Ask user to select a directory.
#' @export
choose_dir <- function(type = "file") {dirbox(okx(da1("Select a", type, "directory")))$res}

#' @describeIn dialogs Ask user to select a document
#' @export
choose_doc <- function(type = "document") {docbox(okx(da1("Select a", type, "file")))$res}

#' @describeIn dialogs Ask the user for typed input to be returned without
#'   further processing.
#' @export
ask <- function(..., def = "", cancel = ".stop") {
  ends <- r(2, "\n")                                                              # pad front and back with three newlines to make all content appear inside the dialog box
  answer <- ansbox(x = paste0(av(ends, ..., ends), collapse = ""), def = def)$res # build the question and ask the user the question
  if (n0(answer) & cancel == ".stop") {stop("canceled by user")}                  # IF no answer should throw error and there is no answer > throw error
  else if (n0(answer)) {cancel}                                                   # BUT IF there is no answer > return the value specified for canceling
  else {answer}                                                                   # ELSE return the answer
}

#' @describeIn dialogs Ask user to select a single option from among a list of
#'   possible options.
#' @export
ask1 <- function(opts, what, more = FALSE) {
  suffix <- f0(more, "\n(more options may be presented next)", NULL)             # suffix if more options are available
  nums <- as_chr(1:nx(opts))                                                     # valid responses
  list <- paste(nums, "=", opts)                                                 # list of options with preceding numbers separated by newlines
  list <- f0(inll(suffix), list, paste0(list, "\n\nnone = none of these options", collapse = ""))
  prompt <- paste0("Enter the number of one ", what, " from these options:\n\n", collapse = "")
  prompt <- paste0(prompt, "\n", list, "\n", suffix)                             # build the question separated by newlines
  answer <- ask(prompt, cancel = ".stop")                                        # ask the question
  if (answer %in% as_chr(nums)) {opts[as_int(answer)]}                           # IF answer is the number of an option, return that option
  else if (ipop(suffix) & answer == "none") {return(NULL)}                       # BUT IF there is a suffix and selection is none, return NULL
  else {stop("invalid selection")}                                               # ELSE error
}

#' @describeIn dialogs Ask user to select a one or more options from among a
#'   list of possible options.
#' @export
askn <- function(opts, what, all = TRUE, none = TRUE, more = FALSE) {
  suffix <- f0(more, "\n(more options may be presented next)", NULL)             # IF there are potentially more options, put in suffix
  nums <- as_chr(1:nx(opts))                                                     # valid option numbers
  list <- paste0(c("\n", paste0(nums, " = ", opts)), collapse = "\n")            # options preceded by numbers
  question <- spf("Enter a sequence of 1 or more numbers of %s from these options:\n\n", what)
  if (all | none) {                                                              # IF all or none are valid responses
    list <- c(list, "")                                                          # : separate from numeric list by two newlines
    if (all) {
      list <- c(list, "ALL = all of the above")
      nums <- c(nums, "all")
    }
    if (none) {
      list <- c(list, "NONE = none of the above")
      nums <- c(nums, "none")
    }
    if (more) {
      list <- c(list, spf("DONE = stop selecting %s", what))
      nums <- c(nums, "done")
  }}                                                                             # END
  question <- paste0(question, paste0(list, collapse = "\n"), f0(inll(suffix), suffix, c("\n\n", suffix)))
  answer <- tolower(ask(question))                                                                # ask the question
  if (all  & answer == "all" ) {answer <- opts; attr(answer, "done") <- !more; return(answer)} # return all options if appropriate
  if (none & answer == "none") {answer <- ""[-1]; attr(answer, "done") <- FALSE; return(answer)} # return no options if appropriate
  answer <- av(strsplit(answer, " ", fixed = T))
  answer <- av(strsplit(answer, ",", fixed = T))
  answer <- answer[answer != ""]                                                 # remove any blanks
  done <- !more | isIN("done", answer)                                          # whether done choosing options
  answer <- f0(more, answer[answer != "done"], answer)                           # IF more to come > answer reduced to options not equal to "done"
  attr(answer, "done") <- done                                                   # attach the [done] attribute to the answer
  if (length(answer) == 0 & more) {return(answer)}                               # IF no options were chosen and more are possible, return answer
  if (setequal(uv(answer), answer)) {if (allIN(answer, nums)) {                  # IF all options selected are unique > IF all options are valid
    answer <- opts[sort(as_int(answer))]                                         # : answer is the options indexed by the numbers entered
    attr(answer, "done") <- done                                                 # : attach [done] attribute
    return(answer)                                                               # : return the result
  }}                                                                             # END:END
  stop("invalid selection")                                                      # throw error if nothing has yet been returned
}

#' @describeIn dialogs Ask user to select a one or more options from among a
#'   list of possible options, possibly across multiple rounds of interaction
#' @export
asksn <- function(opts, what, per = 9) {
  done  <- F                                                                         # start out not done yet
  round <- 1                                                                         # round counter
  out <- NULL                                                                        # results
  while (!done) {                                                                    # WHILE not yet done
    n <- nx(opts)                                                                    # : number of options remaining available
    what <- paste0(f0(round > 1 & ipop(out), "additional ", "blank"), what)          # : add "additional" as prefix to [whats] if this is not the first round
    list <- opts[1:min(per, n)]                                                      # : take the first [per] options (or all options, if not more than per)
    answer <- askn(list, what, all = T, none = T, more = n > per)                    # : ask user to select from among those options, indicating whether more options may be coming
    out <- c(out, av(answer))                                                        # : append the answers to the running results
    if (attr(answer, "done")) {return(f0(inll(out), stop("canceled by user"), out))} # : IF the [done] attribute is [TRUE] > IF none chosen THEN throw error ELSE return results
    opts <- opts[not_in(opts, list)]                                                 # : remove options presented in this round from the remaining options
    round <- round + 1                                                               # : increment the round counter
    done <- nx(opts) == 0                                                            # : [done] is reset to whether there are no options remaining to choose from
  }                                                                                  # END
  out                                                                                # return value
}

#' @describeIn dialogs Ask user to select 1 options from among a list of
#'   possible options, possibly across multiple dialog boxes.
#' @export
asks1 <- function(opts, what, per = 9) {
  done <- F                                                                      # start out not done yet
  out <- NULL                                                                    # results
  while (!done) {                                                                # WHILE not yet done
    n <- nx(opts)                                                                # : number of options remaining available
    list <- opts[1:min(per, n)]                                                  # : take the first [per] options (or all options, if not more than per)
    answer <- ask1(list, what, more = n > per)                                   # : ask user to select from among those options, indicating whether more options may be coming
    opts <- opts[not_in(opts, list)]                                             # : remove options presented in this round from those remaining
    out <- answer                                                                # : store the answer
    done <- n0(opts) | n1(answer)                                                # : reset to either no options of an option was chosen
  }                                                                              # END
  out                                                                            # return value
}

#' @describeIn dialogs Ask user to enter a space- or comma-separated list of
#'   replacement values for existing values.
#' @export
asknew <- function(old, what, unq = T) {
  list <- dw("\n", old)                                                           # separate old values with newlines
  question <- spf("Enter a space- and/or comma-separated list of %d new %s corresponding to:\n\n%s", nx(old), what, list)
  out <- ask(question)                                                            # ask the question
  out <- av(strsplit(out, " ", fixed = T))
  out <- av(strsplit(out, ",", fixed = T))
  out <-  out[out != ""]                                                          # remove any blanks
  if (length(out) == length(old) & (!unq | setequal(uv(out), out))) {return(out)} # IF the replacement length matches and uniqueness checks out THEN answer is valid so return it
  stop("invalid entry")                                                           # ELSE error
}

