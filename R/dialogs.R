#' @name dialogs.
#' @family dialogs
#' @title Dialog boxes using package \code{svDialogs}.
#' @description Launch a dialog box
#' @param x. \link[cmp_chr_scl]{Complete character scalar} message.
#' @param t \link[cmp_chr_scl]{Complete character scalar} type of dialog box
#'   (valid values are \code{c('ok', 'okcancel', 'yesno', 'yesnocancel'}).
#' @param def \link[cmp_chr_scl]{Complete character scalar} default value.
#' @param type, \link[cmp_chr_scl]{Complete character scalar} type of directory
#'   or document to select
#' @param ... Objects to be atomized to create the question.
#' @param cancel \link[cmp_chr_scl]{Complete character scalar} value to return
#'   if user clicks the \code{cancel} button The value \code{cancel = '.stop'}
#'   is an indicator that an error should be thrown if the user clicks the
#'   \code{cancel} button.
#' @param opts Character vector of options to choose from.
#' @param what \link[cmp_chr_scl]{Complete character scalar} describing type of
#'   option being presented.
#' @param more \link[cmp_lgl_scl]{Complete logical scalar} indicating whether
#'   there are more options than could be presented in a single dialog box that
#'   still could be presented.
#' @param all,none \link[cmp_lgl_scl]{Complete logical scalars} indicating
#'   whether the user should be able to select all options or none of the
#'   options, respectively.
#' @param per \link[cmp_psw_scl]{Complete positive whole-number scalar}
#'   indicating the maximum number of options to present in a single dialog box
#'   interaction.
#' @param unq \link[cmp_lgl_scl]{Complete logical scalar} indicating whether new
#'   values must be unique.
#' @return \link[cmp_chr_scl]{Complete character scalar}
#' @export
dialogs_uj <- function() {help("dialogs_uj", package = "uj")}

#' @describeIn dialogs. Collapse \code{...} into a character scalar message and
#'   launch a simple dialog box to give the message.
#' @export
msgbox <- function(..., t = "ok") {
  x <- dw0(av(...))
  err <- NULL
  if (inll(x)) {err <- c(err, "\n • No user update message was provided in [...].")}
  if (!cmp_chr_scl(t)) {err <- c(err, "\n • [t] must be a character scalar in c('ok', 'okcancel', 'yesno', 'yesnocancel').")}
  if (idef(err)) {stop(dw0(err))}
  svDialogs::dlg_message(x, type = t)
  NULL
}

#' @describeIn dialogs. Collapse {...} into a character scalar prompt. Ask user
#'   to select a directory/folder. Fail safe feature launches a message dialog
#'   box prompting the use to take an action in the next dialog box After user
#'   acknowledges, launches the directory/folder selection dialog to avoid
#'   problems with the prompt not showing up on all operating systems.
#' @export
dirbox <- function(..., def = getwd()) {
  dot <- dw0(av(...))
  def <- dw0(av(def))
  if (inll(dot)) {dot <- "choose a directory/folder"}
  err <- NULL
  if (!cmp_chr_scl(dot)) {err <- c(err, "\n • No message to the user was provided in [...].")}
  if (!cmp_chr_scl(def)) {err <- c(err, "\n • No default directory provided in [def].")}
  else if (!dir.exists(def)) {err <- c(err, da0("\n • [def = '", def, "'] is not an existing directory/folder."))}
  msgbox(da0("In the next dialog box, ", dot)); svDialogs::dlg_dir(default = def, title = dot)
}

#' @describeIn dialogs. Collapse \code{...} into a character scalar prompt. Ask
#'   user to select a document Fail safe feature launches a message dialog box
#'   prompting the use to take an action in the next dialog box After user
#'   acknowledges, launches the file selection dialog to avoid problems with the
#'   prompt not showing up on all operating systems.
#' @export
docbox <- function(...) {
  dot <- dw0(av(...))
  if (inll(dot)) {dot <- "choose a document"}
  err <- NULL
  if (!cmp_chr_scl(dot)) {err <- c(err, "\n • [...] must be empty or must be collapsible to a character scalar.")}
  msgbox(da0("In the next dialog box, ", dot)); svDialogs::dlg_open(title = dot)
}

#' @describeIn dialogs. Ask user to input a text response.
#' @export
ansbox <- function(x = "Enter a value", def = "") {svDialogs::dlg_input(message = x, default = def)}

#' @describeIn dialogs. Does user select the 'no' button (rather than 'yes') in
#'   response to the prompt?
no <- function(...) {msgbox(daw00(...), t = "yesno")$res != "yes"}

#' @describeIn dialogs. Display a message in a dialog box, returning
#'   the value of the button clicked.
#' @export
msg <- function(x, t = "ok") {msgbox(x, t = t)$res}

#' @describeIn dialogs. Does user select the 'ok' (rather than 'cancel') button
#'   in response to the prompt?
#' @export
okx <- function(x) {x <- msg(x, "okcancel"); if (x != "ok") {stop("canceled")}}

#' @describeIn dialogs. Ask user to select a directory.
#' @export
choose_dir <- function(type = "file") {dirbox(okx(da1("Select a", type, "directory")))$res}

#' @describeIn dialogs. Ask user to select a document
#' @export
choose_doc <- function(type = "document") {docbox(okx(da1("Select a", type, "file")))$res}

#' @describeIn dialogs. Does user select the 'yes' button (rather than 'no') in
#'   response to the prompt?
#' @export
yes <- function(...) {msgbox(daw00(...), "yesno")$res == "yes"}

#' @describeIn dialogs. Ask the user for typed input to be returned without
#'   further processing.
#' @export
ask <- function(..., def = v(blank), cancel = ".stop") {
  ends <- r(3, v(nl))                                                            # pad front and back with three newlines to make all content appear inside the dialog box
  ans <- ansbox(x = daw00(ends, ..., ends), def = def)$res                       # build the question and ask the user the question
  if (n0(ans) & cancel == ".stop") {stop("canceled by user")}                    # IF no answer should throw error and there is no answer > throw error
  else if (n0(ans)) {cancel}                                                     # BUT IF there is no answer > return the value specified for canceling
  else {ans}                                                                     # ELSE return the answer
}

#' @describeIn dialogs. Ask user to select a single option from among a list of
#'   possible options.
#' @export
ask1 <- function(opts, what, more = F) {
  suff. <- f0(more, "\n(more options may be presented next)", NULL)              # suffix if more options are available
  nums. <- as_chr(1:nx(opts))                                                    # valid responses
  lst <- peq(nums., opts)                                                        # list of options with preceding numbers separated by newlines
  lst <- f0(inll(suff.), lst, daw00(lst, v(nl), v(nl), "none = none of these options")) # add
  qst <- daw00("Enter the number of one ", what, " from these options:\n\n")     # build the prefix
  qst <- da0(v(nl), qst, lst, suff.)                                             # build the question separated by newlines
  ans <- ask(q, cancel = ".stop")                                                # ask the question
  if (ans %in% as_chr(nums.)) {opts[as_int(ans)]}                                # IF answer is the number of an option, return that option
  else if (ipop(suff.) & ans == "none") {return(NULL)}                           # BUT IF there is a suffix and selection is none, return NULL
  else {stop("invalid selection")}                                               # ELSE error
}

#' @describeIn dialogs. Ask user to select a one or more options from among a
#'   list of possible options.
#' @export
askn <- function(opts, what, all = T, none = T, more = F) {
  suff. <- f0(more, "\n(more options may be presented next)", NULL)              # IF there are potentially more options, put in suffix
  nums. <- as_chr(1:nx(opts))                                                    # valid option numbers
  lst <- da0(v(nl), peq(nums., opts))                                            # options preceded by numbers
  qst <- spf(da0("Enter a sequence of 1 or more numbers of %s ",                 # prompt
              "from these options:\n\n"), what)                                  # |
  if (all | none) {                                                              # IF all or none are valid responses
    lst <- c(lst, "\n")                                                          # : separate from numeric list by two newlines
    if (all ) {lst <- c(lst, "all = all of the above")}                          # : add all option if specified
    if (none) {lst <- c(lst, "none = none of the above")}                        # : add none option if specicied
    if (more) {lst <- c(lst, spf("done = stop selecting %s", what))}             # : add done option if more is an option
  }                                                                              # END
  qst <- da0(qst, dw0(lst, w. = v(nl)), f0(inll(suff.), suff., c(v(nl), v(nl), suff.))) # glue the prefix and newline-delimited list of options into a question
  ans <- ask(qst)                                                                # ask the question
  if (all  & ans == "all" ) {ans <- opts; attr(ans, "done") <- !more; return(ans)} # return all options if appropriate
  if (none & ans == "none") {ans <- ""[-1]; attr(ans, "done") <- FALSE; return(ans)} # return no options if appropriate
  ans <- ss(ss1(ans), v(comma0))                                                 # split response along spaces and commas
  ans <- ss1(ans)                                                                # split along spaces
  ans <- ans[ans != v(blank)]                                                    # remove any blanks
  done. <- !more | is_in("done", ans)                                            # whether done choosing options
  ans <- f0(more, ans[ans != "done"], ans)                                       # IF more to come > answer reduced to options not equal to "done"
  attr(ans, "done") <- done.                                                     # attach the [done] attribute to the answer
  if (n0(ans) & more) {return(ans)}                                              # IF no options were chosen and more are possible, return answer
  if (setequal(uv(ans), ans)) {if (allIN(ans, v)) {                              # IF all options selected are unique > IF all options are valid
    ans <- opts[sort(as_int(ans))]                                               # : answer is the options indexed by the numbers entered
    attr(ans, "done") <- done.                                                   # : attach [done] attribute
    return(ans)                                                                  # : return the result
  }}                                                                             # END:END
  stop("invalid selection")                                                      # throw error if nothing has yet been returned
}

#' @describeIn dialogs. Ask user to select a one or more options from among a
#'   list of possible options, possibly across multiple rounds of interaction
#' @export
asksn <- function(opts, what, per = 9) {
  done. <- F                                                                     # start out not done yet
  i <- 1                                                                         # round counter
  out <- NULL                                                                    # results
  while (!done.) {                                                               # WHILE not yet done
    n <- nx(opts)                                                                # : number of options remaining available
    wht <- da0(f0(i > 1 & ipop(out), "additional ", v(blank)), what)             # : add "additional" as prefix to [whats] if this is not the first round
    lst <- opts[1:min(per, n)]                                                   # : take the first [per] options (or all options, if not more than per)
    ans <- askn(lst, wht, all = T, none = T, more = n > per)                     # : ask user to select from among those options, indicating whether more options may be coming
    out <- c(out, av(ans))                                                       # : append the answers to the running results
    if (attr(ans, "done")) {return(f0(inll(out), stop("canceled by user"), out))}# : IF the [done] attribute is [TRUE] > IF none chosen THEN throw error ELSE return results
    opts <- opts[not_in(opts, lst)]                                              # : remove options presented in this round from the remaining options
    i <- i + 1                                                                   # : increment the round counter
    done. <- nx(opts) == 0                                                       # : [done] is reset to whether there are no options remaining to choose from
  }                                                                              # END
  out                                                                            # return value
}

#' @describeIn dialogs. Ask user to select 1 options from among a list of
#'   possible options, possibly across multiple dialog boxes.
#' @export
asks1 <- function(opts, what, per = 9) {
  done. <- F                                                                     # start out not done yet
  i <- 1                                                                         # round counter
  out <- NULL                                                                    # results
  while (!done.) {                                                               # WHILE not yet done
    n <- nx(opts)                                                                # : number of options remaining available
    lst <-  opts[1:min(per, n)]                                                  # : take the first [per] options (or all options, if not more than per)
    ans <- ask1(lst, what, more = n > per)                                       # : ask user to select from among those options, indicating whether more options may be coming
    opts <- opts[not_in(opts, lst)]                                              # : remove options presented in this round from those remaining
    i <- i + 1                                                                   # : increment the round counter
    out <- ans                                                                   # : store the answer
    done. <- n0(opts) | n1(ans)                                                  # : reset to either no options of an option was chosen
  }                                                                              # END
  out                                                                            # return value
}

#' @describeIn dialogs. Ask user to enter a space- or comma-separated list of
#'   replacement values for existing values.
#' @export
asknew <- function(old., what, unq = T) {
  lst <- dw(old., w. = v(nl))                                                    # separate old values with newlines
  qst <- spf(da0("Enter a space- and/or comma-separated list of %d new %s ",     # build the prompt
              "corresponding to:\n\n%s"), nx(old.), what, lst)                   # |
  ans <- ask(qst)                                                                # ask the question
  ans <- ss(ss(ans, v(space)), v(comma0))                                        # split response along spaces and commas
  ans <-  ans[ans != v(blank)]                                                   # remove any blanks
  if (nx(ans) == nx(old.) & (!unq | setequal(uv(ans), ans))) {return(ans)}       # IF valid THEN return
  stop("invalid entry")                                                          # ELSE error
}

