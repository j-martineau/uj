#' @name wraps_readr
#' @family wraps
#' @title Wraps of functions from package \code{readr}.
#' @description The following table describes wrapper functions for the
#'   \code{readr} package:\tabular{ll}{
#'   WRAPPER NAME    \tab READR FUNCTION                                     \cr
#'   \code{rdclip}   \tab \code{\link[readr]{clipboard}}                     \cr
#'   \code{rdcsv}    \tab \code{\link[readr]{read_csv}}                      \cr
#'   \code{rdtsv}    \tab \code{\link[readr]{read_tst}}                      \cr
#'   \code{rdxsv}    \tab \code{\link[readr]{read_delim}}                      }
#'   File-reading functions also allow for a \code{NULL} value of file to prompt
#'   the user to choose a file.
#' @param file Either \code{NULL} or a \link[=chr_vec]{character vector} that
#'   resolves to a file path. When \code{file = NULL} the user is asked to
#'   select a file using a system dialog box.
#' @return A data.frame.
#' @export
wraps_readr <- NULL

#' @describeIn wraps_readr Read a comma separated values text file.
#' @inherit readr::read_csv
#' @export
rdcsv <- function(file = NULL, col_names = TRUE, col_types = NULL, col_select = NULL, id = NULL, locale = readr::default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip = 0, n_max = Inf, guess_max = min(1000, n_max), name_repair = "unique", num_threads = readr::readr_threads(), progress = readr::show_progress(), show_col_types = readr::should_show_types(), skip_empty_rows = TRUE, lazy = readr::should_read_lazy()) {
  if (inll(file)) {file <- choose_doc("comma separated values text file")}
  if (!cmp_chr_vec(file)) {stop("\n \u2022 [file] must be NULL or a complete character vector (?cmp_chr_vec).")}
  else {file <- paste0(file, collapse = "")}
  if (!file.exists(file)) {stop("\n \u2022 the specified file ('", file, "') does not exist.")}
  readr::read_csv(file, col_names = col_names, col_types = col_types, col_select = col_select, id = id, locale = locale, na = na, quoted_na = quoted_na, quote = quote, comment = comment, trim_ws = trim_ws, skip = skip, n_max = n_max, guess_max = guess_max, name_repair = name_repair, num_threads = num_threads, progress = progress, show_col_types = show_col_types, lazy = lazy)
}

#' @describeIn wraps_readr Read the clipboard as text.
#' @inherit readr::clipboard
rdclip <- function() {readr::clipboard()}

#' @describeIn wraps_readr Read an arbitrary-delimiter separated values text
#'   file.
#' @inherit readr::read_delim
#' @export
rdxsv <- function(file = NULL, delim = NULL, quote = "\"", escape_backslash = FALSE, escape_double = TRUE, col_names = TRUE, col_types = NULL, col_select = NULL, id = NULL, locale = readr::default_locale(), na = c("", "NA"), quoted_na = TRUE, comment = "", trim_ws = FALSE, skip = 0, n_max = Inf, guess_max = min(1000, n_max), name_repair = "unique", num_threads = readr::readr_threads(), progress = readr::show_progress(), show_col_types = sreadr::should_show_types(), skip_empty_rows = TRUE, lazy = readr::should_read_lazy()) {
  if (inll(file)) {file <- choose_doc("delimited text file")}
  if (!cmp_chr_vec(file)) {stop("\n \u2022 [file] must be NULL or a complete character vector (?cmp_chr_vec).")}
  else {file <- paste0(file, collapse = "")}
  if (!file.exists(file)) {stop("\n \u2022 the specified file ('", file, "') does not exist.")}
  readr::read_delim(file = file, delim = delim, quote = quote, escape_backslash = escape_backslash, escape_double = escape_double, col_names = col_names, col_types = col_types, col_select = col_select, id = id, locale = locale, na = na, quoted_na = quoted_na, comment = comment, trim_ws = trim_ws, skip = skip, n_max = n_max, guess_max = guess_max, name_repair = name_repair, num_threads = num_threads, progress = progress, show_col_types = show_col_types, skip_empty_rows = skip_empty_rows, lazy = lazy)
}

#' @describeIn wraps_readr Read a tab separated values text file.
#' @inherit readr::read_tsv
#' @export
rdtsv <- function(file = NULL, col_names = TRUE, col_types = NULL, col_select = NULL, id = NULL, locale = readr::default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip = 0, n_max = Inf, guess_max = min(1000, n_max), progress = readr::show_progress(), name_repair = "unique", num_threads = readr::readr_threads(), show_col_types = readr::should_show_types(), skip_empty_rows = TRUE, lazy = readr::should_read_lazy()) {
  if (inll(file)) {file <- choose_doc("tab separated values text file")}
  if (!cmp_chr_vec(file)) {stop("\n \u2022 [file] must be NULL or a complete character vector(?cmp_chr_vec).")}
  else {file <- paste0(file, collapse = "")}
  if (!file.exists(file)) {stop("\n \u2022 the specified file ('", file, "') does not exist.")}
  readr::read_tsv(file = file, col_names = col_names, col_types = col_types, col_select = col_select, id = id, locale = locale, na = na, quoted_na = quoted_na, quote = quote, comment = comment, trim_ws = trim_ws, skip = skip, n_max = n_max, guess_max = guess_max, progress = progress, name_repair = name_repair, num_threads = num_threads, show_col_types = show_col_types, skip_empty_rows = skip_empty_rows, lazy = lazy)
}
