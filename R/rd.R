#' @name rd
#' @family wraps
#' @family files
#' @title Thin and extended functionality wrappers of `readr` functions.
#' @description \tabular{rl}{
#'      `rdclip` \tab   Thinly wraps \code{\link[readr]{clipboard}}.
#'   \cr `rdcsv` \tab   Extends \code{\link[readr]{read_csv}}\eqn{^1}.
#'   \cr `rdtsv` \tab   Extends \code{\link[readr]{read_tsv}}\eqn{^1}.
#'   \cr `rdxsv` \tab   Extends \code{\link[readr]{read_delim}}\eqn{^1}.
#' }
#' \eqn{^1} Prompts user to select a file if `file = NULL`.
#' @param file Either `NULL` or a \link[=chr_vec]{character vector} that resolves to a file path. When `file = NULL` the user is asked to select a file using a system dialog box.
#' @inherit readr::read_csv
#' @return \tabular{rl}{
#'      `rdclip` \tab   Varies.
#'   \cr `rdcsv` \tab   A `data.frame`.
#'   \cr `rdtsv` \tab   A `data.drame`.
#'   \cr `rdxsv` \tab   A `data.drame`.
#' }
#' @export
rdcsv <- function(file = NULL, col_names = TRUE, col_types = NULL, col_select = NULL, id = NULL, locale = readr::default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip = 0, n_max = Inf, guess_max = min(1000, n_max), name_repair = "unique", num_threads = readr::readr_threads(), progress = readr::show_progress(), show_col_types = readr::should_show_types(), skip_empty_rows = TRUE, lazy = readr::should_read_lazy()) {
  if (inll(file)) {file <- choose_doc("comma separated values text file")}
  if (!cmp_chr_vec(file)) {stop(.errs("[file] must be NULL or a complete character vector (?cmp_chr_vec)."))}
  else {file <- paste0(file, collapse = "")}
  if (!file.exists(file)) {stop(.errs(p0("the specified file ('", file, "') does not exist.")))}
  readr::read_csv(file, col_names = col_names, col_types = col_types, col_select = col_select, id = id, locale = locale, na = na, quoted_na = quoted_na, quote = quote, comment = comment, trim_ws = trim_ws, skip = skip, n_max = n_max, guess_max = guess_max, name_repair = name_repair, num_threads = num_threads, progress = progress, show_col_types = show_col_types, lazy = lazy)
}

#' @rdname rd
#' @inherit readr::clipboard
#' @export
rdclip <- function() {readr::clipboard()}

#' @rdname rd
#' @inherit readr::read_delim
#' @export
rdxsv <- function(file = NULL, delim = NULL, quote = "\"", escape_backslash = FALSE, escape_double = TRUE, col_names = TRUE, col_types = NULL, col_select = NULL, id = NULL, locale = readr::default_locale(), na = c("", "NA"), quoted_na = TRUE, comment = "", trim_ws = FALSE, skip = 0, n_max = Inf, guess_max = min(1000, n_max), name_repair = "unique", num_threads = readr::readr_threads(), progress = readr::show_progress(), show_col_types = sreadr::should_show_types(), skip_empty_rows = TRUE, lazy = readr::should_read_lazy()) {
  if (inll(file)) {file <- choose_doc("delimited text file")}
  if (!cmp_chr_vec(file)) {stop(.errs("[file] must be NULL or a complete character vector (?cmp_chr_vec)."))}
  else {file <- paste0(file, collapse = "")}
  if (!file.exists(file)) {stop(.errs(p0("the specified file ('", file, "') does not exist.")))}
  readr::read_delim(file = file, delim = delim, quote = quote, escape_backslash = escape_backslash, escape_double = escape_double, col_names = col_names, col_types = col_types, col_select = col_select, id = id, locale = locale, na = na, quoted_na = quoted_na, comment = comment, trim_ws = trim_ws, skip = skip, n_max = n_max, guess_max = guess_max, name_repair = name_repair, num_threads = num_threads, progress = progress, show_col_types = show_col_types, skip_empty_rows = skip_empty_rows, lazy = lazy)
}

#' @rdname rd
#' @inherit readr::read_tsv
#' @export
rdtsv <- function(file = NULL, col_names = TRUE, col_types = NULL, col_select = NULL, id = NULL, locale = readr::default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip = 0, n_max = Inf, guess_max = min(1000, n_max), progress = readr::show_progress(), name_repair = "unique", num_threads = readr::readr_threads(), show_col_types = readr::should_show_types(), skip_empty_rows = TRUE, lazy = readr::should_read_lazy()) {
  if (inll(file)) {file <- choose_doc("tab separated values text file")}
  if (!cmp_chr_vec(file)) {stop(.errs("[file] must be NULL or a complete character vector(?cmp_chr_vec)."))}
  else {file <- paste0(file, collapse = "")}
  if (!file.exists(file)) {stop(.errs(p0("the specified file ('", file, "') does not exist.")))}
  readr::read_tsv(file = file, col_names = col_names, col_types = col_types, col_select = col_select, id = id, locale = locale, na = na, quoted_na = quoted_na, quote = quote, comment = comment, trim_ws = trim_ws, skip = skip, n_max = n_max, guess_max = guess_max, progress = progress, name_repair = name_repair, num_threads = num_threads, show_col_types = show_col_types, skip_empty_rows = skip_empty_rows, lazy = lazy)
}
