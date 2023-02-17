#' @name rd
#' @encoding UTF-8
#' @family extensions
#' @family wraps
#' @family files
#' @title Thin and extended functionality wrappers of `readr` functions.
#' @description Read the clipboard and text files, but prompting user to choose a file if no file name/path is provided.
#' @details
#' \tabular{ll}{  `rdCLIP`   \tab Thinly wraps \code{\link[readr]{clipboard}}                   \cr   \tab   \cr
#'                `rdCSV`    \tab Extends \code{\link[readr]{read_csv}}\eqn{^{(1)}}             \cr
#'                `rdTSV`    \tab Extends \code{\link[readr]{read_tsv}}\eqn{^{(1)}}             \cr
#'                `rdXSV`    \tab Extends \code{\link[readr]{read_delim}}\eqn{^{(1)}}           \cr
#'                           \tab \eqn{^{(1)}} `file = NULL` prompts the user to select a file.                }
#' @param file Either `NULL` or a \link[=cmp_chr_vec]{complete character vector} that resolves to a file path. When `file = NULL` the user is asked to select a file using a system dialog box.
#' @return **Varies**       \cr `rdCLIP`
#' \cr\cr  **A data.frame** \cr `rdCSV, rdTSV, rdXSV`
#' @export
rdCSV <- function(file = NULL, col_names = TRUE, col_types = NULL, col_select = NULL, id = NULL, locale = readr::default_locale(), na = base::c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip = 0, n_max = Inf, guess_max = base::min(1000, n_max), name_repair = "unique", num_threads = readr::readr_threads(), progress = readr::show_progress(), show_col_types = readr::should_show_types(), skip_empty_rows = TRUE, lazy = readr::should_read_lazy()) {
  if (uj::NLL(file)) {file <- uj::choose_doc("comma separated values text file")}
  uj::err_if_not(uj::cmp_chr_vec(file), "[file] must be NULL or a complete character vec (?cmp_chr_vec).", PKG = "uj")
  file <- uj::g0(file)
  uj::err_if_not(base::file.exists(file), "the specified file ('", file, "') does not exist.", PKG = "uj")
  readr::read_csv(file, col_names = col_names, col_types = col_types, col_select = col_select, id = id, locale = locale, na = na, quoted_na = quoted_na, quote = quote, comment = comment, trim_ws = trim_ws, skip = skip, n_max = n_max, guess_max = guess_max, name_repair = name_repair, num_threads = num_threads, progress = progress, show_col_types = show_col_types, lazy = lazy)
}

#' @rdname rd
#' @export
rdCLIP <- function() {readr::clipboard()}

#' @rdname rd
#' @export
rdXSV <- function(file = NULL, delim = NULL, quote = "\"", escape_backslash = FALSE, escape_double = TRUE, col_names = TRUE, col_types = NULL, col_select = NULL, id = NULL, locale = readr::default_locale(), na = base::c("", "NA"), quoted_na = TRUE, comment = "", trim_ws = FALSE, skip = 0, n_max = Inf, guess_max = base::min(1000, n_max), name_repair = "unique", num_threads = readr::readr_threads(), progress = readr::show_progress(), show_col_types = sreadr::should_show_types(), skip_empty_rows = TRUE, lazy = readr::should_read_lazy()) {
  if (uj::NLL(file)) {file <- uj::choose_doc("delimited text file")}
  uj::err_if_not(uj::cmp_chr_vec(file), "[file] must be NULL or a complete character vector (?cmp_chr_vec).", PKG = "uj")
  file <- uj::g0(file)
  uj::err_if_not(base::file.exists(file), "the specified file ('", file, "') does not exist.", PKG = "uj")
  readr::read_delim(file = file, delim = delim, quote = quote, escape_backslash = escape_backslash, escape_double = escape_double, col_names = col_names, col_types = col_types, col_select = col_select, id = id, locale = locale, na = na, quoted_na = quoted_na, comment = comment, trim_ws = trim_ws, skip = skip, n_max = n_max, guess_max = guess_max, name_repair = name_repair, num_threads = num_threads, progress = progress, show_col_types = show_col_types, skip_empty_rows = skip_empty_rows, lazy = lazy)
}

#' @rdname rd
#' @export
rdTSV <- function(file = NULL, col_names = TRUE, col_types = NULL, col_select = NULL, id = NULL, locale = readr::default_locale(), na = base::c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip = 0, n_max = Inf, guess_max = base::min(1000, n_max), progress = readr::show_progress(), name_repair = "unique", num_threads = readr::readr_threads(), show_col_types = readr::should_show_types(), skip_empty_rows = TRUE, lazy = readr::should_read_lazy()) {
  if (uj::NLL(file)) {file <- uj::choose_doc("tab separated values text file")}
  uj::err_if_not(uj::cmp_chr_vec(file), "[file] must be NULL or a complete character vector (?cmp_chr_vec).", PKG = "uj")
  file <- uj::g0(file)
  uj::err_if_not(base::file.exists(file), "the specified file ('", file, "') does not exist.", PKG = "uj")
  readr::read_tsv(file = file, col_names = col_names, col_types = col_types, col_select = col_select, id = id, locale = locale, na = na, quoted_na = quoted_na, quote = quote, comment = comment, trim_ws = trim_ws, skip = skip, n_max = n_max, guess_max = guess_max, progress = progress, name_repair = name_repair, num_threads = num_threads, show_col_types = show_col_types, skip_empty_rows = skip_empty_rows, lazy = lazy)
}
