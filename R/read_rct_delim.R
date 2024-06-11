#' @title Interactively Select and Read a Rectangular Delimited Text Data File
#' @family files
#' @param type A \link[=cmp_str_scl]{complete string scalar} description of the type of data file to be read.
#' @param d A \link[=cmp_ch1_scl]{complete onechar scalar} text delimiter.
#' @param clear A logical scalar indicating whether to clear the console to denote a new user interaction.
#' @return A data.frame.
#' @export
read_rct_delim <- function(type, d = ",", clear = TRUE) {
  base::on.exit(base::gc(verbose = F))
  errs <- NULL
  if (!uj::cmp_str_scl(type )) {errs <- base::c(errs, "[type] must be a complete string scalar (?cmp_str_scl).")}
  if (!uj::cmp_ch1_scl(d    )) {errs <- base::c(errs, "[d] must be a complete onechar scalar (?cmp_ch1_scl)."  )}
  if (!uj::cmp_lgl_scl(clear)) {errs <- base::c(errs, "[clear] must be scalar TRUE or scalar FALSE."           )}
  if (!base::is.null(errs)) {uj::stopperr(errs)}
  sep <- uj::f0(uj::is_EQ(d, "," ), "comma"   ,
         uj::f0(uj::is_EQ(d, "\t"), "tab"     ,
         uj::f0(uj::is_EQ(d, "`" ), "backtick",
         uj::f0(uj::is_EQ(d, "~" ), "tilde"   ,
         uj::f0(uj::is_EQ(d, "^" ), "caret"   ,
         uj::f0(uj::is_EQ(d, "|" ), "pipe"    ,
         uj::p0("`", d, "`")))))))
  type <- uj::g0("rectangular ", sep, "-separated ", type, " data file")
  path <- uj::choose_doc(type, clear = clear)
  data <- uj::rd_xsv(file = path, d = d)
  uj::clean_data(data, clear = clear)
}
