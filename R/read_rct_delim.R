#' @title Interactively Select and Read a Rectangular Delimited Text Data File
#' @param type A \link[uj:cmp_str_scl]{complete string scalar} description of the type of data file to be read.
#' @param .D A \link[uj:cmp_ch1_scl]{complete onechar scalar} text delimiter.
#' @param .CLEAR A logical scalar indicating whether to clear the console to denote a new user interaction.
#' @return A data.frame.
#' @export
read_rct_delim <- function(type, .D = ",", .CLEAR = TRUE, ...) {
  base::on.exit(base::gc(verbose = F))
  Errors <- NULL
  if (!uj::cmp_str_scl(type)) {Errors <- base::c(Errors, "[type] must be a complete string scalar (?uj::cmp_str_scl).")}
  if (!uj::cmp_ch1_scl(.D)) {Errors <- base::c(Errors, "[.D] must be a complete onechar scalar (?uj::cmp_ch1_scl).")}
  if (!base::isTRUE(.CLEAR) & !base::isFALSE(.CLEAR)) {Errors <- base::c(Errors, "[.CLEAR] must be scalar TRUE or scalar FALSE.")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  sep <- uj::f0(uj::is_EQ(.D, ","), "comma",
                uj::f0(uj::is_EQ(.D, "\t"), "tab",
                       uj::f0(uj::is_EQ(.D, "`"), "backtick",
                              uj::f0(uj::is_EQ(.D, "~"), "tilde",
                                     uj::f0(uj::is_EQ(.D, "^"), "caret",
                                            uj::f0(uj::is_EQ(.D, "|"), "pipe", uj::p0("`", .D, "`")))))))
  doc.type <- uj::g0("rectangular ", sep, "-separated ", type, " data file")
  doc.path <- uj::choose_doc(doc.type, .CLEAR = .CLEAR)
  data <- uj::rd_xsv(file = doc.path, .D = .D)
  uj::clean_data(data, .CLEAR = .CLEAR)
}
