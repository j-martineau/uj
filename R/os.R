#' @encoding UTF-8
#' @family environments
#' @title Identify the current operating system.
#' @return A character scalar (either `mac`, `unix`, or `win`).
#' @examples
#' os()
#' @export
os <- function() {
  Type <- base::.Platform$OS.type
  Name <- base::Sys.info()["sysname"]
  if (!(Type %in% base::c("windows", "unix")) & Name != "Darwin") {uj::stopperr(base::paste0(
    "In function {os}:\n",
    "Unknown operating system. Common operating systems\n", "are identified in the following ways:             \n",
    "  'windows' when .Platform$OS.type  == 'windows'  \n", "  'unix'    when .Platform$OS.type  == 'unix'     \n",
    "  'mac'     when .Platform$OS.type  == 'unix'     \n", "            & Sys.info()['sysname'] == 'Darwin'   \n",
    "                                                  \n", "However, in this case, the values are:            \n",
    "  .Platform$OS.type      == '", Type, "'          \n", "  Sys.info()['sysname']  == '", Name, "'          \n"), .PKG = "uj"
  )} else if (Type == "windows") {"win"}
  else if (Name == "Darwin" ) {"mac"}
  else if (Type == "unix"   ) {"unix"}
}
