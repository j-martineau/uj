#' @encoding UTF-8
#' @family environments
#' @title Identify the current operating system.
#' @return A character scalar (either `mac`, `unix`, or `win`).
#' @examples
#' os()
#' @export
os <- function() {
  type <- base::.Platform$OS.type
  name <- base::Sys.info()["sysname"]
  if (!(type %in% base::c("windows", "unix")) & name != "Darwin") {uj::stopperr(base::paste0(
    "In function {os}:\n",
    "Unknown operating system. Common operating systems\n", "are identified in the following ways:             \n",
    "  'windows' when .Platform$OS.type  == 'windows'  \n", "  'unix'    when .Platform$OS.type  == 'unix'     \n",
    "  'mac'     when .Platform$OS.type  == 'unix'     \n", "            & Sys.info()['sysname'] == 'Darwin'   \n",
    "                                                  \n", "However, in this case, the values are:            \n",
    "  .Platform$OS.type      == '", type, "'          \n", "  Sys.info()['sysname']  == '", name, "'          \n"), pkg = "uj"
  )} else if (type == "windows") {"win"}
  else if (name == "Darwin" ) {"mac"}
  else if (type == "unix"   ) {"unix"}
}
