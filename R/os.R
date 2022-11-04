#' @name os_uj
#' @family meta
#' @title Identify current operating system
#' @return Character scalar
#' @export
os_uj <- function() {help("os", package = "uj")}

#' @describeIn os_uj Identifies the operating system as 'win', 'mac', or 'unix'.
#' @export
os <- function() {
  type <- .Platform$OS.type
  name <- Sys.info()["sysname"]
  if      (type == "windows") {return("win" )}
  else if (name == "Darwin" ) {return("mac" )}
  else if (type == "unix"   ) {return("unix")}
  stop("In function {os}:\n",
    "Unknown operating system. Common operating systems\n",
    "are identified in the following ways:             \n",
    "  'windows' when .Platform$OS.type  == 'windows'  \n",
    "  'unix'    when .Platform$OS.type  == 'unix'     \n",
    "  'mac'     when .Platform$OS.type  == 'unix'     \n",
    "            & Sys.info()['sysname'] == 'Darwin'   \n",
    "--------------------------------------------------\n",
    "However, in this case, the values are:            \n",
    "  .Platform$OS.type      == '", type, "'          \n",
    "  Sys.info()['sysname']  == '", name, "'          \n")
}
