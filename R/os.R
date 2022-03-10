#' @name os
#' @family meta
#' @title Identify current operating system
#' @description Identifies the operating system as 'win', 'mac', or 'unix'.
#' @return Character scalar
#' @export
os <- function() {
  Type <- .Platform$OS.type
  Name <- Sys.info()["sysname"]
  if      (Type == "windows") {return("win" )}
  else if (Name == "Darwin" ) {return("mac" )}
  else if (Type == "unix"   ) {return("unix")}
  stop("In function {os}:\n",
    "Unknown operating system. Common operating systems\n",
    "are identified in the following ways:             \n",
    "  'windows' when .Platform$OS.type  == 'windows'  \n",
    "  'unix'    when .Platform$OS.type  == 'unix'     \n",
    "  'mac'     when .Platform$OS.type  == 'unix'     \n",
    "            & Sys.info()['sysname'] == 'Darwin'   \n",
    "--------------------------------------------------\n",
    "However, in this case, the values are:            \n",
    "  .Platform$OS.type      == '", Type, "'          \n",
    "  Sys.info()['sysname']  == '", Name, "'          \n")
}
