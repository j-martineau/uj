#' @title Open or reopen an R project
#' @details
#' \tabular{ll}{  `reopen`    \tab Reopens the current R project.  \cr   \tab   \cr
#'                `go`        \tab Opens another project and loads the associated library if it exists. When `name = NULL`, launches a document selection dialog box and prompts user to select an existing R project. Otherwise, searches for an R project with name equal to the value of `name` in the parent directory of the current working directory, and if there is such a project, opens that project.}
#' @param ... Either nothing or a single unquoted R project name.
#' @return `NULL`
#' @export
go <- function(...) {
  .rproj <- function(x) {base::substr(x, base::nchar(x) - base::nchar(".Rproj") + 1, base::nchar(x)) == ".Rproj"}
  File <- base::match.call()
  File <- base::as.character(File)
  if (base::length(File) != 2) {uj::stopperr("Invalid call.", PKG = "uj")}
  File <- File[2]
  if (base::is.null(File)) {
    Path <- uj::choose_doc(".Rproj")
    if (!.rproj(File)) {uj::stopperr("The selected File is not an R project.", PKG = "uj")}
    base::gc(verbose = F)
    rstudioapi::openProject(Path)
  } else {
    Sep <- base::.Platform$file.sep
    Dir <- uj::parent_path(base::getwd())
    Doc <- base::paste0(Dir, Sep, File, Sep, File, ".Rproj")
    Doc <- uj::as_path(Doc)
    base::gc(verbose = F)
    if (!base::file.exists(Doc)) {uj::stopperr("No such R project", PKG = "uj")}
    base::gc(verbose = F)
    rstudioapi::openProject(path = Doc)
  }
}

#' @rdname go
#' @export
reopen <- function() {rstudioapi::openProject(); base::gc(verbose = F)}
