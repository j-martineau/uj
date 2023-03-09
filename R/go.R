#' @title Open or reopen an R project
#' @details
#' \tabular{ll}{  `rebuild`   \tab Rebuilds the current R project. \cr   \tab   \cr
#'                `reopen`    \tab Reopens the current R project.  \cr   \tab   \cr
#'                `go`        \tab Opens another project and loads the associated library if it exists. When `name = NULL`, launches a document selection dialog box and prompts user to select an existing R project. Otherwise, searches for an R project with name equal to the value of `name` in the parent directory of the current working directory, and if there is such a project, opens that project.}
#' @param ... Either nothing or a single unquoted R project name.
#' @return `NULL`
#' @export
go <- function(...) {
  rproj <- function(x) {base::substr(x, base::nchar(x) - base::nchar(".Rproj") + 1, base::nchar(x)) == ".Rproj"}
  file <- base::match.call()
  file <- base::as.character(file)
  if (base::length(file) != 2) {uj::stopperr("Invalid call.", PKG = "uj")}
  file <- file[2]
  if (base::is.null(file)) {
    path <- uj::choose_doc(".Rproj")
    if (!rproj(file)) {uj::stopperr("The selected file is not an R project.", PKG = "uj")}
    base::gc(verbose = F)
    rstudioapi::openProject(path)
  } else {
    fsep <- base::.Platform$file.sep
    dir <- uj::parent_path(base::getwd())
    doc <- base::paste0(dir, fsep, file, fsep, file, ".Rproj")
    doc <- uj::as_path(doc)
    base::gc(verbose = F)
    if (!base::file.exists(doc)) {uj::stopperr("No such R project", PKG = "uj")}
    base::gc(verbose = F)
    rstudioapi::openProject(path = doc)
  }
}

#' @rdname go
#' @export
reopen <- function() {rstudioapi::openProject(); base::gc(verbose = F)}

#' @rdname go
#' @export
rebuild <- function() {
  devtools::document(roclets = base::c('rd', 'collate', 'namespace'))
  base::gc(verbose = F)
}
