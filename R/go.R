#' @title Open or reopen an R project
#' @description \tabular{rl}{
#'      `rebuild`   \tab Rebuilds the current R project.
#'   \cr            \tab  
#'   \cr `reopen`   \tab Reopens the current R project.
#'   \cr            \tab  
#'   \cr     `go`   \tab Opens another project and loads the associated library if it exists. When `name = NULL`, launches a document selection dialog box and prompts user to select an existing R project. Otherwise, searches for an R project with name equal to the value of `name` in the parent directory of the current working directory, and if there is such a project, opens that project.
#' }
#' @param ... Either nothing or a single unquoted R project name.
#' @return `NULL`
#' @export
go <- function(...) {
  ext <- ".Rproj"
  rproj <- function(x) {base::substr(x, base::nchar(x) - base::nchar(ext) + 1, base::nchar(x)) == ext}
  name <- base::as.character(base::match.call())
  narg <- base::length(name) - 1
  name <- uj::f0(narg > 1, stop(uj:::.errs("Invalid call")), uj::f0(narg == 1, name[2], NULL))
  if (base::is.null(name)) {
    name <- uj::choose_doc(ext)
    if (rproj(name)) {rstudioapi::openProject(name)} else {stop(uj:::.errs("The selected file is not an R project."))}
  } else {
    fsep <- base::.Platform$file.sep
    dir <- uj::parent_path(base::getwd())
    doc <- base::paste0(dir, fsep, name, fsep, name, ext)
    doc <- uj::as_path(doc)
    if (base::file.exists(doc)) {rstudioapi::openProject(path = doc)} else {stop(uj:::.errs("No such R project"))}
  }
}

#' @rdname go
#' @export
reopen <- function() {rstudioapi::openProject()}

#' @rdname go
#' @export
rebuild <- function() {devtools::document(roclets = base::c('rd', 'collate', 'namespace'))}

