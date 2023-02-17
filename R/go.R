#' @title Open or reopen an R project
#' @details
#' \tabular{ll}{  `rebuild`   \tab Rebuilds the current R project. \cr   \tab  }
#' \tabular{ll}{  `reopen`    \tab Reopens the current R project.  \cr   \tab  }
#' \tabular{ll}{  `go`        \tab Opens another project and loads the associated library if it exists. When `name = NULL`, launches a document selection dialog box and prompts user to select an existing R project. Otherwise, searches for an R project with name equal to the value of `name` in the parent directory of the current working directory, and if there is such a project, opens that project.}
#' @param ... Either nothing or a single unquoted R project name.
#' @return `NULL`
#' @export
go <- function(...) {
  rproj <- function(x) {base::substr(x, uj::LEN(x) - uj::LEN(".Rproj") + 1, uj::LEN(x)) == ".Rproj"}
  file <- base::match.call()
  file <- uj::asCHR(file)
  narg <- uj::N(file) - 1
  uj::err_if(narg > 1, "Invalid call.", PKG = "uj")
  file <- file[2]
  if (uj::DEF(file)) {
    path <- uj::choose_doc(".Rproj")
    uj::err_if_not(rproj(file), "The selected file is not an R project.", PKG = "uj")
    base::gc(verbose = F)
    rstudioapi::openProject(path)
  } else {
    fsep <- base::.Platform$file.sep
    dir <- uj::parent_path(base::getwd())
    doc <- uj::p0(dir, fsep, file, fsep, file, ".Rproj")
    doc <- uj::as_path(doc)
    base::gc(verbose = F)
    uj::err_if_not(base::file.exists(doc), "No such R project", PKG = "uj")
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
