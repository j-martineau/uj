#' @encoding UTF-8
#' @family files
#' @title Evaluate and manipulate paths to files or folders
#' @description Take an file reference and get its directories, path, and name; its parent's directories, path, and name. Convert a `...` args to a path, check the validity of a path, and create new directories within a given directory.
#' @details In most cases, if (optionally) specified, functions throw an error if `...` does not resolve to a valid object path.
#' @param ... Atomic arguments pasted to create a path.
#' @param dirs A \link[=cmp_chr_vec]{complete character vec} of new sub-directory names.
#' @param path A \link[=cmp_chr_scl]{complete character scalar} path to either a directory or a file.
#' @param err `TRUE` or `FALSE` indicating whether an error should be thrown if `...` is not a valid object path when collapsed to a character scalar.
#' @return **A character scalar** \cr\cr `object_dirs, parent_path` \cr `object_name, parent_name` \cr `as_path`
#' \cr\cr  **A character vector** \cr\cr `object_dirs, parent_dirs`
#' \cr\cr  **A logical scalar**   \cr\cr `is_path`
#' @examples
#' path. <- path.expand("~")
#' parts. <- ss(.Platform$file.sep, path.)
#' parts. <- parts.[parts. != ""]
#' parts.[1] <- paste0(.Platform$file.sep, parts.[1])
#' parts. <- paste0("'", parts., "'")
#' code. <- paste0(parts., collapse = ", ")
#' is_path. <- paste0("is_path(", code., ")")
#' as_path. <- paste0("as_path(", code., ")")
#'
#' path.
#' parts.
#' is_path.
#' as_path.
#'
#' parent_dirs(path.)
#' object_dirs(path.)
#' parent_path(path.)
#' object_path(path.)
#' parent_name(path.)
#' object_name(path.)
#' run(is_path.)
#' run(as_path.)
#' @export
paths_help <- function() {utils::help("paths_help", package = "uj")}

#' @describeIn paths_help Checks whether `...` resolves to a valid path for an object (file or folder) when `...` arguments are collapsed into a character scalar.
#' @export
is_path <- function(..., err = F) {
  x <- base::list(...)
  okN <- base::length(x) > 0
  okNs <- uj::f0(!okN, T, base::all(base::lengths(x) > 0))
  errs <- NULL
  if (!okN ) {errs <- base::c(errs, "[...] is empty.")}
  if (!okNs) {errs <- base::c(errs, "An argument in [...] is empty.")}
  if (!uj::f0(okN | okNs, T, base::all(base::sapply(x, uj::.cmp_chr)))) {errs <- base::c(errs, "Arguments in [...] must be complete character scalars/vecs (?cmp_chr_scl, ?cmp_chr_vec).")}
  if (!uj::.cmp_lgl_scl(err)) {errs <- base::c(errs, "[err] must be TRUE or FALSE.")}
  if (!base::is.null(errs)) {uj::stopperr(errs)}
  path <- base::file.path(...)
  if (base::file.exists(path)) {return(T)}
  ok <- !uj::is_err(tryCatch(base::file.create(path, showWarnings = F), error = function(e) e, finally = NULL))
  if (!ok & err) {uj::stopperr("[...] doesn't resolve to a valid file/folder path.")}
  if (ok) {base::file.remove(path)}
  ok
}

#' @describeIn paths_help Collapses `...` into a path using the current platform file path separator `.Platform$file.sep`.
#' @export
as_path <- function(..., err = T) {
  d <- base::.Platform$file.sep
  path <- tryCatch(base::file.path(...), error = function(e) e, finally = NULL)
  if (uj::is_err(path)) {uj::stopperr("[...] does not resolve to a valid file/folder path.")}
  path <- base::strsplit(path, d, fixed = T)[[1]]
  args <- base::paste0("path[", 1:base::length(path), "]")
  args <- base::paste0(args, collapse = ", ")
  code <- base::paste0("base::file.path(", args, ")")
  path <- uj::run(code)
  if (base::substr(path, 1, 1) != d) {path <- base::paste0(d, path)}
  path
}

#' @describeIn paths_help Calls `object_path` and split the resulting character scalar into a character vector containing the name of the root folder, the names of any intermediate folders, the name of the parent folder of the object, and the name of the object.
#' @export
object_dirs <- function(path, err = T) {
  path <- uj::as_path(path, err = err)
  path <- base::strsplit(path, base::.Platform$file.sep, fixed = T)[[1]]
  path <- path[path != ""]
  uj::f0(base::length(path) < 2, base::character(0), path[1:(base::length(path) - 1)])
}

#' @describeIn paths_help Calls `parent_path` and split the resulting character scalar into a character vector containing the name of the root folder, the names of any intermediate folders, and the name of the parent folder of the object specified by `...`.
#' @export
parent_dirs <- function(path, err = T) {
  path <- uj::object_dirs(path, err)
  uj::f0(base::length(path) < 2, base::character(0), path[1:(base::length(path) - 1)])
}

#' @describeIn paths_help Collapses `...` into a character scalar and expand the path to the object indicated by the result to account for relative paths.
#' @export
object_path  <- function(path, err = T) {uj::as_path(path, err = err)}

#' @describeIn paths_help Calls `object_path` and extract from the resulting character scalar just the path to the parent folder of the object specified by `...` (i.e., discarding the name of the object itself).
#' @export
parent_path  <- function(path, err = T) {base::paste0(uj::object_dirs(path, err), collapse = base::.Platform$file.sep)}

#' @describeIn paths_help Calls `object_path` and extract from the resulting character scalar just the name of the object, discarding the path to its parent.
#' @export
object_name <- function(path, err = T) {base::basename(uj::as_path(path, err = err))}

#' @describeIn paths_help Calls `parent_path` and extract from the resulting character scalar just the name of last folder in the path.
#' @export
parent_name <- function(path, err = T) {base::basename(uj::parent_path(path, err = err))}

#' @describeIn paths_help Creates sub-directories within an existing directory, optionally asking the user to choose that existing directory.
#' @export
new_dirs <- function(dirs, path = NULL) {
  if (base::is.null(path)) {path <- uj::choose_dir("folder to create new sub-folders in")}
  for (d in dirs) {if (!base::dir.exists(base::file.path(path, d))) {base::dir.create(base::file.path(path, d))}}
}
