#' @name paths
#' @family files
#' @title Evaluate and manipulate paths to files or folders
#' @description \tabular{rl}{
#'       `parent_dirs`   \tab Calls `parent_path` and split the resulting character scalar into a character vector containing the name of the root folder, the names of any intermediate folders, and the name of the parent folder of the object specified by `...`.
#'   \cr                 \tab   
#'   \cr `object_path`   \tab Collapses `...` into a character scalar and expand the path to the object indicated by the result to account for relative paths.
#'   \cr                 \tab   
#'   \cr `parent_path`   \tab Calls `object_path` and extract from the resulting character scalar just the path to the parent folder of the object specified by `...` (i.e., discarding the name of the object itself).
#'   \cr                 \tab   
#'   \cr `object_name`   \tab Calls `object_path` and extract from the resulting character scalar just the name of the object, discarding the path to its parent.
#'   \cr                 \tab   
#'   \cr `parent_name`   \tab Calls `parent_path` and extract from the resulting character scalar just the name of last folder in the path.
#'   \cr                 \tab   
#'   \cr `object_dirs`   \tab Calls `object_path` and split the resulting character scalar into a character vector containing the name of the root folder, the names of any intermediate folders, the name of the parent folder of the object, and the name of the object.
#'   \cr                 \tab   
#'   \cr    `new_dirs`   \tab Creates sub-directories within an existing directory, optionally asking the user to choose that existing directory.
#'   \cr                 \tab   
#'   \cr     `is_path`   \tab Checks whether `...` resolves to a valid path for an object (file or folder) when `...` arguments are collapsed into a character scalar.
#'   \cr                 \tab   
#'   \cr     `as_path`   \tab Collapses `...` into a path using the current platform file path separator `.Platform$file.sep`.
#' }
#' @details In most cases, if (optionally) specified, functions throw an error if `...` does not resolve to a valid object path.
#' @param ... Atomic arguments pasted to create
#' @param err A non-`NA` logical scalar indicating whether an error should be thrown if `...` is not a valid object path when collapsed to a character scalar.
#' @param dirs A \link[=cmp_chr_vec]{complete character vec} of new sub-directory names.
#' @param path A \link[=cmp_chr_scl]{complete character scalar} path to either a directory or a file.
#' @return \tabular{rl}{
#'       `object_path` \tab   A character scalar.
#'   \cr `folder_path` \tab   A character scalar.
#'   \cr `object_name` \tab   A character scalar.
#'   \cr `folder_name` \tab   A character scalar.
#'   \cr `object_dirs` \tab   A character vector.
#'   \cr `folder_dirs` \tab   A character vector.
#'   \cr     `is_path` \tab   A logical scalar.
#' }
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
#'
#' parent_path(path.)
#' object_path(path.)
#'
#' parent_name(path.)
#' object_name(path.)
#'
#' run(is_path.)
#' run(as_path.)
#' @export
is_path <- function(..., err = F) {
  x <- list(...)
  ok.n <- length(x) > 0
  ok.ns <- f0(!ok.n, T, all(lengths(x) > 0))
  errs <- c(f0(ok.n                                        , NULL, "[...] is empty."),
            f0(ok.ns                                       , NULL, "An argument in [...] is empty."),
            f0(f0(ok.n | ok.ns, T, all(sapply(x, cmp_chr))), NULL, "Arguments in [...] must be complete character scalars/vecs (?cmp_chr_scl, ?cmp_chr_vec)."),
            f0(isTF(err)                                   , NULL, "[err] must be TRUE or FALSE."))
  if (!is.null(errs)) {stop(.errs(errs))}
  path <- file.path(...)
  exists <- file.exists(path)                                                    # whether the file exists
  if (exists) {return(T)}                                                        # if the resulting file already exists, return T
  ok <- !isERR(file.create(path, showWarnings = F))                              # can such a file be created?
  if (!ok & err) {stop(.errs("[...] doesn't resolve to a valid file/folder path."))}
  if (ok) {file.remove(path)}                                                    # remove any newly created file
  ok
}

#' @rdname paths
#' @export
as_path <- function(..., err = T) {
  if (is_path(..., err = err)) {file.path(...)}
  else {stop(.errs("[...] does not resolve to a valid file/folder path."))}
}

#' @rdname paths
#' @export
object_path  <- function(path, err = T) {f0(is_path(path, err = err), path.expand(path), "")}

#' @rdname paths
#' @export
parent_path  <- function(path, err = T) {dirname(object_path(path, err = err))}

#' @rdname paths
#' @export
object_name  <- function(path, err = T) {basename(object_path(path, err = err))}

#' @rdname paths
#' @export
parent_name  <- function(path, err = T) {object_name(parent_path(path, err = err))}

#' @rdname paths
#' @export
object_dirs <- function(path, err = T) {x <- strsplit(object_path(path, err = err), .Platform$file.sep, fixed = T)[[1]]; x[x != ""]}

#' @rdname paths
#' @export
parent_dirs <- function(path, err = T) {x <- strsplit(parent_path(path, err = err), .Platform$file.sep, fixed = T)[[1]]; x[x != ""]}

#' @rdname paths
#' @export
newdirs <- function(dirs, path = NULL) {
  cancel <- function() {stop(.errs("canceled by user."))}                        # FUNCTION to throw an error
  if (inll(path)) {                                                              # IF path is not supplied
    msgbox("In the next dialog, choose a folder to create new sub-folders in.")  # : notify user of what to do next (title doesn't always appear in next box)
    path <- dirbox(x = "Choose a folder to create new sub-folders in:")$res      # : ask user to select a directory
  }                                                                              # END
  if (inll(path)) {cancel()} else if (!file.exists(path)) {cancel()}             # IF path is still NULL > error ELSE if it doesn't exist > error
  for (dir in dirs) {                                                            # FOR each new directory to create in the selected directory
    newdir <- file.path(path, dir)                                               # : build the path
    if (!dir.exists(newdir)) {dir.create(newdir)}                                # : IF it doesn't already exist > create it
  }                                                                              # END
}
