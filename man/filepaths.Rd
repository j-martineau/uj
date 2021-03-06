% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filenames.R
\name{filepaths}
\alias{filepaths}
\alias{is_path}
\alias{as_path}
\alias{object_path}
\alias{parent_path}
\alias{object_name}
\alias{parent_name}
\alias{object_dirs}
\alias{parent_dirs}
\title{Evaluate and manipulate paths to files or folders on disk.}
\usage{
is_path(..., err = F)

as_path(...)

object_path(path, err = T)

parent_path(path, err = T)

object_name(path, err = T)

parent_name(path, err = T)

object_dirs(path, err = T)

parent_dirs(path, err = T)
}
\arguments{
\item{...}{Atomic arguments pasted to create}

\item{err}{Logical scalar indicating whether an error should be thrown if
\code{...} is not a valid object path when collapsed to a character scalar.}
}
\value{
\strong{\code{is_path}}: A logical scalar. \strong{\code{object_path,
  folder_path, object_name, folder_name}}: A character scalar.
\strong{\code{object_dirs, folder_dirs}}: A character vector.
}
\description{
Evaluate and manipulate paths to files or folders on disk.
}
\details{
\strong{\code{is_path}}
\cr Checks whether \code{...} resolves to a valid path for an object (file
or folder) when arguments in \code{...} are collapsed into a character
scalar.
\cr \cr
\strong{\code{as_path}}
\cr Collapses \code{...} into a path using the current platform file path
separator (\code{.Platform$file.sep}).
\cr \cr
\strong{\code{object_path}}
\cr Collapses \code{...} into a character scalar and expands the path to
the object indicated by the result to account for relative paths.
\cr \cr
\strong{\code{parent_path}}
\cr Calls \code{object_path} and extracts from the resulting character
scalar just the path to the parent folder of the object specified by
\code{...} (i.e., discarding the name of the object itself).
\cr \cr
\strong{\code{object_name}}
\cr Calls \code{object_path} and extracts from the resulting character
scalar just the name of the object, discarding the path to its parent
folder.
\cr \cr
\strong{\code{parent_name}}
\cr Calls \code{parent_path} and extracts from the resulting character
scalar just the name of last folder in the path.
\cr \cr
\strong{\code{object_dirs}}
\cr Calls \code{object_path} and splits the resulting character scalar into
a character vector containing the name of the root folder, the names of any
intermediate folders, the name of the parent folder of the object, and the
name of the object.
\cr \cr
\strong{\code{parent_dirs}}
\cr Calls \code{folder_path} and splits the resulting character scalar into
a character vector containing the name of the root folder, the names of any
intermediate folders, and the name of the parent folder of the object
specified by \code{...}.
\cr \cr
In all cases, if (optionally) specified, all functions throw an error if
\code{...} does not resolve to a valid object path.
}
\concept{files}
