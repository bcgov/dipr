library(targets)
library(tarchetypes)

## Load R files
lapply(list.files("./R", pattern = ".R", full.names = TRUE), source)

## A fix for the SRE to handle
library(digest, warn.conflicts = FALSE)
suppressPackageStartupMessages(library(R.utils, warn.conflicts = FALSE))
.custom_check_file <- function (object, errormode) {
  if (!file.exists(object)) {
    return(.errorhandler("The file does not exist: ",
                         object, mode = errormode))
  }
  if (!isTRUE(!file.info(object)$isdir)) {
    return(.errorhandler("The specified pathname is not a file: ",
                         object, mode = errormode))
  }
  if (suppressWarnings(R.utils::fileAccess(object, 4))) {
    return(.errorhandler("The specified file is not readable: ",
                         object, mode = errormode))
  }
}

reassignInPackage("check_file", "digest", .custom_check_file)


## Load packages specified in DESCRIPTION file
tar_option_set(packages = desc::desc_get_deps()$package)


## paths
paths <- list(

)

## data
data <- list(

)

## pipeline
list(
  paths,
  data,
  NULL
)
