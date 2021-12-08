
## Re-jig the internal digest:::check_file function to use
## R.utils::fileAccess instead. See the _targets.R for how
## we re-assign the function in the package.
library(digest, warn.conflicts = FALSE)
suppressPackageStartupMessages(library(R.utils, warn.conflicts = FALSE))
library(utils)
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
