## Re-jig the internal digest:::check_file function to use
## R.utils::fileAccess instead. See the _targets.R for how
## we re-assign the function in the package.
if (Sys.getenv("USERDOMAIN") == "POPDATA") {
  require(digest, warn.conflicts = FALSE)

  if (suppressPackageStartupMessages(require(R.utils, warn.conflicts = FALSE))) {
  require(utils)
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
  }
}

home_rprofile <- file.path(Sys.getenv("HOME"), ".Rprofile")

if (file.exists(home_rprofile)) source(home_rprofile)
