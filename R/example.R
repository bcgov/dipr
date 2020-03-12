#' Get path to dipr example files
#'
#' `dipr` provides an example of compressed
#' fixed width file and a data dictionary. This data
#' is from the `dplyr` package and contains no real personal
#' information. These functions are modelled heavily of similar
#' functions in the vroom package.
#' @param path A path to the data file
#'
#' @export
#'
#' @examples
#'
#' ## Get all available data files
#' dipr_examples()
#'
#' ## Use that to find the path to a file
#' dipr_example("starwars-dict.txt")
dipr_examples <- function () {
  list.files(system.file("extdata", package = "dipr"))
}


#' @rdname dipr_examples
#' @export
dipr_example <- function (path) {
  system.file("extdata", path, package = "dipr",
              mustWork = TRUE)
}
