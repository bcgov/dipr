is_valid_data_dict <- function(x) {

  if(!inherits(x, "data.frame")) {
    stop("Your data dictionary is not a data.frame", call. = FALSE)
  }

  if (!"start" %in% names(x)) {
    stop("You need a 'start' column in your data dictionary")
  }

  if (!"stop" %in% names(x)) {
    stop("You need a 'stop' column in your data dictionary")
  }

  if (!"name" %in% names(x)) {
    stop("You need a 'name' column in your data dictionary")
  }

  invisible(TRUE)
}
