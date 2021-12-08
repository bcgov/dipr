library(targets)
library(tarchetypes)

## Load R files
lapply(list.files("./R", pattern = ".R", full.names = TRUE), source)

## A fix for the SRE to handle
reassignInPackage("check_file", "digest", .custom_check_file)


if (.Platform$OS.type == "windows") options("arrow.use_threads" = FALSE)

## Load packages specified in DESCRIPTION file
tar_option_set(packages = desc::desc_get_deps()$package)


## To debug a target set the target:
# tar_option_set(debug = "target_to_debug")

## And run
# tar_make(callr_function = NULL, names = "target_to_debug", shortcut = TRUE)

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
