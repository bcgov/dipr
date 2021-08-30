## Setting common paths

data_dict_path <- dipr_example("starwars-dict.txt")
dict <- read.table(data_dict_path)
dat_path <- dipr_example("starwars-fwf.dat.gz")
dat_path2 <- dipr_example("starwars-fwf2.dat.gz")
cache_dir <- tempdir()
out_parquet <- file.path(tempdir(),"foo.parquet")


## Some helper functions

temp_file <- function(cache_dir, file_name) {

  file.path(glue::glue("{cache_dir}/{file_name}"))

}
