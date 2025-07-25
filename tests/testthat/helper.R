## Setting common paths

data_dict_path <- dipr_example("starwars-dict.txt")
dict <- read.table(data_dict_path)
dat_path <- dipr_example("starwars-fwf.dat.gz")
dat_path2 <- dipr_example("starwars-fwf2.dat.gz")
dat_path_csv <- dipr_example("starwars-csv.dat.gz")
dat_path_csv2 <- dipr_example("starwars-csv2.dat.gz")
cache_dir <- tempdir()
out_parquet <- file.path(tempdir(),"foo.parquet")

d_in <- data.frame(studyid = c("sxxxx", "uxxxx"),
                   alt_studyid = c("uxxxx", "sxxxx"))

parquet_path <- tempfile(fileext = ".parquet")
arrow::write_parquet(d_in, parquet_path)


## Some helper functions

temp_file <- function(cache_dir, file_name) {

  file.path(glue::glue("{cache_dir}/{file_name}"))

}

make_test_data <- function(format = "Dataset"){
  #make some data
  d <- data.frame(id = 1:10,
                  icd_1 = as.character(1001:1010),
                  icd_2 = as.character(1011:1020))

  if (format == "Dataset"){
    tf <- tempfile()
    arrow::write_dataset(d, tf, format =  "arrow")
    d = arrow::open_dataset(tf, format = "arrow")
  }
  return(d)
}
