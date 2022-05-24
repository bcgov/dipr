library(dplyr)
library(readr)
library(gdata)
library(R.utils)

set.seed(13) # set seed so dates don't chagne each time we run this

## trim data set
small_sw <- starwars %>%
  filter(!is.na(name), !is.na(height), !is.na(mass)) %>%
  mutate(has_hair = is.na(hair_color)) %>%
  select(name:mass, has_hair, species) %>%
  head(30) %>%
  mutate(date = paste0(sample(1900:2025, 30, replace = TRUE),
                       formatC(sample(1:12, 30, replace = TRUE), width = 2, flag = "0"),
                       formatC(sample(1:28, 30, replace = TRUE), width = 2, flag = "0"))) %>%
  as.data.frame()

## Write all data to one fwf file then split it to ensure
## character positions are the same across both files
tmpfile <- tmpfile("temp.dat")
write.fwf(small_sw, tmpfile, colnames = FALSE, sep = "")
readr::write_csv(small_sw[1:15, ], "inst/extdata/starwars-csv.dat")
readr::write_csv(small_sw[15:30, ], "inst/extdata/starwars-csv2.dat")

## Create .dat file
writeLines(readLines(tmpfile)[1:15], con = "inst/extdata/starwars-fwf.dat")
writeLines(readLines(tmpfile)[16:30], con = "inst/extdata/starwars-fwf2.dat")

## Compress data files
gzip(
  filename = "inst/extdata/starwars-fwf.dat",
  destname = "inst/extdata/starwars-fwf.dat.gz",
  remove = FALSE,
  overwrite = TRUE
)

gzip(
  filename = "inst/extdata/starwars-fwf2.dat",
  destname = "inst/extdata/starwars-fwf2.dat.gz",
  remove = FALSE,
  overwrite = TRUE
)

gzip(
  filename = "inst/extdata/starwars-csv.dat",
  destname = "inst/extdata/starwars-csv.dat.gz",
  remove = FALSE,
  overwrite = TRUE
)

gzip(
  filename = "inst/extdata/starwars-csv2.dat",
  destname = "inst/extdata/starwars-csv2.dat.gz",
  remove = FALSE,
  overwrite = TRUE
)

## Create Data Dictionary
data_dict <- tribble(
  ~name, ~start, ~stop, ~col_type,
  "name", 1, 21, "c",
  "height", 22, 24, "i",
  "mass", 25, 30, "d",
  "has_hair", 31, 35, "l",
  "species", 36, 49, "c",
  "date", 50, 57,"c"
)

write.table(data_dict, "inst/extdata/starwars-dict.txt")

## Create Data Dictionary
data_dict_nlft <- data_dict %>%
  mutate(length = stop-start) %>%
  select(start, length, col_type, name)


write.table(
  data_dict_nlft,
  sep = "\t",
  quote = FALSE,
  col.names = FALSE,
  row.names = FALSE,
  file = "inst/extdata/starwars-dict.nflt"
)

# create nflt with comments
dict <- readLines("inst/extdata/starwars-dict.nflt")

dict[3] <- paste0(dict[3], "  /* height in kg */")

writeLines(dict, "inst/extdata/starwars-dict-with-comments.nflt")



