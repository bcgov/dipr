library(dplyr)
library(gdata)
library(R.utils)


## trim data set
small_sw <- starwars %>%
  filter(!is.na(name), !is.na(height), !is.na(mass)) %>%
  mutate(has_hair = is.na(hair_color)) %>%
  select(name:mass, has_hair, species) %>%
  head(30) %>%
  as.data.frame()

## Write all data to one fwf file then split it to ensure
## character positions are the same across both files
tmpfile <- tmpfile("temp.dat")
write.fwf(small_sw, tmpfile, colnames = FALSE, sep = "")

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

## Create Data Dictionary
data_dict <- tribble(
  ~name, ~start, ~stop, ~col_type,
  "name", 1, 21, "c",
  "height", 22, 24, "i",
  "mass", 25, 30, "d",
  "has_hair", 31, 35, "l",
  "species", 36, 50, "c"
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
  file = "inst/extdata/starwars-dict.nflt"
)




