library(dplyr)
library(gdata)
library(R.utils)


## trim data set
small_sw <- starwars %>%
  filter(!is.na(name), !is.na(height), !is.na(mass)) %>%
  mutate(has_hair = is.na(hair_color)) %>%
  select(name:mass, has_hair) %>%
  as.data.frame()

## Create .dat file
write.fwf(small_sw, "inst/extdata/starwars-fwf.dat", colnames = FALSE, sep = "")

## Compress data file
gzip(
  filename = "inst/extdata/starwars-fwf.dat",
  destname = "inst/extdata/starwars-fwf.dat.gz",
  remove = FALSE,
  overwrite = TRUE
)

## Create Data Dictionary
data_dict <- tribble(
  ~name, ~start, ~stop, ~col_type,
  "name", 1, 21, "c",
  "height", 22, 24, "i",
  "mass", 25, 30, "d",
  "has_hair", 31, 35, "l"
)

write.table(data_dict, "inst/extdata/starwars-dict.txt")


## Create Data Dictionary
data_dict_nlft <- tribble(
  ~name, ~start, ~length, ~type,
  "name", 1, 20, "c",
  "height", 22, 2, "i",
  "mass", 25, 5, "d",
  "has_hair", 31, 4, "l"
) %>%
  select(start, length, type, name)

write.table(
  data_dict_nlft,
  sep = "\t",
  quote = FALSE,
  col.names = FALSE,
  file = "inst/extdata/starwars-dict.nflt"
)


## manual step of adding this line to the top of the file:
## foo foo

read_nflt(dipr_example('starwars-dict.nflt'))


