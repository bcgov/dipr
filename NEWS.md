# dipr 0.0.6.9000
* Fix bug in `read_dat` where `...` weren't actually passed to `vroom` and test
* Add a add_linked_status_col function and test


# dipr 0.0.6
* setup of github actions
* make package pass r cmd check

# dipr 0.0.5
* Rename packge to `dipr`

# dipdata 0.0.4
* `use_cache` is now off by default
* Actually export writing functions
* Use `data.table::setDT` to modify in place
* Add `dipdata_write_parquet`
* Pass `...` to underlying read functions for `read_nflt` and `read_health_dict`

# dipdata 0.0.3
* adding tests and functions to convert `.dat.gz` into Apache Arrow formats
* adding `read_nflt` and `read_health_dict` functions
* Add `msp_unique` function
* Add LICENSE header 
* Add `delete_rtmp_dirs` 
* Remove `read_msp` as that functionality is handled by `read_dat`


# dipdata 0.0.2
* cache file now automatically contains a README and gitignore
* Using the `DIPDATA_CACHE_PATH` env variable now works for central caching
* Added `col_type` to dummy data
* Added `dipdata_update` function
* Added `csv` to cache types
* Lots of tests added

# dipdata 0.0.1

* Added a `NEWS.md` file to track changes to the package.
* Working functions `read-dat`, `dipdata_example` & `dipdata_examples`
* Added dummy fixed width data
