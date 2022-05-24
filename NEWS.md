# dipr 1.3.1

* Fixed bug in `install_sre_gitlab()` where it would fail if the desired package didn't have a previous version installed.

# dipr 1.3.0

* Added new arguments `date_format` and `time_format` to `read_dat()`, `dat_to_arrow_formats()` 
etc, to allow overriding the global 
formats when reading in a dataset. Defaults are the same as in `readr` functions.

# dipr 1.2.3

* New function `health_dict_to_spec()` to create a `readr` column specification from a health dictionary. This allows parsing of most date formats used in the health datasets (#37)
* New function `flag_across`, similar to `filter_across`, but rather than filtering the dataset it adds a logical column indicating the result of the pattern search across the selected columns (#38)

# dipr 1.2.2

## Improvements
* Modify credential tests to only run in SRE
* Have `ocwa_branch_export` modified Rmd as well as md

# dipr 1.2.1
* Add Bonnie Robert and Andy Teucher as authors
* Document deprecated functions in one place

# dipr 1.2.0

## Breaking Changes
* Deprecate `msp_unique` and re-export it in the hawkeye package
* Deprecate `dipr_icd_categories`, `dipr_icd9_categories` and `dipr_icd10_categories` functions in favour of functionality added to hawkeye package

## Improvements
* Add new function `filter_across`
* Changed the internal method to install and update `{dipr}`, with new instructions in the README. The new method uses the gitlab API and will enable using `{dipr}` to install other SRE gitlab R packages using `install_sre_gitlab`. (#24)
* Add new function `get_gitlab_sre_repos` 
* Add new function `filter_linked` and add `Dataset` methods for `add_linked_status_col` function so that it can be used in a arrow workflow. 
* The template `DESCRIPTION` file now adds dependent packages to the `Depends` field rather than the `Imports` field.
* Fixed a bug where the template `.Rprofile` file used in `dipr_create_targets_project()` was not included in the package.
* Deprecate `msp_unique` and re-export it in the hawkeye package
* New function `restore_rstudio_prefs()` to help setup RStudio in a new SRE machine (#31)

# dipr 1.1.0

* Added function `ocwa_branch_export()` (#26) to create a clean branch to prepare the repo for import into OCWA by:
  1. Creating a new branch
  2. Removing files that can't be imported - these are listed in the `_ocwaignore` file in the root of the repo
  3. Cleaning `README.md` to comment out references to images and links that won't be available in the SRE.
  4. Committing the changes from 2 and 3 to the new branch and pushing that to GitHub. This branch can then be used as the basis for an import into OCWA.

* Added `dipr_create_targets_project` which will create a thin package-like targets folder structure. 
* Adding `dipr_document_output_groups` as convenience to document datasets
* Changes `get_core_dat_path()` and `get_core_dict_path()` to accomodate the new structure of provisioned data where the metadata are in the same directory as the data. (#17)
* Adds a new `data_format` argument to `dat_to_arrow_formats()` and friends, as well as `read_dat()` and the internal `dipr_reader()` (#17)
* Exposes `...` in `read_nflt()` to allow passing options to `readr::read_delim()` (#17)
* Comments are now removed from nflt files in `read_nflt`. Comments are by default expected to be denoted by `/*`, but this is customizable with the `comment` argument. (#21)

# dipr 1.0.0

## Breaking Change
* Caching has been removed from dipr (via `read_dat`) as it was not being used. Removing caching removed significant code complexity

## Improvements
* Add convenience function `set_gitlab_credentials()` to pull up the Git Credential Manager proactively (#6)
* Directly use readr now that it use vroom under the hood. Dropping vroom as a direct dependency
* Through readr 2.0, `read_dat` can now directly read two files supplied as a vector of `.dat.gz`. We have added a second dipr example data set to test this behaviour.
* Experimental `dat_to_datasets` function that is a low memory method to create partitioned Arrow Datasets. 
* Add species column to internal data
* Improved documentation all around

# dipr 0.0.9
* Add `age_at_date`
* Add `dipr_icd9_categories` and `dipr_icd10_categories` functions to process icd codes. Thanks to Craig Hutton for this contribution.
* Add argument to `msp_unique` to only use primary icd9 code. This is primarily been done to work around memory bottle necks.


# dipr 0.0.8
* Add `get_core_dat_path` and `get_core_dict_path` functions.
* Fix tests to avoid UTF issue. starwars data is now much smaller.
* Add digest shim to work around remote drive issue.
* Add `dipr_use_export_doc` function

# dipr 0.0.7
* Add `read_dat_dt` function which will return a data.table object.
* Soft deprecate `as_data_table` argument from read_dat
* Fix bug in `read_dat` where `...` weren't actually passed to `vroom` and test
* Add a `add_linked_status_col` function and test
* Add a `group_ages()` function
* Add internal function `is_emptyish` 


# dipr 0.0.6
* setup of github actions
* make package pass r cmd check

# dipr 0.0.5
* Rename package to `dipr`

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
