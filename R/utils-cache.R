read_cache <- function(cache_path, cache_type, col_select = NULL, as.data.table) {
  message("Reading from cache. Typically cache variable classes need to be set manually.")

  switch(cache_type,
         fst = fst::read_fst(cache_path, columns = col_select, as.data.table = as.data.table),
         csv = arrow::read_csv_arrow(cache_path),
         parquet = if(is.null(col_select)) {
           arrow::read_parquet(cache_path)
         } else {
           arrow::read_parquet(cache_path, col_select = col_select)
         }
  )
}



write_cache <- function(data, cache_path, cache_type) {

  ## Create the temp sub-directory
  dipr_temp <- file.path(tempdir(),"dipr_temp")
  if (!dir.exists(dipr_temp)) dir.create(dipr_temp)
  ## write to ssd then copy back to cache_path
  temp_local_cache <- file.path(dipr_temp, basename(cache_path))

  switch(cache_type,
         fst = fst::write_fst(data, temp_local_cache),
         csv = readr::write_csv(data, temp_local_cache),
         parquet = arrow::write_parquet(data, temp_local_cache)
  )

  ## copy over
  if(!file.copy(temp_local_cache, cache_path)) {
    stop(glue::glue("File copy from {team_local_cache} to {cache_path}"), call. = FALSE)
  } else {
    ## delete temp file
    unlink(temp_local_cache)
  }
}


create_cache_dir <- function(cache_path) {

  if(!dir.exists(dirname(cache_path))) dir.create(dirname(cache_path))

  readme_path <- file.path(dirname(cache_path), "README.md")
  gitignore_path <- file.path(dirname(cache_path), ".gitignore")

  readme_from <- file.path(fs::path_package("dipr"), "templates/README.md")
  gitignore_from <- file.path(fs::path_package("dipr"), "templates/git.ignore")

  file.copy(readme_from, readme_path)
  file.copy(gitignore_from, gitignore_path)

}
