

#' Create a query string for loading csvs to a database.
#'
#' @param con database connection
#' @param path Character vector of one or more file paths, or a single string with a globbing pattern.
#'    E.g., `c("R/dir/a.csv", "R:/dir/b.csv")` OR `"R:/dir/*.csv"``.
#' @param tbl_name The name of the table to write
#' @param schema named character vector mapping column names to duckdb data types.
#' @param col_rename a named vector of columns to rename, with names as the original
#'    names, and values as the new names.
#'    E.g. `c(PHNNUM = "studyid", OLDNAME2 = "newname2")`
#' @param date_format format of date columns. Default `"%Y%m"`
#' @param timestamp_format format of timestamp columns. Default `"%Y%m%d%H%M"`
#' @param dt_override named list of date/datetime formats for columns
#'    that don't follow those defined in `date_format` and `timestamp_format`.
#'    Use this when a column has more than one date or datetime format.
#'    eg. `list(WEIRDATE = "%m/%d/%YT%H:%M:%S", OTHERCOL = c("%m/%d/%Y %I:%M:%S %p","%m/%d/%Y"))`
#' @param nullstr values that denote missing data. Character vector. Default `NULL` uses
#'    duckdb defaults. E.g., `nullstr = c("NA", "")`
#' @param opts Other options passed on to the internal `duckdb` `read_csv()`. A
#'    list with named elements. E.g.,
#'    `list(thousands = ",", comment = "#", force_not_null = c("a", "b"), null_padding = TRUE, buffer_size = 5)`
#'
#' @return a character string of the query, to be called with [DBI::dbExecute()]
#' @export
csv_to_duckdb_query <- function(
    con,
    path,
    tbl_name,
    schema,
    col_rename = NULL,
    date_format = "%Y%m",
    timestamp_format = "%Y%m%d%H%M",
    dt_override = NULL,
    nullstr = NULL,
    opts = list()
) {

  select_statement <- make_select_statement(schema, col_rename, dt_override)

  sql_schema <- make_sql_schema_string(con, schema, dt_override)

  path <- vec_to_string_or_array(path)

  opts <- parse_opts(con, nullstr, opts)

  glue::glue(
    "
  CREATE OR REPLACE TABLE {tbl_name} AS
  SELECT
    {select_statement}
  FROM read_csv(
    {path},
    types = {sql_schema},
    dateformat = '{date_format}',
    timestampformat = '{timestamp_format}'{opts}
  )
  "
  )
}

make_select_statement <- function(schema, col_rename = NULL, dt_override = NULL) {
  if (!is.null(col_rename)) {
    stopifnot(all(names(col_rename) %in% names(schema)))
  }

  new_column_names <- janitor::make_clean_names(names(schema))

  if (isTRUE(all.equal(new_column_names, names(schema))) && is.null(col_rename)) {
    return("*")
  }

  old_column_names <- names(schema)

  names(new_column_names) <- old_column_names
  names(old_column_names) <- old_column_names

  if (!is.null(col_rename)) {
    new_column_names[names(col_rename)] <- col_rename
  }

  if (!is.null(dt_override)) {
    old_column_names[names(dt_override)] <- paste0(
      "strptime(",
      names(dt_override),
      ", ",
      vapply(dt_override, vec_to_string_or_array, FUN.VALUE = character(1)),
      ")"
    )
  }

  paste0(
    sprintf("%s AS %s", old_column_names, new_column_names),
    collapse = ",\n  "
  )
}

make_sql_schema_string <- function(con, schema, dt_override = NULL) {
  stopifnot(is.character(schema))
  stopifnot(!is.null(names(schema)))
  check_duckdb_types(con, schema)

  if (!is.null(dt_override)) {
    # import special date/datetime columns as VARCHAR because they will be
    # converted in SELECT
    schema[names(dt_override)] <- "VARCHAR"
  }

  paste0(
    "{\n",
    paste(
      sprintf("    '%s': '%s'", names(schema), schema),
      collapse = ",\n"
    ),
    "\n  }"
  )
}

check_duckdb_types <- function(con, schema) {
  valid_types <- DBI::dbGetQuery(
    con,
    "SELECT type_name FROM duckdb_types()"
  )[["type_name"]]

  not_in <- setdiff(tolower(schema), valid_types)

  if (length(not_in) > 0) {
    stop(
      "The following are not valid duckdb data types: '",
      paste0(not_in, collapse = "', '"),
      "'"
    )
  }
}

#' Sniff a csv to get duckdb's auto-detected spec for the file
#'
#' This is a good function to run before [csv_to_duckdb_query()] to get
#' a starting point, especially for the schema.
#'
#' @inheritParams csv_to_duckdb_query
#' @param sample_size How many rows to sample to sniff formats from the data
#'
#' @return a list, with auto-detected `date_format`, `timestamp_format`, and
#'    `schema`
#' @export
#'
#' @examples
#' \dontrun{
#' spec <- sniff_csv_duckdb("path_to_csv.csv")
#'
#' # Use dput to get a schema you can edit
#' dput(spec$schema)
#' }
sniff_csv_duckdb <- function(path, sample_size = NULL) {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con))

  ss <- ""
  if (!is.null(sample_size)) {
    ss <- glue::glue(", sample_size = {sample_size}")
  }

  res <- DBI::dbGetQuery(
    con,
    glue::glue(
      "SELECT DateFormat, TimestampFormat, Columns FROM sniff_csv('{path}' {ss})"
    )
  )

  structure(
    list(
      date_format = res$DateFormat,
      timestamp_format = res$TimestampFormat,
      schema = stats::setNames(res$Columns[[1]]$type, res$Columns[[1]]$name)
    ),
    class = "sniffed_csv"
  )
}

#' @export
print.sniffed_csv <- function(x, ...) {
  cat("Auto-detected date_format: ", x$date_format)
  cat("\n")
  cat("Auto-detected timstamp_format: ", x$timestamp_format)
  cat("\n")
  cat("\n")

  cat("Auto-detected schema:\n")
  cat("  c(\n")
  cat(paste0("    ", names(x$schema), " = \"", x$schema, collapse = "\",\n"))
  cat("\"\n  )")
}

parse_opts <- function(con, nullstr = NULL, opts = list()) {
  stopifnot(is.list(opts))
  stopifnot(is.null(nullstr) || is.character(nullstr))

  opts <- remove_nulls(c(list(nullstr = nullstr), opts))

  if (is.null(opts) || length(opts) == 0) {
    return("")
  }

  check_params(con, opts)

  varchar_scalar_opts <- Filter(\(x) is.character(x) & length(x) == 1, opts)
  varchar_array_opts <- Filter(\(x) is.character(x) & length(x) > 1, opts)
  bool_opts <- Filter(\(x) is.logical(x) & length(x) == 1, opts)
  int_opts <- Filter(\(x) is.numeric(x) & length(x) == 1, opts)

  varchar_scalar_opts_parsed <-
    varchar_array_opts_parsed <-
    bool_opts_parsed <-
    int_opts_parsed <-
    NULL

  if (length(varchar_scalar_opts) > 0) {
    varchar_scalar_opts_parsed <- paste0(
      names(varchar_scalar_opts),
      " = '",
      vapply(varchar_scalar_opts, `[`, FUN.VALUE = character(1)),
      "'",
      collapse = ",\n  "
    )
  }

  if (length(varchar_array_opts) > 0) {
    varchar_array_opts_parsed <- paste0(
      names(varchar_array_opts),
      " = ['",
      vapply(
        varchar_array_opts,
        \(x) paste0(x, collapse = "', '"),
        FUN.VALUE = character(1)
      ),
      "']",
      collapse = ",\n  "
    )
  }

  if (length(bool_opts) > 0) {
    bool_opts_parsed <- paste0(
      names(bool_opts),
      " = ",
      vapply(bool_opts, \(x)  tolower(as.character(x)), FUN.VALUE = character(1)),
      collapse = ",\n  "
    )
  }

  if (length(int_opts) > 0) {
    int_opts_parsed <- paste0(
      names(int_opts),
      " = ",
      vapply(int_opts, \(x) as.character(x), FUN.VALUE = character(1)),
      collapse = ",\n  "
    )
  }

  paste0(
    ",\n  ",
    paste(
      c(
        varchar_scalar_opts_parsed,
        varchar_array_opts_parsed,
        bool_opts_parsed,
        int_opts_parsed
      ),
      collapse = ",\n  "
    )
  )
}

check_params <- function(con, opts) {
  read_csv_doc <- DBI::dbGetQuery(
    con,
    "SELECT * FROM duckdb_functions() WHERE function_name = 'read_csv'"
  )

  params <- unique(unlist(read_csv_doc$parameters))

  illegal_params <- setdiff(names(opts), params)

  if (length(illegal_params) > 0) {
    stop(
      "Illegal duckdb read_csv parameters: ",
      paste0(illegal_params, collapse = ", "),
      call. = FALSE
    )
  }
}

remove_nulls <- function(x) Filter(Negate(is.null), x)

vec_to_string_or_array <- function(x) {

  if (length(x) == 1) {
    return(paste0("'", x, "'"))
  }

  paste0("['", paste0(x, collapse = "','"), "']")
}
