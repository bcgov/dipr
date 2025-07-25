#' Make crosswalk table of duplicate studyids
#'
#' Creates a table of studyids and duplicate studyids from a "client" table and
#' an extra table of additional studyids that some individuals in the client
#' table may be linked with.
#'
#' This is used to 'restage' data (using the [restage()] function), replacing
#' instances of the duplicate studyid with the "primary" studyid.
#'
#' You have the option to rowwise sort the crosswalk table alphanumerically so
#' the lower is always on the 'left' (`primary_study_id`), and the higher is
#' always on the 'right' (`duplicate_study_id`). We do this because multiple
#' extra id tables (PSSG, Supportive housing) may have the same duplicate IDs
#' but have chosen the opposite one as primary - this way for each distinct pair
#' of related studyids we have a canonical primary and duplicate studyid across
#' datasets. If creating a 'sorted' duplicate id table, then the original data
#' must be restaged in addition to restaging other domain datasets.
#'
#' @param client_data primary dataset containing a client id column and a
#'   `studyid` column
#' @param dup_ids the dataset containing the extra studyids, containing a client
#'   id column and a `studyid` colum.
#' @param clientid_col The name of the client id column. Often `clientid`,
#'   `client_id`, `individual_id` etc. It is assumed that the name of the client
#'   id column is the same in both datasets.
#' @param studyid_col The name of the study id column. This will almost always
#'   be `"studyid"`, which is the default. It is assumed that the name of the
#'  study id column is the same in both datasets.
#' @param drop_clientid should the `clientid` column be dropped and the table be
#'   made distinct only on pairs of `studyid`. Default `TRUE`.
#' @param sorted Should the table have each row sorted alphanumerically so the
#'   lower id is always in the left (`primary_studyid`) column and the higher is
#'   always in the right (`duplicate_studyid`).
#'
#' @return a data.frame of two columns: `primary_studyid` and
#'   `duplicate_studyid`
#' @export
make_dup_id_table <- function(client_data, dup_ids, clientid_col, studyid_col = "studyid", drop_clientid = TRUE, sorted = FALSE) {
  clients_ids <- client_data %>%
    dplyr::select(dplyr::all_of(c(clientid_col, studyid_col))) %>%
    dplyr::rename(primary_studyid = {{studyid_col}}) %>%
    dplyr::distinct()

  dup_ids <- dup_ids %>%
    dplyr::rename(duplicate_studyid = {{studyid_col}}) %>%
    dplyr::distinct()

  extra_id_crosswalk <- clients_ids %>%
    dplyr::inner_join(dup_ids, by = clientid_col) %>%
    dplyr::filter(.data$primary_studyid != .data$duplicate_studyid) %>%
    dplyr::distinct()

  if (sorted) {
    extra_id_crosswalk <- as.data.frame(cbind(
      individual_id = extra_id_crosswalk[[clientid_col]],
      primary_studyid = apply(extra_id_crosswalk[, c("primary_studyid", "duplicate_studyid")], 1, min),
      duplicate_studyid = apply(extra_id_crosswalk[, c("primary_studyid", "duplicate_studyid")], 1, max)
    )) %>%
      dplyr::rename({{clientid_col}} := "individual_id")
  }

  if (drop_clientid) {
    extra_id_crosswalk <- dplyr::select(extra_id_crosswalk, -dplyr::all_of(clientid_col))
  }

  extra_id_crosswalk
}

#' Restage a dataset with reconciled studyids
#'
#' Using an "extra id" datset from [make_dup_id_table()], update the studyids in
#' a dataset with the canonical studyid. This is necessary when doing an
#' analysis with datasets that have been provisioned with an `extra_id`
#' crosswalk table
#'
#' @param data the dataset to restage
#' @param extra_id_data the extra id data (from [make_dup_id_table()]).
#' @param target_col the studyid column in `data` (default `"studyid"`)
#' @param extra_id_source_col the "source" column in `extra_id_data` containing
#'   the "canonical" `studyid`s. Default `"primary_studyid"`.
#' @param extra_id_join_col the column in `extra_id_data` to join to
#'   `target_col` in `data`. Default `duplicate_studyid`.
#' @param distinct_on If the data should be collapsed to one row per individual,
#'   what is the column containin the unique id? Treat this with caution -
#'   should only by used in client roster type datasets where there is only
#'   expected to be a single individual per-row. This selects the first
#'   appearance for each value in `distinct_on` column. It will usually be
#'   `"studyid"`.
#' @param copy If necessary, should the datasets be copied to the same backend.
#'   Can be slow but sometimes necessary. See [dplyr::left_join()]
#' @param count_changed_ids Should the number of unique IDs that were r estaged
#'   be counted? This can be useful, but also computationally expensive on big
#'   datasets in DBI backends, because it will prematurely compute the query
#'   result. Default `TRUE`.
#' @param count_changed_rows Should the number of rows that were added or
#'   removed be counted? This can be useful, but also computationally expensive
#'   on big datasets in DBI backends, because it will prematurely compute the
#'   query result. Default `TRUE`. source for joining.
#' @return `data` with restaged studyids.
#' @export
restage <- function(
    data,
    extra_id_data,
    target_col = "studyid",
    extra_id_source_col = "primary_studyid",
    extra_id_join_col = "duplicate_studyid",
    distinct_on = NULL,
    copy = TRUE,
    count_changed_ids = TRUE,
    count_changed_rows = count_changed_ids) {
  ct_cols <- colnames(extra_id_data)
  data_cols <- colnames(data)

  if (!all(c(extra_id_source_col, extra_id_join_col) %in% ct_cols)) {
    stop(glue::glue("extra_id_data does not contain columns '{extra_id_source_col}' and '{extra_id_join_col}' columns"), call. = FALSE)
  }

  if (!target_col %in% data_cols) stop(glue::glue(".data needs a {target_col} column"), call. = FALSE)

  orig_n_row <- n_row(data)

  data_orig <- data

  data <- dplyr::left_join(
    data,
    extra_id_data,
    by = stats::setNames(extra_id_join_col, target_col),
    copy = copy
  )

  data <- dplyr::mutate(data, {{ target_col }} := dplyr::case_when(
    !is.na(.data[[extra_id_source_col]]) ~ .data[[extra_id_source_col]], ## when source id is not NA that means we replace that record's studyid with it.
    TRUE ~ .data[[target_col]]
  ))

  if (count_changed_ids) {
    num_changed <- dplyr::summarise(
      data,
      n_changed_ids = dplyr::n_distinct(.data[[extra_id_source_col]], na.rm = TRUE)
    ) %>%
      dplyr::pull(.data$n_changed_ids)

    message(glue::glue("*{num_changed} {target_col}s restaged*"))
  }

  data <- dplyr::select(data, -dplyr::all_of(extra_id_source_col))

  # Treat this with caution - should only by used in client roster type datasets
  # where there is only expected to be a single individual per-row. This
  # selects the first appearance for each value of `distinct_on`
  if (!is.null(distinct_on)) {
    data <- dplyr::distinct(data, dplyr::across(dplyr::all_of(distinct_on)), .keep_all = TRUE)
  } else {
    data <- dplyr::distinct(data)
  }

  if (count_changed_rows) {
    new_nrow <- n_row(data)

    if (!identical(new_nrow, orig_n_row)) {
      add_r <- new_nrow - orig_n_row
      warning(paste0("you've added ", add_r, " rows"), call. = FALSE)
    }
  }

  data
}

#' Global nrow() replacement
#'
#' @param x A data.frame, an Arrow tabular object, a duckplyr data frame, or a
#' lazy sql data frame
#'
#' @return a single number with the number of rows
#' @export
n_row <- function(x) {
  if (inherits(x, c("duckplyr_df", "tbl_lazy", "Dataset", "arrow_dplyr_query", "Table"))) {
    return(dplyr::collect(dplyr::count(x))[[1]])
  }
  nrow(x)
}
