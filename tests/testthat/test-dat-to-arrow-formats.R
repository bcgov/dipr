context('test conversion to Apache Arrow formats')

test_that("dat_to_arrow_format works",{
  dat_to_arrow_formats(
    data_path = dat_path,
    data_dict = dict,
    arrow_format = 'parquet',
    output_dir = out_parquet
  )

  expect_true(file.exists(out_parquet))
  # expect_equal(
  #   arrow::read_parquet(out_parquet),
  #   read_dat(dipr_example("starwars-fwf.dat.gz"), dict, as.data.table = FALSE, use_cache = FALSE)
  # )
})

test_that('output_dir is created',{
  skip("only locally for now")
  outd <- file.path(tempdir(), "test_create")
  expect_false(dir.exists(outd))


  converted <- dat_to_arrow_formats(
    data_path = dat_path,
    data_dict = dict,
    arrow_format = 'parquet',
    output_dir = outd
  )

  expect_true(dir.exists(outd))
  #delete_rtmp_dirs()
})


test_that('dat_to_datasets creates a partioned dataset',{

  in_mem <- read_dat(data_path = dat_path,
           data_dict = dict)

  outd <- file.path(tempdir(), "test_arrow")
  dat_to_datasets(
    data_path = dat_path,
    data_dict = dict,
    path = outd,
    partitioning = "species",
    chunk_size = 2)
  expect_is(arrow::open_dataset(outd), "FileSystemDataset")
  expect_equal(nrow(in_mem), nrow(arrow::open_dataset(outd)))
})
