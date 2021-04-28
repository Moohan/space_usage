test_that("get full names returns as tibble and writes data", {
  full_names <- get_full_names()
  full_names_path <- fs::path(tempdir(), "user_full_names.rds")

  expect_s3_class(full_names, "tbl_df")
  expect_equal(ncol(full_names), 2)
  expect_gt(nrow(full_names), 100)

  expect_true(fs::file_exists(full_names_path))
  expect_identical(full_names, readRDS(full_names_path))
})
