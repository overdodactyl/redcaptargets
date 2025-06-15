testthat::test_that("try_tibble returns tibble when tibble is installed", {
  df <- data.frame(x = 1:3)
  result <- try_tibble(df)

  if (rlang::is_installed("tibble")) {
    expect_s3_class(result, "tbl_df")
  } else {
    expect_equal(result, df)
  }
})

testthat::test_that("try_tibble returns original object when tibble is not installed", {
  fake_data <- data.frame(x = 1:3)

  testthat::local_mocked_bindings(
    is_installed = function(pkg) FALSE,
    .package = "rlang"
  )

  result <- try_tibble(fake_data)
  expect_equal(result, fake_data)
})
