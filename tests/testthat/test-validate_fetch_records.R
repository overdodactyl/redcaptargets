testthat::test_that("validate_fetch_records passes when function has required arguments", {
  good_fun <- function(con, forms) {}
  expect_invisible(validate_fetch_records(good_fun))
})

testthat::test_that("validate_fetch_records passes for default function", {
  expect_invisible(validate_fetch_records(redcap_fetch_records))
})

testthat::test_that("validate_fetch_records fails when missing 'con'", {
  bad_fun <- function(forms) {}
  expect_error(
    validate_fetch_records(bad_fun),
    "fetch_records.*missing.*con"
  )
})

testthat::test_that("validate_fetch_records fails when missing 'forms'", {
  bad_fun <- function(con) {}
  expect_error(
    validate_fetch_records(bad_fun),
    "fetch_records.*missing.*forms"
  )
})

testthat::test_that("validate_fetch_records fails when missing both 'con' and 'forms'", {
  bad_fun <- function(...) {}
  expect_error(
    validate_fetch_records(bad_fun),
    "fetch_records.*missing.*con.*forms"
  )
})
