test_that("returns all instruments when input is NULL", {
  mock_fetch <- function(con) {
    data.frame(name = c("demographics", "labs", "visits"))
  }

  con <- structure(list(), class = "redcapApiConnection")

  testthat::local_mocked_bindings(
    fetch_redcap_instruments = mock_fetch,
    .package = "redcaptargets"
  )

  result <- resolve_redcap_instruments(NULL, con)
  expect_equal(result$name, c("demographics", "labs", "visits"))
})

test_that("returns specified instruments when valid", {
  mock_fetch <- function(con) {
    data.frame(name = c("demographics", "labs", "visits"))
  }

  con <- structure(list(), class = "redcapApiConnection")

  testthat::local_mocked_bindings(
    fetch_redcap_instruments = mock_fetch,
    .package = "redcaptargets"
  )

  result <- resolve_redcap_instruments(c("labs", "visits"), con)
  expect_equal(result$name, c("labs", "visits"))
})

test_that("errors when instruments contains unknown values", {
  mock_fetch <- function(con) {
    data.frame(name = c("demographics", "labs", "visits"))
  }

  con <- structure(list(), class = "redcapApiConnection")

  testthat::local_mocked_bindings(
    fetch_redcap_instruments = mock_fetch,
    .package = "redcaptargets"
  )

  expect_error(
    resolve_redcap_instruments(c("labs", "nonexistent"), con),
    "Unknown instrument name\\(s\\) detected"
  )
})

test_that("errors when instruments is not a character vector", {
  mock_fetch <- function(con) {
    data.frame(name = c("demographics", "labs", "visits"))
  }

  con <- structure(list(), class = "redcapApiConnection")

  testthat::local_mocked_bindings(
    fetch_redcap_instruments = mock_fetch,
    .package = "redcaptargets"
  )

  expect_error(
    resolve_redcap_instruments(123, con),
    "`instruments` must be a character vector"
  )
})
