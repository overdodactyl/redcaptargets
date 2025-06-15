test_that("validate_redcap_connection accepts a valid connection object", {
  dummy_con <- structure(list(), class = c("redcapApiConnection", "redcapConnection"))
  expect_invisible(validate_redcap_connection(dummy_con))
})

test_that("validate_redcap_connection errors for invalid input", {
  expect_error(
    validate_redcap_connection("not a connection"),
    class = "rlang_error",
    regexp = "Invalid `con` supplied"
  )
})
