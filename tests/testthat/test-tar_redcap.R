targets::tar_test("tar_redcap() pulls all forms and metadata", {
  skip_if_not_installed("redcapAPI")
  skip_if_not_installed("tarchetypes")

  redcap_url <- Sys.getenv("REDCAP_URL")
  redcap_token <- Sys.getenv("REDCAP_INTERNAL_82203")

  if (redcap_url == "" || redcap_token == "") {
    skip("REDCAP_URL and/or REDCAP_INTERNAL_82203 not set.")
  }

  con <- redcapAPI::redcapConnection(url = redcap_url, token = redcap_token)

  targets::tar_script({
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    list(
      redcaptargets::tar_redcap(
        name = redcap,
        con = con,
        instruments = NULL,
        mode = "always"
      )
    )
  })

  suppressWarnings(targets::tar_make(callr_function = NULL))
  manifest <- targets::tar_manifest(callr_function = NULL)

  expected_targets <- c(
    "redcap_demographics",
    "redcap_labs",
    "redcap_visits",
    "redcap_meta_db"
  )

  expect_true(all(expected_targets %in% manifest$name))

})
