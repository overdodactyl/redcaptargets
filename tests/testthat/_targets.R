library(targets)
list(tar_redcap(name = redcap, con = con, instruments = c("form_a",
    "form_b"), fetch_records = fetch_records, mode = "always"))
