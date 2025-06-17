library(targets)
library(redcaptargets)

# Establish REDCap Connection
redcap_con <- redcapAPI::redcapConnection(
  url = Sys.getenv("REDCAP_URL"),
  token = Sys.getenv("REDCAP_INTERNAL_82203")
)

# Source functions in R/ directory
tar_source()

# Targets pipeline
list(
  tar_redcap(redcap, redcap_con),
  tar_target(
    n_enrolled,
    command = nrow(redcap_demographics),
    description = "Enrollment Count"
  )
)
