#' Validate REDCap connection object
#'
#' Ensures the object is a redcapAPI connection.
#'
#' @param con Object to validate.
#' @keywords internal
validate_redcap_connection <- function(con) {
  if (!inherits(con, "redcapApiConnection")) {
    cli::cli_abort(c(
      "!" = "Invalid {.arg con} supplied.",
      "x" = "Must be a {.cls redcapApiConnection} object from {.pkg redcapAPI}.",
      "i" = "Use {.fun redcapAPI::redcapConnection} to create one."
    ))
  }
  invisible(TRUE)
}

#' Determine whether REDCap metadata should be refreshed based on logs
#'
#' Checks the REDCap project logging for changes to metadata (excluding
#' exports/downloads) since the last time a metadata target was built.
#'
#' Used internally by [tar_redcap()] when `mode = "logging"` to decide
#' whether the metadata target should be rerun.
#'
#' @param name A character vector of target names corresponding to the
#'   REDCap metadata target(s). Typically of the form `"<stem>_meta_db"`.
#' @param con A `redcapAPI::redcapConnection` object.
#'
#' @return Logical: `TRUE` if metadata has changed since last pull and the
#'   target should be rebuilt, `FALSE` otherwise.
#'
#' @keywords internal
redcap_meta_force <- function(name, con) {
  last_meta_pull <- targets::tar_meta(targets::any_of(name))$time

  meta_logs <- redcapAPI::exportLogging(
    con,
    logtype = "manage",
    beginTime = last_meta_pull
  )

  meta_logs <- meta_logs[!grepl("Export|Download", meta_logs$details), ]

  nrow(meta_logs) > 0
}

#' Validate that the `fetch_records` function has required arguments
#'
#' Ensures that the user-supplied `fetch_records` function includes the
#' required arguments `con` and `forms`. Helps avoid runtime errors when
#' dynamic targets call the function inside [`tar_redcap()`].
#'
#' @param fetch_records A function meant to be passed to the `fetch_records`
#'   argument of [tar_redcap()]. It must accept at least `con` and `forms`.
#'
#' @return Invisibly returns `TRUE` if validation passes; otherwise an
#'   informative error is thrown.
#'
#' @keywords internal
validate_fetch_records <- function(fetch_records) {
  formals  <- names(formals(fetch_records))
  required <- c("con", "forms")
  missing  <- setdiff(required, formals)

  if (length(missing) > 0) {
    cli::cli_abort(c(
      "!" = "{.arg fetch_records} is missing required argument{?s}: {.val {missing}}",
      "i" = "It must accept {.val con} and {.val forms} so that {.fun tar_redcap} can",
      "  call it like {.code fetch_records(forms = form_name, con = con, ...)}."
    ))
  }

  invisible(TRUE)
}

#' Determine if REDCap instruments should be re-fetched
#'
#' Checks REDCap project logs for relevant record-level changes since the last
#' pull of any specified instrument targets. This is used in `mode = "logging"`
#' to conditionally trigger target invalidation based on recent changes.
#'
#' @param names A character vector of REDCap target names (e.g., `redcap_formA`).
#'   Used to determine the most recent run time of each corresponding target.
#' @param con A `redcapAPI::redcapConnection` object.
#'
#' @return A logical scalar: `TRUE` if REDCap logs suggest the data may have
#'   changed and should be re-fetched; `FALSE` otherwise.
#'
#' @keywords internal
redcap_instrument_force <- function(names, con) {
  last_meta_pull <- targets::tar_meta(targets::any_of(names))
  if (nrow(last_meta_pull) == 0) return(TRUE)
  last_meta_pull <- min(last_meta_pull$time, na.rm = FALSE)

  if (is.na(last_meta_pull)) return(TRUE)

  meta_logs <- redcapAPI::exportLogging(
    con,
    logtype = "record",
    beginTime = last_meta_pull
  )

  nrow(meta_logs) > 0
}

#' Fetch REDCap instrument names from a project
#'
#' Queries the REDCap project for its list of instruments (forms) and returns
#' a data frame with a column `name` that can be used for dynamic branching in
#' `targets` pipelines.
#'
#' This function is used internally by [tar_redcap()] to determine which
#' instruments to pull.
#'
#' @param con A `redcapAPI::redcapConnection` object.
#'
#' @return A data frame with one column, `name`, containing the instrument/form names.
#'
#' @keywords internal
fetch_redcap_instruments <- function(con) {
  instruments <- redcapAPI::exportInstruments(con)
  instruments[["instrument_name"]]
}

#' Convert a data object to a tibble if `tibble` is installed
#'
#' A lightweight helper that attempts to coerce `.data` to a tibble
#' using [tibble::as_tibble()]—but only if the `tibble` package is installed.
#' Otherwise, it silently returns the input unmodified.
#'
#' This is useful for optional tibble formatting without forcing `tibble`
#' to be a hard dependency of the package.
#'
#' @param .data A data frame–like object.
#'
#' @return A tibble (if `tibble` is installed) or the original object.
#'
#' @keywords internal
try_tibble <- function(.data) {

  if (rlang::is_installed("tibble")) {
    .data <- tibble::as_tibble(.data)
  }

  .data
}

#' Fetch records for one or more REDCap forms
#'
#' This is the default record-fetching function used by [tar_redcap()]. It uses
#' `redcapAPI::exportRecordsTyped()` to pull records for a given form or set of
#' forms. Additional arguments can be passed via `...` (e.g., `batch_size`,
#' `drop_fields`, `labels`).
#'
#' @param con A `redcapAPI::redcapConnection` object.
#' @param forms A character vector of one or more REDCap form names.
#' @param ... Additional arguments passed to `redcapAPI::exportRecordsTyped()`,
#'   such as `batch_size`, `labels`, or `drop_fields`.
#'
#' @return A data frame (or tibble if the `tibble` package is installed) containing
#'   the REDCap records.
#'
#' @export
redcap_fetch_records <- function(con, forms, ...) {
  args <- list(...)
  res <- redcapAPI::exportRecordsTyped(
    con,
    forms = forms,
    ...
  )
  try_tibble(res)
}


redcap_fetch_meta <- function(con) {
  redcapAPI::exportMetaData(con) |>
    try_tibble()
}

#' Resolve target cue settings for REDCap targets
#'
#' Determines how targets should be invalidated (`re-run`) based on the
#' specified mode. Used internally by [tar_redcap()] to assign cue behavior
#' for both the metadata target and instrument-level data targets.
#'
#' @param mode A character string indicating cue mode. One of:
#'   - `"logging"`: use REDCap project logs to detect changes.
#'   - `"always"`: always re-fetch data regardless of changes.
#'   - `"thorough"`: use default `targets` metadata-based invalidation.
#'   - `"never"`: never re-run unless missing or failed.
#' @param name_meta Name of the metadata target (used to check last run time).
#' @param instrument_names A character vector of instrument target names.
#' @param con A `redcapAPI::redcapConnection` object.
#'
#' @return A named list with `meta` and `inst` cue objects to use in
#'   [tar_target()] or [tarchetypes::tar_map()].
#'
#' @keywords internal
redcap_resolve_cues <- function(mode, name_meta, instrument_names, con) {
  switch(
    mode,
    logging = {
      meta_force <- redcap_meta_force(name_meta, con)
      inst_force <- redcap_instrument_force(instrument_names, con)
      tar_cue_meta <- tarchetypes::tar_cue_force(meta_force)
      tar_cue_inst <- tarchetypes::tar_cue_force(inst_force)
      list(meta = tar_cue_meta, inst = tar_cue_inst)
    },
    always = list(
      meta = tarchetypes::tar_cue_force(TRUE),
      inst = tarchetypes::tar_cue_force(TRUE)
    ),
    thorough = list(
      meta = targets::tar_cue(mode = "thorough"),
      inst = targets::tar_cue(mode = "thorough")
    ),
    never = list(
      meta = targets::tar_cue(mode = "never"),
      inst = targets::tar_cue(mode = "never")
    )
  )
}

#' Validate and resolve REDCap instruments to pull
#'
#' Ensures that the list of instruments is either NULL (in which case all
#' instruments are fetched) or a valid character vector of instrument names.
#'
#' @param instruments NULL or character vector of instrument names to fetch.
#' @param con A `redcapAPI::redcapConnection` object.
#'
#' @return A data frame with a `name` column (same structure as `fetch_redcap_instruments()`).
#' @keywords internal
resolve_redcap_instruments <- function(instruments, con) {
  if (is.null(instruments)) {
    instruments <- fetch_redcap_instruments(con)
  } else {
    all_forms <- fetch_redcap_instruments(con)$name

    if (!is.character(instruments)) {
      cli::cli_abort(c(
        "!" = "{.arg instruments} must be a character vector of form names or {.code NULL}.",
        "x" = "You supplied an object of class {.cls {class(instruments)}}."
      ))
    }

    unknown <- setdiff(instruments, all_forms)
    if (length(unknown) > 0) {
      cli::cli_abort(c(
        "!" = "Unknown instrument name(s) detected.",
        "x" = "The following instrument(s) are not in the REDCap project:",
        "*" = paste0(unknown, collapse = ", ")
      ))
    }
  }

  data.frame(name = instruments, stringsAsFactors = FALSE)
}

#' Create dynamic REDCap targets for a `targets` pipeline
#'
#' `tar_redcap()` builds a set of dynamic `targets` to pull REDCap metadata and
#' form-level data using the `redcapAPI` backend. It supports dynamic branching
#' across all instruments and offers flexible control over when data is re-fetched
#' via `cue` behavior (e.g., logging-based updates, always re-fetching, or
#' never rerunning unless missing).
#'
#' @param name Symbol. A name stem for the REDCap targets (e.g., `redcap` will
#'   generate `redcap_meta_db` and one target per instrument prefixed with `redcap_`).
#' @param con A `redcapAPI::redcapConnection` object.
#' @param mode Character string indicating how target invalidation should occur:
#'   - `"logging"`: re-run only when REDCap logs show changes (requires user access to logs).
#'   - `"always"`: always re-run REDCap pulls regardless of changes.
#'   - `"thorough"`: default `targets` invalidation (based on dependencies and metadata).
#'   - `"never"`: never re-run unless missing or errored previously.
#' @param fetch_records A function used to fetch records for each REDCap form.
#'   Must accept arguments `con` and `forms`, along with additional optional
#'   arguments via `...`. Defaults to [redcap_fetch_records()].
#' @param instruments Optional character vector of instrument (form) names to fetch from REDCap.
#'   If `NULL` (default), all available instruments will be fetched using [fetch_redcap_instruments()].
#'   If supplied, the values must be valid instrument names present in the REDCap project. This allows
#'   selective fetching of only a subset of forms.
#' @param ... Additional arguments passed to `fetch_records`, such as
#'   `batch_size`, `drop_fields`, or label formatting options.
#'
#' @return A list of `tar_target()` and `tar_map()` objects, including:
#'   - `<name>_meta_db`: a target for REDCap metadata.
#'   - One dynamic target per REDCap instrument, each named `<name>_<instrument>`.
#'
#' @examples
#' \dontrun{
#' tar_redcap(
#'   name = redcap,
#'   con = redcap_con,
#'   mode = "logging",
#'   batch_size = 1000
#' )
#'
#' # Custom record fetcher example:
#' custom_fetch <- function(con, forms, ...) {
#'   redcapAPI::exportRecords(con, forms = forms, labels = TRUE, ...)
#' }
#'
#' tar_redcap(
#'   name = redcap,
#'   con = redcap_con,
#'   mode = "always",
#'   fetch_records = custom_fetch
#' )
#' }
#'
#' @export
tar_redcap <- function(name, con,
                       mode = c("logging", "always", "thorough", "never"),
                       fetch_records = redcap_fetch_records,
                       instruments = NULL,
                       ...) {
  con_obj <- con
  con_sym <- as.symbol(deparse(substitute(con)))

  mode <- rlang::arg_match(mode, c("logging", "always", "thorough", "never"))
  dots <- rlang::list2(...)

  validate_fetch_records(fetch_records)
  validate_redcap_connection(con_obj)

  name_sym <- rlang::ensym(name)
  name_str <- rlang::as_string(name_sym)
  name_meta <- paste0(name_str, "_meta_db")

  instruments_df <- resolve_redcap_instruments(instruments, con_obj)
  name_instruments <- paste0(name_str, "_", instruments_df$name)

  cues <- redcap_resolve_cues(mode, name_meta, name_instruments, con_obj)

  rlang::list2(
    targets::tar_target_raw(
      name = name_meta,
      command = substitute(
        redcap_fetch_meta(con),
        env = list(con = con_sym)
      ),
      cue = cues$meta,
      description = "REDCap Metadata"
    ),
    !!!tarchetypes::tar_map(
      values = instruments_df,
      names = "name",
      targets::tar_target_raw(
        name_str,
        command = rlang::call2(
          fetch_records,
          forms = rlang::sym("name"),
          con = con_sym,
          !!!dots
        ),
        cue = cues$inst,
        description = "REDCap Data:"
      ),
      unlist = FALSE
    )
  )
}
