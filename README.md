
<!-- README.md is generated from README.Rmd. Please edit that file -->

# redcaptargets <img src="man/figures/logo.png" align="right" width="100" height="100"/>

<!-- badges: start -->

[![R
Targetopia](https://img.shields.io/badge/R_Targetopia-member-blue?style=flat&labelColor=gray)](https://wlandau.github.io/targetopia/)
[![R-CMD-check](https://github.com/overdodactyl/redcaptargets/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/overdodactyl/redcaptargets/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

`redcaptargets` integrates [REDCap](https://project-redcap.org/)
databases with the [`targets`](https://books.ropensci.org/targets/)
pipeline toolkit to streamline reproducible clinical research workflows.
It leverages the
[`redcapAPI`](https://cran.r-project.org/web/packages/redcapAPI/index.html)
package to connect to REDCap and automatically generates dynamic targets
for both metadata and instrument-level data. By leveraging REDCap
logging, redcaptargets detects meaningful changes and re-pulls data only
when needed, minimizing compute time and ensuring local copies remain up
to date. Modular factory functions minimize boilerplate and adapt
seamlessly to evolving study designs.

## Motivation

REDCap doesn’t support SQL-like queries, so users often resort to
pulling *all* data across all instruments into their workspace for
analysis. This leads to slow, repetitive code, especially in
longitudinal studies where:

- New participants enroll continuously

- Forms and surveys evolve

- Analyses need frequent refreshes

`redcaptargets` simplifies this by integrating REDCap with the targets
pipeline toolkit:

- Automatically detects meaningful changes via REDCap logs

- Dynamically creates targets for each instrument and metadata

- Skips unnecessary pulls to save time and compute

- Enables modular, reproducible workflows with built-in caching via
  gittargets

## Example

This example shows how to use `tar_redcap()`—the main function in
`redcaptargets`—to pull all REDCap instruments and metadata into a
targets pipeline.

#### **\_targets.R: Minimal example**

``` r
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
```

#### Trigger the pipeline

``` r
targets::tar_make()
#> + redcap_visits dispatched
#> ✔ redcap_visits completed [1.5s, 432 B]
#> + redcap_labs dispatched
#> ✔ redcap_labs completed [224ms, 477 B]
#> + redcap_meta_db dispatched
#> ✔ redcap_meta_db completed [334ms, 568 B]
#> + redcap_demographics dispatched
#> ✔ redcap_demographics completed [458ms, 623 B]
#> + redcap_record_id dispatched
#> ✔ redcap_record_id completed [0ms, 62 B]
#> + n_enrolled dispatched
#> ✔ n_enrolled completed [0ms, 48 B]
#> ✔ ended pipeline [2.7s, 6 completed, 0 skipped]
```

#### View generated targets

Each instrument in the REDCap project will have its own target, along
with one for metadata:

``` r
manifest <- targets::tar_manifest()
manifest[c("name", "description")]
#> # A tibble: 6 × 2
#>   name                description              
#>   <chr>               <chr>                    
#> 1 redcap_visits       REDCap Data: visits      
#> 2 redcap_labs         REDCap Data: labs        
#> 3 redcap_meta_db      REDCap Metadata          
#> 4 redcap_demographics REDCap Data: demographics
#> 5 redcap_record_id    REDCap Record ID         
#> 6 n_enrolled          Enrollment Count
```
