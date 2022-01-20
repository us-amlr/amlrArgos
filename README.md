# argos-r
R code used by the AERD for processing Argos data

## Installation

You can install the development version of amlrPinnipeds from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("us-amlr/amlrArgos")
```

## Usage

The following file paths are simply examples; this code cannot be run as-is. 

``` r
library(amlrArgos)

# Run for all new data, without the speed filter
x <- import_argos(
  UPLOAD = FALSE, FORMAT.ARGOS = TRUE, SPEED.FILTER = FALSE, 
  argos.csv.path = "processing/input", 
  trackdata.file = "", 
  export.file = "ArgosData_2022_01_20_02_13_00_output.csv", 
  log.file = "PTTlog.csv"
)

# Run for all new data, with the speed filter
x.speed <- import_argos(
  UPLOAD = FALSE, FORMAT.ARGOS = TRUE, SPEED.FILTER = TRUE, 
  argos.csv.path = "processing/input", 
  trackdata.file = "", 
  export.file = "ArgosData_2022_01_20_02_13_00_output.csv", 
  log.file = "PTTlog.csv"
)
```

## Disclaimer

This repository is a scientific product and is not official
communication of the National Oceanic and Atmospheric Administration, or
the United States Department of Commerce. All NOAA GitHub project code
is provided on an ‘as is’ basis and the user assumes responsibility for
its use. Any claims against the Department of Commerce or Department of
Commerce bureaus stemming from the use of this GitHub project will be
governed by all applicable Federal law. Any reference to specific
commercial products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by the Department of Commerce.
The Department of Commerce seal and logo, or the seal and logo of a DOC
bureau, shall not be used in any manner to imply endorsement of any
commercial product or activity by DOC or the United States Government.
