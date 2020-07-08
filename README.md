`caRey`
================

  - [Installation](#installation)
  - [Functions](#functions)
      - [`progress()`](#progress)
  - [Bug reports](#bug-reports)

<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis build
status](https://travis-ci.com/nicholascarey/caRey.svg?branch=master)](https://travis-ci.com/nicholascarey/caRey)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/nicholascarey/caRey?branch=master&svg=true)](https://ci.appveyor.com/project/nicholascarey/caRey)
[![Coverage
status](https://codecov.io/gh/nicholascarey/caRey/branch/master/graph/badge.svg)](https://codecov.io/github/nicholascarey/caRey?branch=master)
[![DOI](https://zenodo.org/badge/277777549.svg)](https://zenodo.org/badge/latestdoi/277777549)

The `caRey` package is a collection of general R functions that may be
useful in general data management or processing.

### Installation

`caRey` can be installed using the `devtools` package:

``` r
install.packages("devtools")
devtools::install_github("nicholascarey/caRey")
```

### Functions

#### `progress()`

A simple progress bar function for use in loops or operations where you
want some indication of progress and how long it will take.

``` r
## Simple example with custom message
for(i in 1:1000) {
    Sys.sleep(0.01) # pause or it will be too quick
    progress(i, max = 1000, message = "Operation progress")
    }
```

    #> [=========================                         ] 50% Operation progress

### Bug reports

If you find any bugs, weird behaviour, or other issues please [let me
know](mailto:nicholascarey@gmail.com) or [open an
issue](https://github.com/nicholascarey/caRey/issues).
