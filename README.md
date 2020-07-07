`caRey`
================

  - [Installation](#installation)
  - [Functions](#functions)
      - [`progress()`](#progress)
  - [Future functionality](#future-functionality)
  - [Bug reports](#bug-reports)

<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis build
status](https://travis-ci.com/nicholascarey/caRey.svg?branch=master)](https://travis-ci.com/nicholascarey/caRey)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/nicholascarey/caRey?branch=master&svg=true)](https://ci.appveyor.com/project/nicholascarey/caRey)
[![Coverage
status](https://codecov.io/gh/nicholascarey/caRey/branch/master/graph/badge.svg)](https://codecov.io/github/nicholascarey/caRey?branch=master)
[![DOI](https://zenodo.org/badge/277777549.svg)](https://zenodo.org/badge/latestdoi/277777549)

The `caRey` package is a collection of R functions. I will add to it
periodically ([suggestions
welcome](https://github.com/nicholascarey/caRey/issues)). It is not
intended to be a fully featured package, more a collection of handy
functions.

If you use it a citation using this [Zenodo
DOI](https://zenodo.org/badge/latestdoi/277777549) would be much
appreciated.

### Installation

`caRey` can be installed using the `devtools` package:

``` r
install.packages("devtools")
devtools::install_github("nicholascarey/caRey")
```

### Functions

#### `progress()`

Progress bar function.

``` r
## Simple example with custom message

for(i in 1:1000) {
    Sys.sleep(0.01) # pause or it will be too quick
    progress(i, max = 1000, message = "Operation progress")
    }
```

    #> [=========================                         ] 50% Operation progress

### Future functionality

In due course I’ll add a few more functions. Suggestions for additional
functions are welcome via [email](mailto:nicholascarey@gmail.com), or by
[opening an issue](https://github.com/nicholascarey/caRey/issues).

### Bug reports

If you find any bugs, weird behaviour, or other issues please [let me
know](mailto:nicholascarey@gmail.com) or [open an
issue](https://github.com/nicholascarey/caRey/issues).
