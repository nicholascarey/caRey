`caRey`
================

  - [Installation](#installation)
  - [Functions](#functions)
      - [`progress()`](#progress)
      - [`smooth()`](#smooth)
      - [`peaks()`](#peaks)
  - [Bug reports](#bug-reports)

<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis build
status](https://travis-ci.com/nicholascarey/caRey.svg?branch=master)](https://travis-ci.com/nicholascarey/caRey)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/nicholascarey/caRey?branch=master&svg=true)](https://ci.appveyor.com/project/nicholascarey/caRey)
[![Coverage
status](https://codecov.io/gh/nicholascarey/caRey/branch/master/graph/badge.svg)](https://codecov.io/github/nicholascarey/caRey?branch=master)
[![DOI](https://zenodo.org/badge/277777549.svg)](https://zenodo.org/badge/latestdoi/277777549)

The `caRey` package is a collection of R functions that may be useful in
general data management or processing.

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
    Sys.sleep(0.01) # pause or this example will be too quick
    progress(i, max = 1000, message = "Operation progress")
    }
```

    #> [=========================                         ] 50% Operation progress

#### `smooth()`

A general data smoothing function with several methods including moving
average, splines, and loess regression.

``` r
smooth(sine_noisy.rd, method = "spline")
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="60%" style="display: block; margin: auto;" />

#### `peaks()`

A function that automatically identifies peaks (and troughs) in
oscillating data, with several options for adjusting the detection
sensitivity.

``` r
peaks(swim_y.rd, span = 5, smooth.method = "spline", smooth.n = 0.4, plot.which = "p")
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="60%" style="display: block; margin: auto;" />

### Bug reports

If you find any bugs, weird behaviour, or other issues please [let me
know](mailto:nicholascarey@gmail.com) or [open an
issue](https://github.com/nicholascarey/caRey/issues).
