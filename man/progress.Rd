% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/progress.R
\name{progress}
\alias{progress}
\title{Display a progress bar in the console}
\usage{
progress(x, total = NULL, message = NULL)
}
\arguments{
\item{x}{numeric. Current iteration of the progress of the operation. This
would be, for example, the \code{i} in a \code{for} loop.}

\item{total}{numeric. Total number of iterations until operation complete. This
would be, for example, the total \code{i} in a \code{for} loop.}

\item{message}{string. Optional text to add to right side of progress bar.
Should not be more than a few words.}
}
\description{
A function for displaying a progress bar in the console. Useful in loops and
other lengthy operations where you want some indication of progress or how
long the operation will take.
}
\details{
Displays an updating progress bar in the console in percentage
complete.

To use, place within a code block (e.g. a \code{for} loop or \code{apply} call). \code{x}
should be mapped to the number of each particular iteration (e.g. the \code{i}),
and \code{total} is the total expected number of iterations.

The size of the bar adjusts to the width of the R console at the time it is
first run, up to a maximum total width of 80 characters (the general
default R console width). It may look weird if the console width is
subsequently made narrower, though the \% done will remain correct.
}
\examples{
\dontrun{
## Simple example
for(i in 1:1000) {
    Sys.sleep(0.005) # pause or it will be too quick
    progress(i, total = 1000)
    }

## Example with custom message
for(i in 1:1000) {
    Sys.sleep(0.005) # pause or it will be too quick
    progress(i, total = 1000, message = "Loop progress")
    }
    }

}
\author{
Nicholas Carey - \email{nicholascarey@gmail.com}
}
