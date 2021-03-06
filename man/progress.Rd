% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/progress.R
\name{progress}
\alias{progress}
\title{Display a progress bar in the console}
\usage{
progress(x, max = NULL, message = NULL)
}
\arguments{
\item{x}{numeric. Current iteration of the progress of the operation. This
would be, for example, the \code{i} in a \code{for} loop.}

\item{max}{numeric. Total number of iterations until operation complete. This
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
}
\examples{
\dontrun{
## Simple example
for(i in 1:1000) {
    Sys.sleep(0.005) # pause or it will be too quick
    progress(i, max = 1000)
    }

## Example with custom message
for(i in 1:1000) {
    Sys.sleep(0.005) # pause or it will be too quick
    progress(i, max = 1000, message = "Operation progress")
    }
    }

}
\author{
Nicholas Carey - \email{nicholascarey@gmail.com}
}
