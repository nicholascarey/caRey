% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/left.R
\name{left}
\alias{left}
\title{Left substring function.}
\usage{
left(x, n)
}
\arguments{
\item{x}{numeric value or string.}

\item{n}{numeric. The number of characters to extract from the left side.}
}
\description{
Takes a string or numeric value and subsets out the left n characters.
\subsection{Details}{

A simple function that extracts the \emph{left} number of specified characters
from a string or numeric value.
}

\subsection{Output}{

The returned output will be same class as the input.
}
}
\examples{
left("some text", 4)
left(123456, 4)

}
\seealso{
\code{\link{right}}
}
\author{
Nicholas Carey - \email{nicholascarey@gmail.com}
}
