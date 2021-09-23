#' @title Right substring function.
#'
#' @description
#' Takes a string or numeric value and subsets out the right n characters.
#'
#' ### Details
#' A simple function that extracts the *right* number of specified characters
#' from a string or numeric value.
#'
#' ### Output
#' The returned output will be same class as the input.
#'
#' @seealso \code{\link{left}}
#'
#' @usage
#' right(x, n)
#'
#' @param x numeric value or string.
#' @param n numeric. The number of characters to extract from the right side.
#'
#' @examples
#' right("some text", 4)
#' right(123456, 4)
#'
#' @author Nicholas Carey - \link{nicholascarey@gmail.com}
#' @md
#' @export

right <- function(x, n) {
  ## substring from
  output <- substr(x, nchar(x) - (n - 1), nchar(x))

  ## make same class (i.e. if numeric, make numeric; if text, make text etc.)
  class(output) <- class(x)

  return(output)
}
