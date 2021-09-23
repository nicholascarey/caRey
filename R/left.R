#' @title Left substring function.
#'
#' @description
#' Takes a string or numeric value and subsets out the left n characters.
#'
#' ### Details
#' A simple function that extracts the *left* number of specified characters
#' from a string or numeric value.
#'
#' ### Output
#' The returned output will be same class as the input.
#'
#' @seealso \code{\link{right}}
#'
#' @usage
#' left(x, n)
#'
#' @param x numeric value or string.
#' @param n numeric. The number of characters to extract from the left side.
#'
#' @examples
#' left("some text", 4)
#' left(123456, 4)
#'
#' @author Nicholas Carey - \link{nicholascarey@gmail.com}
#' @md
#' @export

left <- function(x, n) {
  output <- substr(x, 1, n)

  ## make same class
  class(output) <- class(x)

  return(output)
}
