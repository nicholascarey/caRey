#' Display a progress bar in the console
#'
#' A function for displaying a progress bar in the console. Useful in loops and
#' other lengthy operations where you want some indication of progress or how
#' long the operation will take.
#'
#' @details Displays an updating progress bar in the console in percentage
#'   complete.
#'
#' @param x numeric. Current iteration of the progress of the operation. This
#'   would be, for example, the `i` in a `for` loop.
#' @param max numeric. Total number of iterations until operation complete. This
#'   would be, for example, the total `i` in a `for` loop.
#' @param message string. Optional text to add to right side of progress bar.
#'   Should not be more than a few words.
#'
#' @examples
#' ## Simple example
#' for(i in 1:1000) {
#'     Sys.sleep(0.01) # pause or it will be too quick
#'     progress(i, max = 1000)
#'     }
#'
#' ## Example with custom message
#' for(i in 1:1000) {
#'     Sys.sleep(0.01) # pause or it will be too quick
#'     progress(i, max = 1000, message = "Operation progress")
#'     }
#'
#' @author Nicholas Carey - \email{nicholascarey@gmail.com}
#' @md
#' @export

progress <- function (x, max = NULL, message = NULL) {

  if(is.null(x) || !is.numeric(x)) stop("progress: valid numeric 'x' input not found.")
  if(is.null(max) || !is.numeric(max)) stop("progress: please enter a numeric 'max' input.")
  if(x > max) stop("progress: 'x' input greater than 'max' input.")

  percent <- x / max * 100
  cat(sprintf(c("\r[%-50s] %d%%", message),
              paste(rep("=", percent / 2), collapse = ""),
              floor(percent)))
  if (x == max)
    cat("\n")
}
