#' Display a progress bar in the console
#'
#' A function for displaying a progress bar in the console. Useful in loops and
#' other lengthy operations where you want some indication of progress or how
#' long the operation will take.
#'
#' @details Displays an updating progress bar in the console in percentage
#'   complete.
#'
#'   To use, place within a code block (e.g. a `for` loop or `apply` call). `x`
#'   should be mapped to the number of each particular iteration (e.g. the `i`),
#'   and `total` is the total expected number of iterations.
#'
#'   The size of the bar adjusts to the width of the R console at the time it is
#'   first run, up to a maximum total width of 80 characters (the general
#'   default R console width). It may look weird if the console width is
#'   subsequently made narrower, though the % done will remain correct.
#'
#' @param x numeric. Current iteration of the progress of the operation. This
#'   would be, for example, the `i` in a `for` loop.
#' @param total numeric. Total number of iterations until operation complete. This
#'   would be, for example, the total `i` in a `for` loop.
#' @param message string. Optional text to add to right side of progress bar.
#'   Should not be more than a few words.
#'
#' @examples
#' \dontrun{
#' ## Simple example
#' for(i in 1:1000) {
#'     Sys.sleep(0.005) # pause or it will be too quick
#'     progress(i, total = 1000)
#'     }
#'
#' ## Example with custom message
#' for(i in 1:1000) {
#'     Sys.sleep(0.005) # pause or it will be too quick
#'     progress(i, total = 1000, message = "Loop progress")
#'     }
#'     }
#'
#' @author Nicholas Carey - \email{nicholascarey@gmail.com}
#' @md
#' @importFrom glue glue
#' @export

progress <- function (x, total = NULL, message = NULL) {

  if(is.null(x) || !is.numeric(x)) stop("progress: valid numeric 'x' input not found.")
  if(is.null(total) || !is.numeric(total)) stop("progress: please enter a numeric 'total' input.")
  if(x > total) message("progress: 'x' input greater than 'total' input.")

  ## if no message, just make it an empty character vector or causes errors later
  if(is.null(message)) message <- ""

  ## current width of console
  wdth <- getOption("width")

  msg_length <- nchar(message) # nchar in message
  buf <- 8 # nchar of % msg (total), spaces and brackets
  # general default character width of R console
  # Don't want to go over this.
  def_nchar <- 80
  total_nchar <- wdth # total char of current console

  # determine final nchar - either total available now, or R default
  if (total_nchar >= def_nchar) final_nchar <- def_nchar else
    if (total_nchar < 80) final_nchar <- total_nchar

  ## number of equals signs required
  n_eq <- final_nchar - msg_length - buf

  ## proportion done in this loop
  prop_done <- x / total

  ## print message
  cat(sprintf(c(glue::glue("\r[%-{n_eq}s] %3d%%"), message),
              paste(rep("=", (n_eq * prop_done)), collapse = ""),
              floor(prop_done*100)))

  ## end
  if (x == total)
    cat("\n")
}

