#' Display image generation progress bar
#'
#' @details
#' Displays an updating progress bar in the console. Modified from
#' \url{https://github.com/klmr/rcane/blob/master/system.R}.
#'
#' @export
progress <- function (x, max = 100) {
  percent <- x / max * 100
  cat(sprintf('\r[%-50s] %d%% Generating image files',
              paste(rep('=', percent / 2), collapse = ''),
              floor(percent)))
  if (x == max)
    cat('\n')
}
