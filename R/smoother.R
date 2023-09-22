#'@title Smooth noisy data
#'
#'@description Simple smoothing function with several methods.
#'
#'@details Smooths a vector using one of four methods. The `n` input controls
#'  the degree of smoothing. If `n` is between 0 and 1 it represents a
#'  proportion of the data length (i.e. 0.1 is a window of 10% of the total
#'  data). If `n` is above 1 it specifies a window of a number of data values
#'  (i.e. `n = 20` in a dataset of 200 values would represent a window of 10% of
#'  the data length). The same `n` value will have radically different results
#'  with different methods.
#'
#'  For all methods a vector of equal length as `x` containing the smoothed
#'  values is returned.
#'
#'  **`method = "movav"`**
#'
#'  A moving average (rolling mean). Good for data which is noisy, but which
#'  does not oscillate sharply. `n` controls the window size for the moving
#'  average. For each data value, half the window width forward and half
#'  backward are averaged to produce the smoothed values. At the start and end
#'  of the vector where n/2 would extend beyond the start or end, the available
#'  data points are averaged.
#'
#'  **`method = "loess"`**
#'
#'  Locally estimated scatterplot smoothing. Good for data which oscillates in a
#'  non-predictable manner. The `n` input is parsed to be the \code{span}
#'  operator of the `loess` function. For more control over smoothing using this
#'  method, see [loess()].
#'
#'  **`method = "spline"`**
#'
#'  Cubic smoothing spline. Good for data which oscillates in a somewhat regular
#'  manner, such as a sinusoidal wave. The `n` input is parsed to be the
#'  \code{spar} operator of the `smooth.spline` function. If `n = NULL`, the
#'  `smooth.spline` 'cross-validation' option is used to estimate the 'best'
#'  `spar` value between 0 and 1, though this is not returned and results may
#'  vary. For more control over smoothing using this method, see
#'  [smooth.spline()].
#'
#'  **`method = "supersmooth"`**
#'
#'  Friedman's SuperSmoother. Good for data which oscillates in a somewhat
#'  regular manner, such as a sinusoidal wave. The `n` input is parsed to be the
#'  \code{span} operator of the `supsmu` function. If `n = NULL`, the `supsmu`
#'  'cross-validation' option is used to estimate the 'best' `span` value
#'  between 0 and 1, though this is not returned and results may vary. Smoothing
#'  performs well at much lower `n` values in comparison to other methods. See
#'  examples. For more control over smoothing using this method, see [supsmu()].
#'
#'@section Plotting: If `plot = TRUE` a three panel plot is produced of the
#'  original data values, the smoothed data values, and the smoothed data
#'  plotted as a line over the original data. Each single panel can be plotted
#'  alone using the `panel` argument (e.g. `panel = 3`).
#'
#'@usage smoother(x, n = NULL, method = "movav", plot = TRUE, panel = "all")
#'
#'@param x numeric. A numeric vector.
#'@param n numeric. Smoothing factor. See Details.
#'@param method string. Smoothing method: `"movav"`, `"loess"`, `"spline"`, or
#'  `"supersmooth"`.
#'@param plot logical. Plots the result.
#'@param panel Which panel(s) of the plot to show. Default is `"all"`, other
#'  options are `1`, `2` or `3`. See Details.
#'
#'@return Returns a vector of smoothed values of the same length as the input.
#'
#' @examples
#' ## Moving average smoothing
#' ## Using a 10% rolling window
#' smoother(resp_noisy.rd, n = 0.1, method = "movav")
#' ## Same size window expressed as number of data values
#' smoother(resp_noisy.rd, n = 94, method = "movav")
#'
#' ## Loess smoothing
#' ## Good fit to the data, but doesn't capture the sinusoidal structure that well
#' smoother(sine_noisy.rd, n = 0.1, method = "loess")
#' ## Higher 'n' works much better in this case
#' smoother(sine_noisy.rd, n = 0.2, method = "loess")
#' ## But too high and range is lost
#' smoother(sine_noisy.rd, n = 0.4, method = "loess")
#'
#' ## Spline smoothing
#' smoother(swim_y.rd, n = 0.4, method = "spline")
#'
#' ## Supersmooth
#' ## Same 'n' as above is much too high
#' smoother(swim_y.rd, n = 0.4, method = "supersmooth")
#' ## Much smaller values work better with this method
#' smoother(swim_y.rd, n = 0.01, method = "supersmooth")
#'
#' ## Change plot output
#' ## Plot all panels
#' smoother(swim_y.rd, n = 0.01, method = "supersmooth",
#'        plot = TRUE, panel = "all")
#' ## Only plot third panel
#' smoother(swim_y.rd, n = 0.01, method = "supersmooth",
#'        plot = TRUE, panel = 3)
#'
#'@author Nicholas Carey - \email{nicholascarey@gmail.com}
#'@md
#'@import stats graphics
#'@importFrom dplyr between
#'@importFrom zoo rollapply
#'@export

smoother <- function(x, n = NULL, method = "movav", plot = TRUE, panel = "all") {

  if(!is.numeric(x) || length(x) == 1) stop("smoother: the 'x' input should be a numeric vector.")
  if(is.null(n) && method %in% c("movav", "loess"))
    stop("smoother: for 'movav' and 'loess' methods the 'n' input should be a single numeric value.")
  if(length(n) > 1)
    stop("smoother: the 'n' input should be NULL or a single numeric value.")
  if(!(method %in% c("movav", "loess", "spline", "supersmooth")))
    stop("smoother: 'method' not recognised.")

  if (method == "movav") {
    if(between(n, 0, 1) ) width <- length(x) * n else
      width <- n
    smoothered <- zoo::rollapply(x, width, FUN = mean, align = "center", partial = TRUE)
  }

  else if (method == "loess") {
    if(between(n, 0, 1) ) span <- n else
      span <- n / length(x)

    y.loess <- loess(x ~ seq(1, length(x), 1), span = span)
    ## predict new y
    smoothered <- predict(y.loess, seq(1, length(x), 1))
  }

  else if (method == "spline") {
    if(is.null(n)) spar <- NULL else
      if(between(n, 0, 1) ) spar <- n else
        spar <- n / length(x)

      y.spline <- smooth.spline(x ~ 1:length(x), spar = spar, cv = TRUE)
      ## predict new y
      smoothered <- predict(y.spline, 1:length(x))$y
  }

  else if (method == "supersmooth") {
    if(is.null(n)) span <- "cv" else
      if(between(n, 0, 1) ) span <- n else
        span <- n / length(x)

      y.supsmu <- supsmu(x = 1:length(x), y = x, span = span)
      ## predict new y
      smoothered <- y.supsmu$y
  }

  ## Plot data
  if (plot == TRUE) {

    parorig <- par(no.readonly = TRUE) # save original par settings

    ylim = grDevices::extendrange(r = range(x), f = 0.05)
    xlim = c(1, length(x))

    if(panel == "all") {
      par(mfrow = c(3, 1), mar = c(0,0,0,0),
          oma = c(2.2, 2.2, 0.2, 0.2))
    } else {
      par(mfrow = c(1, 1), mar = c(1, 1, 1, 0.2))
    }


    if(panel == 1) {
      plot(x, ylim = ylim, xlim = xlim, axes = FALSE,
           col = "green")
      Axis(side = 2, cex.axis = 1)
      Axis(side = 1, cex.axis = 1)
      title("Original values", outer = FALSE)}

    if(panel == 2) {
      plot(smoothered, ylim = ylim, xlim = xlim, axes = FALSE)
      Axis(side = 2, cex.axis = 1)
      Axis(side = 1, cex.axis = 1)
      title("Smoothed values")}

    if(panel == 3) {
      plot(x, ylim = ylim, xlim = xlim, axes = FALSE,
           col = "green")
      Axis(side = 2, cex.axis = 1)
      Axis(side = 1, cex.axis = 1)
      lines(smoothered, ylim = ylim, xlim = xlim)
      title("Combined")}

    if(panel == "all"){
      plot(x, ylim = ylim, xlim = xlim, axes = FALSE,
           col = "green")
      Axis(side = 2, cex.axis = 1)
      plot(smoothered, ylim = ylim, xlim = xlim, axes = FALSE)
      Axis(side = 2, cex.axis = 1)
      plot(x, ylim = ylim, xlim = xlim, axes = FALSE,
           col = "green")
      Axis(side = 2, cex.axis = 1)
      Axis(side = 1, cex.axis = 1)
      lines(smoothered, ylim = ylim, xlim = xlim)
    }
    on.exit(par(parorig)) # revert par settings to original
  }

  return(smoothered)
}
