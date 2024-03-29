#' @title peaks
#'
#' @description Identify peaks and troughs in oscillating data.
#'
#' @details `peaks` scans a vector of numeric values and identifies peaks and
#'   troughs.
#'
#'   The required input (`span`) and two optional inputs (smoothing and
#'   `height`), need to be balanced to successfully identify peaks and troughs.
#'   Characteristics such as data noisiness, amplitude, wavelength etc will
#'   affect how important each of these are and how successful the
#'   identification process is.
#'
#'   **span**
#'
#'   The most important parameter in determining peaks is the `span`, which sets
#'   the threshold for identification. A rolling window of width `span` moves
#'   across the data, and to be designated a peak a value (after any smoothing)
#'   must be the highest value within that window (or lowest value for troughs).
#'   The `span` window can be entered as an integer number of values (e.g. `span
#'   = 11`), or if between 0 and 1 is a proportion of the total data length
#'   (e.g. `span = 0.1`). Note: strictly speaking, the function tests
#'   `floor(span/2)` values before and after each central value, therefore any
#'   even `span` inputs are rounded up. That is `span = 10` and `span = 11` will
#'   both result in an effective moving window of 11 values, with the central
#'   value tested against the 5 values before and after it.
#'
#'   **Smoothing**
#'
#'   For noisy data there is optional smoothing functionality via the
#'   `smooth_method` input. See [smoother()] for the methods available and
#'   appropriate `smooth.n` values. `smooth.method = "spline"` works
#'   particularly well for oscillating data.
#'
#'   **Prominence**
#'
#'   Peak identification is also affected by the optional `prominence` argument.
#'   This sets a threshold of 'prominence' for peak identification (equivalent
#'   to 'depth' for a trough). This should be a value between 0 and 1 describing
#'   a proportion of the total range of the data. Only peaks or troughs
#'   containing a range equal to or above this proportion within their `span`
#'   window will be retained. Essentially the higher this is set, the more
#'   prominent the peaks must be to be identified (or deeper the troughs). To
#'   help with choosing appropriate values, ranges for peaks and troughs are
#'   included in the output. Peaks and troughs can have different `prominence`
#'   thresholds: simply enter them as a vector of two values in peak-trough
#'   order. E.g. `prominence = c(0.2, 0.1)`.
#'
#'   **Equal values within a `span` window**
#'
#'   If there happens to be equal values within a `span` window, the first
#'   occurrence is designated as the peak or trough. If peaks or troughs are
#'   being missed because of this, use a lower `span`. If there are many
#'   instances of equal values, try smoothing the data.
#'
#'   **Partial windows**
#'
#'   A rolling window of width `span` is used across the data to identify peaks.
#'   At the start and end of the data vector this window will overlap the start
#'   and end. The default `partial = TRUE` input tells the function to attempt
#'   to identify peaks near the start/end of the data where the `span` width
#'   window is not complete. Change this to `FALSE` if you see odd matching
#'   behaviour at the start or end of the data.
#'
#'   **Plot**
#'
#'   If `plot = TRUE` a plot is produced of the data with peaks and/or troughs
#'   highlighted. If the data has been smoothed this will be a 2-panel plot of
#'   both data sets, otherwise it is the raw data. `plot.which` controls what
#'   results are plotted. The default `plot.which = "b"` means *both* peaks
#'   (green points and lines) and troughs (blue points and lines) are
#'   highlighted within the data. Change to `plot.which = "p"` to plot only
#'   peaks, or `plot.which = "t"` for only troughs. This does not affect the
#'   output object.
#'
#'   **Output**
#'
#'   The output of the function is a `list` object of class `peaks` containing
#'   the following elements which can be extracted via the `$` operator for
#'   further use:
#'
#'   - `data` - original data input
#'
#'   - `data_smooth` - smoothed data. Identical to `data` if no smoothing
#'   applied
#'
#'   - `peaks` - index of locations of identified peaks
#'
#'   - `troughs` - index of locations of identified troughs
#'
#'   - `both` - index of locations of *both* peaks and troughs
#'
#'   - `peaks_range` - proportional data range within the `span` window of each
#'   peak
#'
#'   - `troughs_range` - proportional data range within the `span` window of
#'   each trough
#'
#'   - `both_range` - proportional data range within the `span` window of each
#'   peak and trough
#'
#'   - `call` - the function call
#'
#' @usage peaks(x, span = NULL, prominence = NULL, partial = TRUE, smooth.method
#'   = NULL, smooth.n = NULL, plot = TRUE, plot.which = "b")
#'
#' @param x numeric. A numeric vector.
#' @param span numeric. Sets window size for peak (or trough) identification; to
#'   be designated a peak, a value must be the highest value (lowest for
#'   troughs) within a rolling window of width `span`. Can be entered as either
#'   a window of number of values, or value between 0 and 1 of proportion of
#'   total data length. See Details.
#' @param prominence numeric. Value between 0 and 1. Sets threshold for peak or
#'   trough 'prominence'. See Details.
#' @param partial logical. Default TRUE. Should the function attempt to identify
#'   peaks or troughs at the start or end of the vector where the `span` window
#'   is truncated? See Details.
#' @param smooth.method string. Method by which to smooth data before peak
#'   identification. Optional. Default is `NULL`. See [smoother()].
#' @param smooth.n numeric. Smoothing factor. See [smoother()].
#' @param plot logical. Plots the result.
#' @param plot.which string. What to plot: "p" for peaks, "t" for troughs, or
#'   the default "b" for both.
#'
#' @examples
#' peaks(swim_y.rd, span = 5, smooth.method = "spline", smooth.n = 0.4, plot.which = "p")
#'
#' @author Nicholas Carey - \email{nicholascarey@gmail.com}
#' @importFrom zoo rollapply
#' @importFrom dplyr between
#' @md
#' @export

peaks <- function(x,
                  span = NULL,
                  prominence = NULL,
                  partial = TRUE,
                  smooth.method = NULL,
                  smooth.n = NULL,
                  plot = TRUE,
                  plot.which = "b"){

  ## Save call for output
  call <- match.call()

  ## Stop if no span
  if(is.null(span)) stop("peaks: please enter a 'span' value.")
  if(!(dplyr::between(span, 0, 1)) && span %% 1 != 0)
    stop("peaks: 'span' should be a value between 0 and 1, or an integer greater than 1.")

  ## make span
  if(dplyr::between(span, 0, 1))
    span <- round(length(x) * span)
  span <- floor(span/2)

  ## make prominence zero if null
  if(is.null(prominence)) {
    prominence_p <- 0
    prominence_t <- 0
    ## separate to peak and trough prominence
  } else if(length(prominence) == 1) {
    prominence_p <- prominence
    prominence_t <- prominence
  } else if(length(prominence) == 2) {
    prominence_p <- prominence[1]
    prominence_t <- prominence[2]
  } else {
    stop("peaks: 'prominence' input should be NULL, a single numeric value, or vector of two values.")
  }

  ## set logicals
  if(!is.null(smooth.method)) smooth <- TRUE else
    smooth = FALSE

  ## smooth x
  if(smooth) z <- smoother(x, n = smooth.n, method = smooth.method, plot = FALSE) else
    z <- x

  ## For first and last 'span' values, the central value is not the one of
  ## interest. This is a vector of which is the one to be tested.
  test_val <- c(1:span, rep(span+1, length(z)-(span*2)), span:1)

  ## df of value of interest in each window and all data
  df <- data.frame(test_val, z)


  # prominence range ------------------------------------------------------------

  ## determine y range within each span
  prominencerange <- zoo::rollapply(z, width = span * 2 + 1, FUN = range, align = "center", partial = TRUE)
  prominencerange <- abs(prominencerange[,1] - prominencerange[,2])
  ## convert to proportion of total
  prominencerange <- prominencerange / abs(diff(range(z)))


  # Detect peaks ------------------------------------------------------------

  ## apply across rolling window of appropriate width
  peaks <- zoo::rollapply(df, width = span * 2 + 1, function(q) {
    cent <- q[length(q[,1]) - span,1] # value to test
    x <- q[,2] ## all values
    # is it the highest value?
    # This also shoudl catch identical values, but only return first one as the peak
    logi <- all(x[cent] > x[1:(cent-1)]) && all(x[cent] >= x[(cent+1):length(x)])
    return(logi)
  },
  align = "center", ## so window is aligned on central value
  partial = TRUE, ## so it includes start/end where window is truncated
  by.column = FALSE) ## to apply across the df by row not column

  ## if 'partial' should not return peaks in first or last 'span' values
  if(!partial) {
    peaks <- replace_head(peaks, span, FALSE)
    peaks <- replace_tail(peaks, span, FALSE)
  }

  ## index of peaks
  peaks <- which(peaks)
  ## subset to peaks with range greater than 'prominence' input
  peaks <- peaks[prominencerange[peaks] >= prominence_p]
  peakrange <- prominencerange[peaks]


  # Detect troughs ----------------------------------------------------------

  troughs <- zoo::rollapply(df, width = span * 2 + 1, function(q) {
    cent <- q[length(q[,1]) - span,1]
    x <- q[,2]
    logi <- all(x[cent] < x[1:(cent-1)]) && all(x[cent] <= x[(cent+1):length(x)])
    return(logi)
  },
  align = "center",
  partial = TRUE,
  by.column = FALSE)

  if(!partial) {
    troughs <- replace_head(troughs, span, FALSE)
    troughs <- replace_tail(troughs, span, FALSE)
  }

  troughs <- which(troughs)
  troughs <- troughs[prominencerange[troughs] >= prominence_t]
  troughrange <- prominencerange[troughs]

  # Assemble both for output ------------------------------------------------

  both <- sort(c(peaks,troughs))
  bothrange <- prominencerange[both]

  # Assemble output ---------------------------------------------------------

  output <- list(data = x,
                 data_smooth = z,
                 peaks = peaks,
                 troughs = troughs,
                 both = both,
                 peaks_range = peakrange,
                 troughs_range = troughrange,
                 both_range = bothrange,
                 call = call)

  ## apply class
  class(output) <- "peaks"

  ## plot
  if(plot) plot(x = output, plot.which = plot.which)

  ## return
  return(output)
}


# S3 plotting function
#' @export
plot.peaks <- function(x, plot.which = "b", ...){

  parorig <- par(no.readonly = TRUE) # save original par settings

  dt <- x$data
  dtsm <- x$data_smooth
  peaks <- x$peaks
  troughs <- x$troughs
  ylim <- grDevices::extendrange(r = range(dt,dtsm), f = 0.05)

  if(plot.which == "p") {
    plot.peaks <- TRUE
    plot.troughs <- FALSE
  } else if(plot.which == "t") {
    plot.peaks <- FALSE
    plot.troughs <- TRUE
  } else if(plot.which == "b") {
    plot.peaks <- TRUE
    plot.troughs <- TRUE
  }

  if(!identical(dt, dtsm)) {
    par(mfrow=c(2,1), mar=c(2.2,2.2,0.5,0.5))
    plot(1:length(dt), dt, ylim = ylim)
    if(plot.peaks) {
      abline(v = peaks, col = "green", lty = 2)
    }
    if(plot.troughs) {
      abline(v = troughs, col = "slateblue", lty = 2)
    }
  }

  par(mar=c(2.2,2.2,0.5,0.5))
  plot(1:length(dtsm), dtsm, ylim = ylim)
  if(plot.peaks) {
    points(peaks, dtsm[peaks], col="black", pch = 25, bg = "green")
    abline(v = peaks, col = "green", lty = 2)
  }
  if(plot.troughs) {
    points(troughs, dtsm[troughs], col="black", pch = 24, bg = "slateblue")
    abline(v = troughs, col = "slateblue", lty = 2)
  }

  on.exit(par(parorig)) # revert par settings to original
}


