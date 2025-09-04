#' Plot a Spectrogram Using av::read_audio_fft
#'
#' This function generates a spectrogram from an audio file
#' using the \pkg{av} package’s FFT functionality. It is a
#' custom replacement for \code{seewave::spectro()} designed
#' to integrate into Shiny workflows and R packages.
#'
#' @param path_to_file Character string with the path to the audio file
#'   (e.g., \code{.wav} or \code{.mp3}).
#' @param dark Logical, whether to use a dark mode color scheme (default \code{FALSE}).
#' @param legend Logical, whether to display a legend with channel and sample rate (default \code{TRUE}).
#' @param keep.par Logical, whether to preserve the current graphical parameters (default \code{FALSE}).
#' @param useRaster Logical, passed to \code{\link[graphics]{image}} (default \code{TRUE}).
#' @param vline Optional numeric vector of time points to draw vertical lines at (default \code{NULL}).
#' @param max_freq Numeric, maximum frequency to display (in kHz if \code{kHz = TRUE}, else Hz). Default = 16.
#' @param tstart Numeric, start time in seconds (default = 0).
#' @param tende Numeric, end time in seconds. If \code{NULL}, uses the duration of the audio.
#' @param kHz Logical, whether to display frequency in kHz (default \code{TRUE}).
#' @param delineate_center Numeric, optional width in seconds to mark the central region of the spectrogram.
#' @param fft_window_length Integer, FFT window length (required).
#' @param overlap Numeric between 0 and 1, fraction of window overlap (required).
#' @param ... Additional arguments passed to \code{\link[graphics]{image}}.
#'
#' @details
#' The function uses \code{av::read_audio_fft()} with a Hann window to compute
#' the short-time Fourier transform, and then visualizes the result with
#' \code{\link[graphics]{image}}. Users can optionally display in dark mode,
#' add vertical guide lines, or highlight the central portion of the spectrogram.
#'
#' @return
#' A base R graphics spectrogram is plotted. The function is called for its side effects
#' (plotting) and does not return a value.
#'
#' @examples
#' \dontrun{
#' # Example with a WAV file
#' audio_path <- system.file("extdata", "sample.wav", package = "yourPackageName")
#' plot_av_fft(
#'   path_to_file = audio_path,
#'   fft_window_length = 1024,
#'   overlap = 0.75,
#'   max_freq = 20
#' )
#' }
#'
#'
#' @export
plot_av_fft <-
  function(path_to_file,
           dark = FALSE,
           legend = TRUE,
           keep.par = FALSE,
           useRaster = TRUE,
           vline = NULL,
           max_freq = 16,
           tstart = 0,
           tende = NULL,
           kHz = TRUE,
           delineate_center = NULL,
           fft_window_length,
           overlap,
           ...) {

    audio_fft <-
      av::read_audio_fft(
        path_to_file,
        window = av::hanning(fft_window_length),
        overlap = overlap
      )

    # Check attributes of x
    necessary_attributes <- c("time", "frequency", "duration", "sample_rate", "input")
    missing_attributes <- necessary_attributes[!(necessary_attributes %in% names(attributes(audio_fft)))]

    if (length(missing_attributes) > 0) {
      stop(paste("Input is missing the following necessary attributes:", paste(missing_attributes, collapse = ", "), "."))
    }

    # If 'keep.par' is FALSE, save the current graphical parameters and ensure they will be reset when the function exits
    if (!isTRUE(keep.par)) {
      oldpar <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(oldpar))
    }

    # Set color scheme for the plot, depending on the 'dark' option
    col <- if (isTRUE(dark)) {
      graphics::par(
        bg = "black",
        col.axis = "white",
        fg = "white",
        family = "mono",
        font = 2,
        col.lab = "white",
        col.main = "white"
      )
      viridisLite::inferno(24, direction = -1)
    } else {
      gray.colors(
        24,
        start = 0,
        end = 1,
        gamma = 2.2,
        1,
        rev = FALSE
      )
    }

    # Check if 'tende' (the end time) is specified, if not, set it to the 'duration' attribute of 'x'
    if (is.null(tende)) {
      # if no end is given, print everything
      tende <- attr(audio_fft, "duration")
    }

    # Set unit and divider based on the 'kHz' option
    if (kHz) {
      unit <- "kHz"
      divider <- 1000
    } else {
      unit <- "Hz"
      divider <- 1
    }

    # Check if 'max_freq' is specified, if not, set it to half of the 'sample_rate' attribute of 'x'
    if (is.null(max_freq)) {
      # if no maximum frequency is given, print everything
      max_freq <- (attr(audio_fft, "sample_rate") / 2) / divider
    }

    # Set some graphical parameters and create an image plot using these parameters and the attributes of 'x'
    graphics::par(mar = c(6, 5, 3, 3), mex = 0.6)
    graphics::image(
      attr(audio_fft, "time"),
      attr(audio_fft, "frequency") / divider,
      t(audio_fft),
      ylim = c(0, max_freq),
      xlim = c(tstart, tende),
      xlab = "TIME",
      ylab = sprintf("FREQUENCY (%s)", unit),
      col = col,
      useRaster = useRaster,
      ...
    )

    # If 'delineate_center' is not NULL, draw two vertical dashed lines to delineate the central region of the plot
    if (!is.null(delineate_center)) {
      # calculate the center
      buffer <- (tende - tstart - delineate_center) / 2

      graphics::abline(
        v = tstart + buffer,
        lwd = 1.1,
        lty = 2,
        col = "blue"
      )
      graphics::abline(
        v = tende - buffer,
        lwd = 1.1,
        lty = 2,
        col = "blue"
      )
    }

    # If 'vline' has some value(s), draw vertical line(s) at these time points
    if (length(vline)) {
      graphics::abline(v = vline, lwd = 2)
    }

    # If 'legend' is TRUE, add a legend to the plot showing the number of channels and sample rate
    if (isTRUE(legend)) {
      input <- attr(audio_fft, "input")
      label <- sprintf(
        "%d channel, %d%s",
        input$channels,
        attr(audio_fft, "sample_rate") / divider,
        unit
      )
      graphics::legend(
        "topright",
        legend = label,
        pch = "",
        xjust = 1,
        yjust = 1,
        bty = "o",
        cex = 0.7
      )
    }
  }
