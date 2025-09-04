test_that("plot_av_fft runs without error on a synthetic sine wave", {
  skip_on_cran()
  skip_if_not_installed("av")
  skip_if_not_installed("tuneR")

  # Create a temporary sine wave audio file
  wav_file <- tempfile(fileext = ".wav")
  sine_wave <- tuneR::sine(440, duration = 1, samp.rate = 8000, bit = 32, pcm = TRUE) # 440Hz, 1 second
  tuneR::writeWave(sine_wave, wav_file)

  # Should run without error and produce a plot
  expect_silent(
    plot_av_fft(
      path_to_file = wav_file,
      fft_window_length = 512,
      overlap = 0.5,
      max_freq = 4  # in kHz
    )
  )
})

test_that("plot_av_fft errors with missing fft_window_length", {
  skip_if_not_installed("tuneR")

  # Generate a short noise file
  wav_file <- tempfile(fileext = ".wav")
  noise <- tuneR::noise(duration = 1, samp.rate = 8000, bit = 32)
  tuneR::writeWave(noise, wav_file)

  # Missing fft_window_length should throw an error
  expect_error(
    plot_av_fft(
      path_to_file = wav_file,
      overlap = 0.5
    )
  )
})
