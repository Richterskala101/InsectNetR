test_that("create_validation_segments runs on small mock data", {
  skip_on_cran()
  skip_if_not_installed("tuneR")
  skip_if_not_installed("fs")

  # Make temporary dirs
  tmpdir <- tempdir()
  audio_dir <- file.path(tmpdir, "audio")
  fs::dir_create(audio_dir)
  out_dir <- file.path(tmpdir, "segments")

  # Create a dummy wav file
  wavfile <- file.path(audio_dir, "dummy.wav")
  wav <- tuneR::Wave(left = rep(0L, 8000), samp.rate = 8000, bit = 16)
  tuneR::writeWave(wav, wavfile)

  # Fake CSV
  csv_path <- file.path(tmpdir, "preds.csv")
  df <- data.frame(
    offset = "0-1",
    filename = "dummy.wav",
    speciesA = 0.9,
    speciesB = 0.0,
    stringsAsFactors = FALSE
  )
  utils::write.csv(df, csv_path, row.names = FALSE)

  # Run function
  result <- create_validation_segments(
    input_csv = csv_path,
    audio_folder = audio_dir,
    output_folder = out_dir,
    segment_length = 1,
    n_per_class = 1
  )

  expect_true(dir.exists(file.path(out_dir, "speciesA")))
  expect_s3_class(result, "data.frame")
  expect_true(any(grepl("speciesA", result$species)))
})
