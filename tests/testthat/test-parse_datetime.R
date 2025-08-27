test_that("parse_datetime() created a posixct object", {
  expect_equal(parse_datetime(filename = c("path/to/20240719_112000.WAV", "another/path/20240722_124000.WAV")),
               lubridate::ymd_hms(c("2024-07-19 11:20:00", "2024-07-22 12:40:00"), tz = "UTC"))
})
