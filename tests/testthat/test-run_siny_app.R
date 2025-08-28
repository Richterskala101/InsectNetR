test_that("run_shiny_app returns a shiny app object", {
  skip_on_cran()
  skip_if_not_installed("shiny")

  app <- run_shiny_app()

  # Only check the class
  expect_s3_class(app, "shiny.appobj")
})
