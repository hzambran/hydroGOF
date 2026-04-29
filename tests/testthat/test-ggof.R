test_that("ggof restores graphical parameters after a basic plot", {
  pngfile <- tempfile(fileext = ".png")
  grDevices::png(pngfile)
  on.exit(grDevices::dev.off(), add = TRUE)

  graphics::par(mfrow = c(2, 2), mar = c(2, 2, 2, 2), las = 2)
  before <- graphics::par(c("mfrow", "mar", "las"))

  ggof(1:20, 1:20, gof.leg = FALSE)

  after <- graphics::par(c("mfrow", "mar", "las"))
  expect_identical(after, before)
})

test_that("ggof restores graphical parameters after a multi-panel plot", {
  skip_if_not_installed("zoo")
  skip_if_not_installed("hydroTSM")
  suppressPackageStartupMessages(library(zoo))

  dates <- seq.Date(as.Date("2000-01-01"), by = "day", length.out = 800)
  obs <- zoo::zoo(abs(sin(seq_along(dates) / 20)) + 1, dates)
  sim <- obs * 1.1

  pngfile <- tempfile(fileext = ".png")
  grDevices::png(pngfile)
  on.exit(grDevices::dev.off(), add = TRUE)

  graphics::par(mfrow = c(2, 2), mar = c(2, 2, 2, 2), las = 2)
  before <- graphics::par(c("mfrow", "mar", "las"))

  ggof(sim, obs, ftype = "dm", FUN = mean, gof.leg = FALSE)

  after <- graphics::par(c("mfrow", "mar", "las"))
  expect_identical(after, before)
})
