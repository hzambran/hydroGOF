test_that("ggof restores graphical parameters for all plot types", {
  skip_if_not_installed("zoo")
  skip_if_not_installed("hydroTSM")

  # Create daily inputs long enough for all temporal aggregations.
  dates <- seq.Date(as.Date("2000-01-01"), by = "day", length.out = 800)
  obs <- zoo::zoo(abs(sin(seq_along(dates) / 20)) + 1, dates)
  sim <- obs * 1.1

  # Use a file device so the test is independent of the interactive session.
  pngfile <- tempfile(fileext = ".png")
  grDevices::png(pngfile, width = 1000, height = 700)
  on.exit(grDevices::dev.off(), add = TRUE)

  # Exercise every plot type with and without the goodness-of-fit legend.
  plot.types <- c("o", "dm", "ma", "dma", "seasonal")
  for (ftype in plot.types) {
    for (gof.leg in c(FALSE, TRUE)) {
      # Set non-default values that expose order-dependent restoration bugs.
      graphics::par(mfrow = c(2, 2), mar = c(2, 3, 4, 1),
                    oma = c(1, 2, 3, 4), las = 2, xpd = NA,
                    bg = "grey95", fg = "navy", cex = 0.83, mex = 1.15)
      graphics::plot(1:5)
      before <- graphics::par(no.readonly = TRUE)

      # Forward additional graphical arguments through '...' as users may do.
      ggof(sim, obs, ftype = ftype, FUN = mean, gof.leg = gof.leg,
           pt.style = if (gof.leg) "bar" else "ts",
           col = c("green4", "orange3"), cex.axis = 0.7, cex.lab = 0.9,
           las = 1, xpd = FALSE)

      # Compare every writable graphical parameter, including derived geometry.
      after <- graphics::par(no.readonly = TRUE)
      expect_equal(after, before, tolerance = 0,
                   info = paste("ftype =", ftype, ", gof.leg =", gof.leg))
    } # FOR end
  } # FOR end
})

test_that("ggof restores graphical parameters after an error", {
  # Use a file device so the test is independent of the interactive session.
  pngfile <- tempfile(fileext = ".png")
  grDevices::png(pngfile, width = 1000, height = 700)
  on.exit(grDevices::dev.off(), add = TRUE)

  # Set non-default values before triggering an error inside the plotting code.
  graphics::par(mfrow = c(2, 2), mar = c(2, 3, 4, 1),
                oma = c(1, 2, 3, 4), las = 2, cex = 0.83, mex = 1.15)
  graphics::plot(1:5)
  before <- graphics::par(no.readonly = TRUE)

  # An invalid limit fails only after 'ggof' has changed graphical parameters.
  expect_error(ggof(1:20, 1:20, gof.leg = FALSE, ylim = "invalid"),
               "invalid 'ylim' value")

  # Ensure cleanup also covers unsuccessful calls.
  after <- graphics::par(no.readonly = TRUE)
  expect_equal(after, before, tolerance = 0)
})
