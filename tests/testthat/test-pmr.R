test_that("PMR requires zoo inputs", {
  obs <- matrix(1:20, ncol = 2)
  sim <- obs

  expect_error(PMR(sim, obs, k = 3), "must be 'zoo' objects")
  expect_error(PMR(as.data.frame(sim), as.data.frame(obs), k = 3),
               "must be 'zoo' objects")
})

test_that("PMR handles multicolumn zoo objects column-wise", {
  dates <- seq.Date(as.Date("2000-01-01"), by = "day", length.out = 10)
  obs <- zoo::zoo(matrix(1:20, ncol = 2), dates)
  sim <- obs
  sim[1:5, 1] <- sim[1:5, 1] + 2
  sim[6:10, 2] <- sim[6:10, 2] - 2

  out <- PMR(sim, obs, k = 3)
  expected <- c(PMR(sim[, 1], obs[, 1], k = 3),
                PMR(sim[, 2], obs[, 2], k = 3))

  expect_equal(out, expected)
})

test_that("PMR selects fixed windows before removing invalid pairs", {
  dates <- seq.Date(as.Date("2000-01-01"), by = "day", length.out = 6)
  obs <- zoo::zoo(c(1, 2, NA, 4, 5, 6), dates)
  sim <- zoo::zoo(c(1, 4, 10, 8, NA, 6), dates)
  k <- 3

  valid <- !is.na(sim) & !is.na(obs)
  mean_bias <- mean(sim[valid]) - mean(obs[valid])
  qobs_mean <- mean(obs[valid])
  moving_bias <- sapply(seq_len(length(obs) - k + 1), function(i) {
    idx <- i:(i + k - 1)
    valid_window <- !is.na(sim[idx]) & !is.na(obs[idx])
    mean(sim[idx][valid_window]) - mean(obs[idx][valid_window])
  })
  expected <- 2 * mean(abs(moving_bias - mean_bias)) / qobs_mean

  expect_equal(PMR(sim, obs, k = k), expected)
})
