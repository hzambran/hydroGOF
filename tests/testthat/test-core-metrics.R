test_that("NSE returns one for perfect agreement and matches a hand-computed case", {
  obs <- c(1, 2, 3, 4)
  sim <- c(1, 2, 2, 5)

  expect_equal(NSE(obs, obs), 1)
  expect_equal(NSE(sim, obs), 0.6)
})

test_that("error and agreement metrics match hand-computed values", {
  obs <- c(1, 2, 3, 4)
  sim <- c(1, 2, 2, 5)

  expect_equal(me(obs, obs), 0)
  expect_equal(me(sim, obs), 0)

  expect_equal(mae(obs, obs), 0)
  expect_equal(mae(sim, obs), 0.5)

  expect_equal(mse(obs, obs), 0)
  expect_equal(mse(sim, obs), 0.5)

  expect_equal(ubRMSE(obs, obs), 0)
  expect_equal(ubRMSE(sim, obs), sqrt(0.5), tolerance = 1e-7)

  expect_equal(nrmse(sim, obs), 54.8)
  expect_equal(rsr(sim, obs), 0.5477226, tolerance = 1e-7)
  expect_equal(rSD(sim, obs), 1.3416408, tolerance = 1e-7)

  expect_equal(mNSE(obs, obs), 1)
  expect_equal(mNSE(sim, obs), 0.5)

  expect_equal(rNSE(obs, obs), 1)
  expect_equal(rNSE(sim, obs), 0.7829861, tolerance = 1e-7)

  expect_equal(wNSE(obs, obs), 1)
  expect_equal(wNSE(sim, obs), 0.44, tolerance = 1e-12)

  expect_equal(md(obs, obs), 1)
  expect_equal(md(sim, obs), 0.7777778, tolerance = 1e-7)

  expect_equal(rd(obs, obs), 1)
  expect_equal(rd(sim, obs), 0.9598122, tolerance = 1e-7)

  expect_equal(cp(obs, obs), 1)
  expect_equal(cp(sim, obs), 0.3333333, tolerance = 1e-7)
})

test_that("correlation and efficiency decomposition metrics behave as expected", {
  obs <- c(1, 2, 3, 4)
  sim <- c(1, 2, 2, 5)

  expect_equal(rPearson(obs, obs), 1)
  expect_equal(rPearson(sim, obs), 0.8944272, tolerance = 1e-7)

  expect_equal(R2(obs, obs), 1)
  expect_equal(R2(sim, obs), 0.6)

  expect_equal(br2(obs, obs), 1)
  expect_equal(br2(sim, obs), 0.5806452, tolerance = 1e-7)

  expect_equal(VE(obs, obs), 1)
  expect_equal(VE(sim, obs), 0.8, tolerance = 1e-12)

  expect_true(is.finite(KGElf(obs, obs)))
  expect_true(is.finite(KGElf(sim, obs)))

  expect_equal(KGEnp(obs, obs), 1)
  expect_equal(KGEnp(sim, obs), 0.8876016, tolerance = 1e-7)
})

test_that("KGE returns the expected 2012 components", {
  obs <- c(1, 2, 3, 4)
  sim <- c(1, 2, 2, 5)

  out <- KGE(sim, obs, method = "2012", out.type = "full")

  expect_true(is.finite(out$KGE.value))
  expect_equal(unname(out$KGE.elements["r"]), 0.8944272, tolerance = 1e-7)
  expect_equal(unname(out$KGE.elements["Beta"]), 1, tolerance = 1e-12)
  expect_equal(unname(out$KGE.elements["Gamma"]), 1.3416408, tolerance = 1e-7)
  expect_true(is.finite(KGE(obs, obs, method = "2012")))
})

test_that("KGE returns the optimum value for perfect agreement, including constant series", {
  x <- c(1, 1, 1, 1)
  z <- c(0, 0, 0, 0)

  out_2012 <- KGE(x, x, method = "2012", out.type = "full")
  out_2009 <- KGE(x, x, method = "2009", out.type = "full")
  out_2021 <- KGE(x, x, method = "2021", out.type = "full")
  out_zero <- KGE(z, z, method = "2012", out.type = "full")

  expect_equal(out_2012$KGE.value, 1)
  expect_equal(unname(out_2012$KGE.elements), c(1, 1, 1))

  expect_equal(out_2009$KGE.value, 1)
  expect_equal(unname(out_2009$KGE.elements), c(1, 1, 1))

  expect_equal(out_2021$KGE.value, 1)
  expect_equal(unname(out_2021$KGE.elements), c(1, 0, 1))

  expect_equal(out_zero$KGE.value, 1)
  expect_equal(unname(out_zero$KGE.elements), c(1, 1, 1))
})

test_that("KGE returns NA instead of NaN when 2009/2012 bias terms are undefined", {
  obs <- c(-1, 1)
  sim <- c(-2, 2)

  expect_warning(out_2012 <- KGE(sim, obs, method = "2012", out.type = "full"))
  expect_warning(out_2009 <- KGE(sim, obs, method = "2009", out.type = "full"))

  expect_true(is.na(out_2012$KGE.value))
  expect_true(is.na(out_2009$KGE.value))
  expect_equal(unname(out_2012$KGE.elements), c(1, NA, NA))
  expect_equal(unname(out_2009$KGE.elements), c(1, NA, 2))
})

test_that("PBIAS matches a simple bias calculation", {
  obs <- c(1, 2, 3)
  sim <- c(2, 3, 4)

  expect_equal(pbias(obs, obs), 0)
  expect_equal(pbias(sim, obs), 50)
})

test_that("KGEkm matches the expected 2012 components and score", {
  obs <- c(1, 2, 3, 4)
  sim <- c(1, 2, 2, 5)

  out <- KGEkm(sim, obs, method = "2012", out.type = "full")

  expect_equal(out$KGEkm.value, 0.8915774, tolerance = 1e-7)
  expect_equal(unname(out$KGEkm.elements["r"]), 0.8944272, tolerance = 1e-7)
  expect_equal(unname(out$KGEkm.elements["Beta"]), 1, tolerance = 1e-12)
  expect_equal(unname(out$KGEkm.elements["Gamma"]), 1.0246951, tolerance = 1e-7)
})

test_that("zoo-only flow diagnostics return ideal values for perfect agreement", {
  skip_if_not_installed("hydroTSM")
  suppressPackageStartupMessages(library(hydroTSM))

  dates <- seq.Date(as.Date("2000-01-01"), by = "day", length.out = 365 * 3)
  base <- 10 + sin(seq_along(dates) / 20) + (seq_along(dates) %% 30) / 30
  obs <- zoo::zoo(base, dates)
  sim <- obs

  expect_equal(wsNSE(sim, obs), 1)
  expect_true(is.finite(sKGE(sim, obs)))
  expect_equal(APFB(sim, obs), 0)
  expect_equal(HFB(sim, obs), 0)
  expect_equal(pbiasfdc(sim, obs, plot = FALSE), 0)
})

test_that("d and dr match hand-computed values", {
  obs <- c(1, 2, 3, 4)
  sim <- c(1, 2, 2, 5)

  expect_equal(d(obs, obs), 1)
  expect_equal(d(sim, obs), 0.9259259, tolerance = 1e-7)

  expect_equal(dr(obs, obs), 1)
  expect_equal(dr(sim, obs), 0.75, tolerance = 1e-12)
})

test_that("RMSE matches the root mean squared error formula", {
  obs <- c(1, 2, 3, 4)
  sim <- c(1, 2, 2, 5)

  expect_equal(rmse(obs, obs), 0)
  expect_equal(rmse(sim, obs), sqrt(0.5), tolerance = 1e-12)
})

test_that("sKGE returns internally consistent yearly output", {
  dates <- seq.Date(as.Date("2001-01-01"), by = "day", length.out = 365 * 3)
  pattern <- seq_len(365)
  obs <- zoo::zoo(rep(pattern, 3), dates)
  sim <- obs

  out <- sKGE(sim, obs, out.PerYear = TRUE)

  expect_equal(names(out), c("sKGE.value", "KGE.PerYear"))
  expect_length(out$KGE.PerYear, 3)
  expect_true(all(is.finite(out$KGE.PerYear)))
  expect_equal(unname(out$sKGE.value), mean(unname(out$KGE.PerYear)), tolerance = 1e-12)
})

test_that("pfactor matches scalar and matrix expectations", {
  x <- c(1, 2, 3, 4)
  lband <- c(0, 1, 2, 3)
  uband <- c(2, 3, 4, 5)

  expect_equal(pfactor(x, lband, uband), 1)

  x_mat <- matrix(c(5, 1, 5, 1), 2, 2)
  l_mat <- matrix(0, 2, 2)
  u_mat <- matrix(c(10, 0, 10, 0), 2, 2)

  expect_equal(pfactor(x_mat, l_mat, u_mat), c(0.5, 0.5))
})

test_that("rfactor matches scalar and matrix expectations", {
  x <- c(1, 2, 3, 4)
  lband <- c(0, 1, 2, 3)
  uband <- c(2, 3, 4, 5)

  expect_equal(rfactor(x, lband, uband), 1.549193, tolerance = 1e-6)

  x_mat <- matrix(c(5, 1, 5, 1), 2, 2)
  l_mat <- matrix(0, 2, 2)
  u_mat <- matrix(c(10, 0, 10, 0), 2, 2)

  expect_equal(rfactor(x_mat, l_mat, u_mat), c(1.767767, 1.767767), tolerance = 1e-6)
})
