test_that("JDKGE perfect agreement returns one across divergence backends", {
  obs <- c(0, 1, 2, 4, 8, 16)
  sim <- obs

  out_hist <- JDKGE(sim, obs, density.method = "hist", out.type = "full")
  out_kde <- JDKGE(sim, obs, density.method = "kde", out.type = "full")
  out_w1 <- JDKGE(sim, obs, density.method = "wasserstein", out.type = "full")

  expect_equal(out_hist$JDKGE.value, 1)
  expect_equal(out_kde$JDKGE.value, 1)
  expect_equal(out_w1$JDKGE.value, 1)
  expect_equal(out_kde$JDKGE.elements[["Delta"]], 1)
  expect_equal(out_w1$JDKGE.elements[["Delta"]], 1)
})

test_that("JDKGE supports backend tuning arguments", {
  obs <- c(0, 0.2, 0.5, 1, 2, 4, 8, 16)
  sim <- c(0, 0.1, 0.4, 1.2, 1.7, 3.8, 7, 20)

  out_kde_1 <- JDKGE(sim, obs, density.method = "kde", kde.n.grid = 128, out.type = "full")
  out_kde_2 <- JDKGE(sim, obs, density.method = "kde", kde.n.grid = 1024, out.type = "full")
  out_w1_1 <- JDKGE(sim, obs, density.method = "wasserstein", wasserstein.n.quantiles = 64, out.type = "full")
  out_w1_2 <- JDKGE(sim, obs, density.method = "wasserstein", wasserstein.n.quantiles = 2048, out.type = "full")

  expect_true(is.finite(out_kde_1$JDKGE.value))
  expect_true(is.finite(out_kde_2$JDKGE.value))
  expect_true(is.finite(out_w1_1$JDKGE.value))
  expect_true(is.finite(out_w1_2$JDKGE.value))
  expect_false(isTRUE(all.equal(out_kde_1$JDKGE.elements[["Delta"]], out_kde_2$JDKGE.elements[["Delta"]])))
  expect_false(isTRUE(all.equal(out_w1_1$JDKGE.elements[["Delta"]], out_w1_2$JDKGE.elements[["Delta"]])))
})

test_that("JDKGE default epsilon is the article epsilon through otherValue", {
  obs <- c(0, 0, 1, 2, 3, 5)
  sim <- c(0, 1, 1, 2, 4, 5)
  eps <- min(1e-6, 0.1 * min(c(sim[sim > 0], obs[obs > 0])))

  out_default <- JDKGE(sim, obs, out.type = "full")
  out_explicit <- JDKGE(sim, obs, epsilon.type = "otherValue", epsilon.value = eps, out.type = "full")

  expect_equal(out_default$JDKGE.value, out_explicit$JDKGE.value)
  expect_error(JDKGE(sim, obs, epsilon.type = "paper"), "should be one of")
})

test_that("JDKGE method variants expose the expected component names", {
  obs <- c(0, 0.2, 0.5, 1, 2, 4, 8, 16)
  sim <- c(0, 0.1, 0.4, 1.2, 1.7, 3.8, 7, 20)

  out_2012 <- JDKGE(sim, obs, method = "2012", out.type = "full")
  out_2009 <- JDKGE(sim, obs, method = "2009", out.type = "full")
  out_2021 <- JDKGE(sim, obs, method = "2021", out.type = "full")

  expect_identical(names(out_2012$JDKGE.elements), c("r", "Beta", "Gamma", "Delta"))
  expect_identical(names(out_2009$JDKGE.elements), c("r", "Beta", "Alpha", "Delta"))
  expect_identical(names(out_2021$JDKGE.elements), c("r", "Beta.2021", "Alpha", "Delta"))
})
