test_that("LCE returns one for perfect agreement", {
  obs <- 1:10
  sim <- obs

  expect_equal(LCE(sim, obs), 1)
})

test_that("LCE matches the published component formula", {
  obs <- c(1, 2, 4, 8, 16)
  sim <- c(1.1, 1.8, 5, 7, 18)

  r <- stats::cor(sim, obs)
  alpha <- stats::sd(sim) / stats::sd(obs)
  beta <- mean(sim) / mean(obs)
  expected <- 1 - sqrt((r * alpha - 1)^2 +
                       (r / alpha - 1)^2 +
                       (beta - 1)^2)

  expect_equal(LCE(sim, obs), expected)
})

test_that("LCE handles matrix inputs column-wise", {
  obs <- matrix(1:20, ncol = 2)
  sim <- obs
  sim[, 2] <- sim[, 2] * 1.1

  out <- LCE(sim, obs)
  expected <- c(LCE(sim[, 1], obs[, 1]),
                LCE(sim[, 2], obs[, 2]))

  expect_equal(out, expected)
})
