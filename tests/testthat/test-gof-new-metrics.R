test_that("gof includes JDKGE, LME, and LCE rows", {
  obs <- 1:10
  sim <- obs

  out <- gof(sim, obs)

  expect_true(all(c("JDKGE", "LME", "LCE") %in% rownames(out)))
  expect_false("PMR" %in% rownames(out))
  expect_equal(unname(out["JDKGE", 1]), 1)
  expect_equal(unname(out["LME", 1]), 1)
  expect_equal(unname(out["LCE", 1]), 1)
})

test_that("gof computes PMR for zoo inputs", {
  dates <- seq.Date(as.Date("2000-01-01"), by = "day", length.out = 20)
  obs <- zoo::zoo(seq_len(20), dates)
  sim <- obs
  sim[1:10] <- sim[1:10] + 2

  out <- gof(sim, obs, do.pmr = TRUE, k = 5)

  expect_equal(unname(out["PMR", 1]), unname(round(PMR(sim, obs, k = 5), 2)))
})

test_that("gof handles optional rows for matrix inputs", {
  obs <- matrix(1:40, ncol = 2)
  sim <- obs

  out <- gof(sim, obs, do.spearman = TRUE)

  expect_true("rSpearman" %in% rownames(out))
  expect_equal(ncol(out), 2)
  expect_equal(unname(out["rSpearman", ]), c(1, 1))
})

test_that("gof handles multicolumn zoo inputs column-wise", {
  dates <- seq.Date(as.Date("2000-01-01"), by = "day", length.out = 20)
  obs <- zoo::zoo(matrix(1:40, ncol = 2), dates)
  sim <- obs

  out <- gof(sim, obs, do.pmr = TRUE, k = 5)

  expect_equal(ncol(out), 2)
  expect_true("PMR" %in% rownames(out))
  expect_equal(unname(out["PMR", ]), c(0, 0))
})

test_that("gof handles zoo series with integer indexes", {
  obs <- zoo::zoo(1:10, 1:10)
  sim <- obs

  out <- gof(sim, obs, do.pmr = TRUE)

  expect_equal(unname(out["KGE", 1]), 1)
  expect_true(is.na(out["PMR", 1]))
})
