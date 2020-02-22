library(testthat)
context("reasonable add_binom_gest values")
set.seed(42)
obs <- 200
dat <- tibble(prob = rbeta(obs, 10, 50),
                  n = round(rlnorm(obs, 4, 2)) + 1,
                  x = rbinom(obs, n, prob))

result <- add_binom_gest(dat, x, n)

testthat::test_that("add_binom_gest returns correct values", {

  testthat::expect_lt(result$.lo[1],result$.hi[1])
  testthat::expect_lt(result$.lo[1],result$.gest[1])
  testthat::expect_lt(result$.gest[1],result$.hi[1])

})

context("check add_binom_gest colnames")

testthat::test_that("add_binom_gest returns correct colnames", {

  testthat::expect_true(all(c(".raw", ".gest_dist", ".gest", ".lo", ".hi") %in% colnames(result)))

})
