test_that("scripts can be re-expanded into formulas", {

  # Direct
  f <- mpg + wt ~ X(hp) + X(cyl) + gear
  t <- term_archetype(f)
  x <- prescribe(t, pattern = "direct")
  lof <- construct_script(x)
  expect_length(lof, 4)

  # Sequential
  f <- mpg + wt ~ X(hp) + X(cyl) + drat + qsec
  t <- term_archetype(f, tier = list(drat + qsec ~ "secondary"))
  x <- prescribe(t, pattern = "sequential")
  lof <- construct_script(x)
  expect_length(lof, 8)

  # Parallel
  f <- mpg + wt ~ X(hp) + X(cyl) + drat + qsec
  t <- term_archetype(f, tier = list(qsec ~ "measurement"))
  x <- prescribe(t, pattern = "parallel")
  lof <- construct_script(x)
  expect_length(lof, 8)
})

test_that("mediation creates appropriate lists", {

  # Simple mediation
  x <- Surv(stop, status) ~ X(primary) + X(secondary) + M(mediator)
  t <- tm(x)
  f <- rx(t)
  lof <- construct_script(f)
  expect_length(lof, 5)

  # Mediation with covariates
  x <- Surv(stop, status) + Surv(stop, censor) ~ X(exposure) + M(mediator) + covariate
  t <- tm(x)
  f <- rx(t, pattern = "direct")
  lof <- construct_script(f)
  expect_length(lof, 5)
  f <- rx(t, pattern = "sequential")
  lof <- construct_script(f)
  expect_length(lof, 8)
  f <- rx(t, pattern = "parallel")
  lof <- construct_script(f)
  expect_length(lof, 5)
})

test_that("strata can be made appropriately", {

  f <- mpg ~ X(wt) + hp + qsec + S(cyl)
  labels <- list(mpg ~ "Mileage", hp ~  "Horsepower")
  tiers <- list(qsec ~ "speed", wt ~ "hardware")
  t <- tm(f, label = labels, tier = tiers)
  x <- rx(t, pattern = "sequential")
  lof <- construct_script(x)
  expect_length(lof, 3)
})
