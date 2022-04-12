test_that("formulas can be made with appropriate roles and complexity", {

  # Zeroeth order
  t <- tm(~x)
  expect_equal(decipher(t), 0)

  # First order
  t <- tm(y ~ x)
  expect_equal(decipher(t), 1)

  # Second order
  t <- tm(y ~ X(x) + c)
  expect_equal(decipher(t), 2)

  # Third order/mediation
  t <- tm(y ~ X(x) + M(m) + c)
  expect_equal(decipher(t), 3)

  # Fourth order/script
  t <- tm(y1 + y2 ~ X(x) + M(m) + C(c))
  expect_equal(decipher(t), 4)

})

test_that("scripts can be decomposed appropriately", {

  # Fourth order scripts into third order
  t <- tm(y1 + y2 ~ X(x1) + X(x2) + C(c1) + C(c2))
  s4 <- rx(t)
  expect_equal(field(s4, "order"), 4)
  s3 <- recompose_roles(s4)
  expect_length(s3, 3) # Third order decomposition
  expect_equal(format(s3[1]), "y1 + y2 ~ x1 + x2 + c1 + c2")

  # Third order/mediation
  t <- tm(y ~ X(x) + M(m) + c)
  s3 <- rx(t)
  s2 <- recompose_roles(s3)
  expect_length(s2, 4) # Second order decomposition
  expect_equal(format(s2[2]), "y ~ x + c")

  # Second order recomposition (into first order)
  t <- tm(y ~ X(x) + c)
  expect_equal(decipher(t), 2)
  s <- rx(t)
  sl <- recompose_roles(s)
  expect_length(sl, 3)
  expect_equal(field(sl, "formula")[2], "y ~ x")

  # Multiple order decompositions
  f <- mpg + wt ~ X(hp) + X(cyl) + gear + drat + log(qsec)
  t <- tm(f)
  x <- rx(t)
  s1 <- recompose_roles(x)
  expect_length(s1, 3)
  s2 <- recompose_roles(s1)
  expect_length(s2, 7)

  # Complex break down with mediation
  s <- rx(mpg + wt ~ X(hp) + M(cyl) + am)
  s1 <- recompose_roles(x)
  expect_equal(min(field(s1, "order")), 3)
  s2 <- recompose_roles(s1)
  s3 <- recompose_roles(s2[6])


})

test_that("scripts can be re-expanded into formulas", {

  # Direct
  f <- mpg + wt ~ X(hp) + X(cyl) + gear
  t <- term_archetype(f)
  x <- prescribe(t, pattern = "direct")
  lof <- decompose_patterns(x)
  expect_length(lof, 2)

  # Sequential
  f <- mpg + wt ~ X(hp) + X(cyl) + drat + qsec
  t <- term_archetype(f, tier = list(drat + qsec ~ "secondary"))
  x <- prescribe(t, pattern = "sequential")
  lof <- decompose_patterns(x)
  expect_length(lof, 4)

  # Parallel
  f <- mpg + wt ~ X(hp) + X(cyl) + drat + qsec
  t <- term_archetype(f, tier = list(qsec ~ "measurement"))
  x <- prescribe(t, pattern = "parallel")
  lof <- decompose_patterns(x)
  expect_length(lof, 4)
})

test_that("mediation creates appropriate lists", {

  # Simple mediation
  f <- Surv(stop, status) ~ X(primary) + X(secondary) + M(mediator)
  t <- tm(f)
  x <- rx(t)
  sl <- recompose_roles(x)
  expect_length(sl, 5)

  # Mediation with covariates
  f <- Surv(stop, status) + Surv(stop, censor) ~ X(exposure) + M(mediator) + confounder + covariate + predictor
  t <- tm(f)
  x <- rx(t, pattern = "direct")
  sl <- recompose_roles(x)
  fl <- decompose_patterns(sl)
  expect_length(sl, 3)
  expect_length(fl, 3)

  x <- rx(t, pattern = "parallel")
  fl <- decompose_patterns(x)
  expect_length(fl, 3)

  x <- rx(t, pattern = "sequential")
  fl <- decompose_patterns(x)
  expect_length(fl, 4)

  # Mediation complexity
  s <- rx(mpg ~ X(wt) + M(cyl) + hp)
  sl <- recompose_roles(s)
  fl <- decompose_patterns(sl)
  f <- fmls(s) # Not appropriately adding hte mediation class here
  expect_equal(field(f, "formula")[4], "cyl ~ wt + hp")

})

test_that("strata can be made appropriately", {

  f <- mpg ~ X(wt) + hp + qsec + S(cyl)
  labels <- list(mpg ~ "Mileage", hp ~  "Horsepower")
  tiers <- list(qsec ~ "speed", wt ~ "hardware")
  t <- tm(f, label = labels, tier = tiers)
  s <- rx(t, pattern = "sequential")
  sl <- recompose_roles(s)
  expect_length(sl, 4)
})

