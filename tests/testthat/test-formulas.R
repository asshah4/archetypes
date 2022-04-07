test_that("custom formuals can be initialized", {

  # Scripts
  f <- mpg + wt ~ X(hp) + X(cyl) + gear + drat + log(qsec)
  t <- tm(f)
  x <- rx(t, pattern = "direct")
  fl <- fmls(x)
  expect_length(fl, 8)

  # Terms
  f <- mpg + wt ~ X(hp) + X(cyl) + gear + drat + log(qsec)
  t <- term_archetype(f)
  x <- formula_archetype(t)
  expect_s3_class(x, "formula_archetype")

  # Formula
  f <- mpg ~ wt + hp + S(cyl)
  x <- formula_archetype(f)
  expect_s3_class(x, "formula_archetype")
})

test_that("appropriate family tracking occurs in strata", {
  f <- mpg ~ X(wt) + hp + qsec + S(cyl)
  label <- list(mpg ~ "Mileage", hp ~ "Horsepower")
  tier <- list(qsec ~ "speed", wt ~ "hardware")
  t <- tm(f, label = label, tier = tier)
  x <- rx(t, pattern = "sequential")
  fl <- fmls(x)
  expect_equal(field(fl, "order")[1], 2)
  expect_equal(field(fl, "order")[2], 2)
  expect_equal(field(fl, "formula")[1], "mpg ~ wt")
  expect_equal(field(fl, "ancestor")[1], field(fl, "ancestor")[2])
  expect_equal(field(fl, "strata")[1], "cyl")
})
