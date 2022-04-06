test_that("formulas can be made with appropriate roles and complexity", {

  f <- y ~ X(x) + M(m) + c
  t <- tm(f)
  x <- rx(t)


})


test_that("custom formuals can be initialized", {

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

test_that("casting and coercion can occur", {
  f <- mpg + wt ~ X(hp) + X(cyl) + gear + drat + log(qsec)
  t <- tm(f)
  x <- rx(t, pattern = "sequential")
  lof <- construct_script(x)
  expect_length(lof, 16)
})

test_that("appropriate family tracking occurs in strata", {
  f <- mpg ~ X(wt) + hp + qsec + S(cyl)
  labels <- list(mpg ~ "Mileage", hp ~ "Horsepower")
  tiers <- list(qsec ~ "speed", wt ~ "hardware")
  t <- tm(f, label = labels, tier = tiers)
  x <- rx(t, pattern = "sequential")
  fl <- construct_script(x)
  expect_equal(field(fl, "stage")[1], "unit")
  expect_equal(field(fl, "stage")[2], "simple")
  expect_equal(field(fl, "formula")[1], "mpg ~ wt")
  expect_equal(field(fl, "family")[1], field(fl, "family")[2])
  expect_equal(field(fl, "source")[1], "script")
  expect_equal(field(fl, "strata")[1], "cyl")
})
