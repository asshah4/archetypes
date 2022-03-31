test_that("custom formuals can be initialized", {

  # Terms
  f <- mpg + wt ~ X(hp) + X(cyl) + gear + drat + log(qsec)
  t <- term_archetype(f)
  x <- formula_archetype(t)
  expect_s3_class(x, "formula_archetype")

  # Formula
  f <- mpg ~ wt + hp
  x <- formula_archetype(f)
  expect_s3_class(x, "formula_archetype")
})

test_that("casting and coercion can occur", {
  f <- mpg + wt ~ X(hp) + X(cyl) + gear + drat + log(qsec)
  t <- tm(f)
  x <- rx(t, pattern = "sequential")
  lof <- construct_script(x)
})
