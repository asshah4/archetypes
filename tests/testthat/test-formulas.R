test_that("custom formuals can be initialized", {

  # Scripts
  f <- mpg + wt ~ X(hp) + X(cyl) + gear + drat + log(qsec)
  t <- tm(f)
  x <- rx(t, pattern = "direct")
  fl <- fmls(x, order = 2:4)
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
  expect_true(is_formula(x))

  # Character look alikes
  f <- "mpg ~ wt + hp"
  x <- formula_archetype(f)
  expect_match(field(x, "outcome")[[1]], "mpg")

  # Errors
  expect_error(formula_archetype(1))
  expect_error(formula_archetype("test"))
})

test_that("output is appropriate", {

  # Empty
  expect_output(print(fmls()), "[0]")

  # Simple output
  f <- rx(mpg ~ wt + hp + S(cyl), pattern = "sequential")
  x <- formula_archetype(f)
  expect_type(format(x), "character")

  # Tibble
  if (isTRUE(requireNamespace("tibble", quietly = TRUE))) {
    tibble::tibble(x) |>
      print() |>
      expect_output("<fmls>")
  }

})

test_that("casting and coercion for formulas works", {

  # Character
  f <- "mpg ~ wt + hp"
  x <- c(fmls(f), f)
  expect_length(x, 2)
  expect_type(x, "character")
  x <- vec_c(f, fmls(f))
  expect_length(x, 2)
  expect_type(x, "character")


})

test_that("appropriate orders of formulas occur", {

  # Simple order check
  f <- rx(mpg ~ wt + hp, pattern = "direct")
  x <- fmls(f, order = 1)
  expect_length(x, 2)

  # Complex breakdown to order 1
  f <- rx(mpg + wt ~ X(hp) + am)
  x <- fmls(f, order = 1:2)
  expect_length(x, 6)
  expect_length(x[field(x, "n") == 2], 4)

  # Mediation break down works
  t <- tm(mpg + wt ~ X(hp) + M(cyl) + am)
  f <- rx(t)
  x <- fmls(f, order = 1:4)
  expect_length(x, 6)
  expect_length(x[field(x, "n") == 2], 4)



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

