test_that("custom formuals can be initialized", {

  # Scripts
  f <- mpg + wt ~ X(hp) + X(cyl) + gear + drat + log(qsec)
  t <- tm(f)
  x <- rx(t, pattern = "direct")
  fl <- fmls(x, order = 2:4)
  expect_length(fl, 7)

  # Terms
  f <- mpg + wt ~ X(hp) + X(cyl) + gear + drat + log(qsec)
  t <- term_archetype(f)
  x <- formula_archetype(t)
  expect_s3_class(x, "formula_archetype")

  y <- term_archetype("Y", side = "left")
  x <- term_archetype("X", side = "right")
  t <- c(x, y)
  f <- fmls(t, order = 1)

  # Formula
  f <- mpg ~ wt + hp + S(cyl)
  x <- formula_archetype(f)
  expect_s3_class(x, "formula_archetype")
  expect_true(is_formula(x))

  # Character look alikes
  f <- "mpg ~ wt + hp"
  x <- formula_archetype(f)
  expect_match(as.character(field(x, "outcome")[[1]]), "mpg")

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
  x <- fmls(f, order = 1:4)
  expect_length(x, 1)

  # Complex breakdown to order 1
  f <- rx(mpg + wt ~ X(hp) + am, pattern = "sequential")
  x <- fmls(f, order = 1:2)
  expect_length(x, 6)
  expect_length(x[field(x, "n") == 2], 4)

  # Long lists of sequential with default order of 2:4
  f <- mpg + wt ~ X(hp) + X(cyl) + gear + drat + qsec
  labels <- list(mpg ~ "Mileage", hp ~  "Horsepower")
  tiers <- list(c(drat, qsec) ~ "speed", wt ~ "hardware")
  t <- tm(f, label = labels, tier = tiers)
  x <- rx(t, pattern = "sequential")

  # Mediation break down works
  t <- tm(mpg + wt ~ X(hp) + M(cyl) + qsec)
  f <- rx(t)
  x <- fmls(f, order = 1:4)
  expect_length(x, 9)

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
  expect_equal(as.character(field(fl, "strata")[[1]]), "cyl")
})


test_that("multiple strata expand appropriately", {

  x <- rx(mpg ~ X(wt) + C(hp) + S(am) + S(vs), pattern = "direct")
  f <- fmls(x, order = 2)
  expect_length(f, 2)
  f <- fmls(x, order = 3)
  expect_length(f, 1)
  expect_s3_class(f, "formula_archetype")

  # Should have three formulas total when sequential
  f <- mpg ~ X(wt) + hp + qsec + S(cyl)
  labels <- list(mpg ~ "Mileage", hp ~  "Horsepower")
  tiers <- list(c(drat, qsec) ~ "speed", wt ~ "hardware")
  t <- tm(f, label = labels, tier = tiers)
  x <- rx(t, pattern = "sequential")
  expect_length(fmls(x, order = 1:4), 5)
  expect_length(fmls(x), 3)

})

test_that("survival terms work for formulas", {

  # Testing more complex survival models
  x <- rx(
    Surv(death_timeto, death_any_yn) + Surv(death_timeto, death_cv_yn) ~
      X(hf_stress_rest_delta_zn) + hf_rest_ln_zn + age_bl + blackrace +  hx_hypertension_bl + hx_diabetes_bl + hx_hbchol_bl + cath_gensini_bl + ejection_fraction + S(female_bl),
    pattern = "sequential"
  )
  f <- fmls(x)
  expect_length(f, 37)

})