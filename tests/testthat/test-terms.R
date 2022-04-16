test_that("new term_archetypes can be made from character/atomic components", {
  ty <- term_archetype(
    x = "Y",
    side = "left",
    role = "outcome",
    label = "Dependent Variable",
    description = "Artificially created",
    distribution = "normal",
    type = "numeric",
    subtype = "continuous"
  )

  tm <- term_archetype(
    "X",
    side = "right",
    role = "exposure",
    label = "Independent Variable",
    description = "Artificially created",
    distribution = "normal",
    type = "numeric",
    subtype = "dichotomous"
  )

  tm <- term_archetype(
    "M",
    side = "right",
    role = "mediator",
    label = "Independent Variable",
    description = "Artificially created",
    distribution = "normal",
    type = "integer",
    subtype = "continuous"
  )

  tc <- term_archetype(
    "C",
    side = "right",
    role = "covariate",
    label = "Independent Variable",
    description = "Artificially created",
    distribution = "normal",
    type = "character",
    subtype = "categorical"
  )

  ts <- term_archetype(
    "S",
    side = "meta",
    role = "strata",
    label = "Stratification Variable",
    description = "Levels for data set",
    distribution = "binary",
    type = "character",
    subtype = "dichotomous"
  )

  t <- c(ty, tm, tm, tc, ts)

  expect_length(t, 5)
  expect_true(is_term(t))

  # Inappropriate variables should lead to stop
  x <- tm("M", side = "right")
  y <- tm("Y", side = "left")
  role = list(M ~ "mediator", Y ~ "covariate")
  expect_error(set_roles(c(x, y), roles = formula_to_named_list(role)))

})

test_that("term_archetype() makes term_archetype object or errors", {

  # Messages for zero length objects
  expect_message(term_archetype(formula()))
  expect_message(term_archetype(character()))
  expect_message(term_archetype(data.frame()))

  t1 <- term_archetype("y", side = "left", role = "outcome", label = "Dependent Variable")
  t2 <- tm("x", side = "right", role = "exposure", label = "Independent Variable")
  expect_s3_class(t1, "term_archetype")
  expect_true(is_term(t1))
  expect_error(new_term("x"))
  expect_length(t1, 1)
  expect_length(t2, 1)
  expect_length(suppressMessages(term_archetype(formula())), 0)

  # Field size should be the same
  expect_error(term_archetype(c("x", "y")))

  # Expected class of input matters
  expect_error(term_archetype(as.name("x")))
})

test_that("formatting is correct", {
  t1 <- term_archetype("y", side = "left", role = "outcome", label = "Dependent Variable")
  t2 <- term_archetype("x", side = "right", role = "exposure", label = "Independent Variable")
  vt <- c(t1, t2)
  expect_output(print(t1), "y")
  expect_output(print(new_term()), "[0]")

  if (isTRUE(requireNamespace("tibble", quietly = TRUE))) {
    tibble::tibble(vt) |>
      print() |>
      expect_output("<tm>")
  }
})

test_that("casting and coercion for different dispatches work", {

  # Basic cast into character
  x1 <- term_archetype("x1", side = "right", role = "exposure", label = "Independent Variable")
  x2 <- term_archetype("x2", side = "right", role = "confounder", label = "Independent Variable")
  y <- "y"
  expect_type(c(x1, y), "character")
  expect_s3_class(c(x1, x2), "term_archetype")
  expect_type(vec_c(x1, y), "character")

  # Formula archetypes
  s <- rx(mpg ~ X(wt) + M(cyl) + hp)
  f <- fmls(s)
  t <- tm(f)
    expect_length(t, 4)
  expect_equal(decipher(t), 3)
  expect_equal(decipher(t), field(s, "order"))
  expect_equal(length(t), length(field(s, "terms")[[1]]))

})


test_that("terms can be generated from formulas", {

  # Simple formula for term_archetypes to be broken down
  ts <- term_archetype.formula(
    x = mpg + wt ~ hp + cyl + gear,
    tier = list(cyl ~ "engine", gear ~ "engine"),
    label = list(mpg ~ "Mileage")
  )
  expect_length(ts, 5)

  # Complex formula with term_archetype and data operations
  f <- mpg + wt ~ X(hp) + M(cyl) + gear + drat + log(qsec)
  t <- term_archetype(
    x = f,
    tier = list(drat + qsec ~ "spec"),
    label = list(mpg ~ "Mileage", wt ~ "Weight")
  )
  expect_length(t, 7)

  # Reversing a term object into a formula
  expect_s3_class(stats::formula(t), "formula")

  t1 <- term_archetype(f)
  t2 <- term_archetype(f, label = list(mpg ~ "Mileage"), tier = list(qsec + drat ~ "speed"))
  expect_equal(vec_size(t1), 7)
  expect_equal(vec_size(t1), length(t1))
  expect_length(tiers(t2), 2)

  # Adding roles and labels works, including strata
  f <- mpg ~ X(hp) + M(gear) + drat + S(cyl)
  tm <-
    term_archetype(f, label = list(gear ~ "Gears")) |>
    vec_data()
  expect_equal(tm$role[tm$terms == "gear"], "mediator")
  expect_equal(tm$label[tm$terms == "gear"], "Gears")
  expect_equal(tm$side[tm$terms == "cyl"], "meta")
  expect_equal(tm$role[tm$terms == "cyl"], "strata")


})

test_that("terms can be made from a fitted model", {

  # lm models
  m_lm <- lm(mpg ~ wt + hp + cyl, mtcars)
  t_lm <- term_archetype(m_lm)
  expect_length(t_lm, 4)

  # glm
  m_glm <- glm(am ~ wt + hp, mtcars, family = "binomial")
  t_glm <- term_archetype(m_glm, label = list(am ~ "Automatic Transmission"))
  expect_length(t_glm, 3)
  expect_equal(labels(t_glm)$am, "Automatic Transmission")

  # Model spec of parsip models
  if (isTRUE(requireNamespace("parsnip", quietly = TRUE))) {
    m_parsnip <-
      parsnip::linear_reg() |>
      parsnip::set_engine("lm") |>
      parsnip::fit(mpg ~ ., data = mtcars)
    t_parsnip <- term_archetype(m_parsnip)
    expect_length(t_parsnip, 11)
  }
})


