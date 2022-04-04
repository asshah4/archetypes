test_that("models can be made from base regressions", {

  # Empty
  expect_length(model_archetype(), 0)

  m <- lm(mpg ~ hp + cyl, mtcars)
  m1 <- model_archetype(
    x = m,
    term_labels = list(mpg ~ "Mileage"),
    term_roles = list(hp ~ "exposure"),
    label = "LM Test"
  )
  expect_length(m1, 1)
  expect_output(print(m1), "lm")

  m <- glm(am ~ hp + cyl, mtcars, family = "binomial")
  m2 <- model_archetype(
    m,
    term_labels = list(am ~ "Automatic", cyl ~ "Cylinders"),
    term_roles = list(am ~ "outcome"),
    label = "GLM Test"
  )
  expect_length(m2, 1)
  expect_output(print(m2), "glm")

  # Passing objects to the model cards
  m <- lm(mpg ~ wt, data = mtcars)
  m3 <- model_archetype(m)

  # Multiple objects as a vector
  x <- c(m1, m2, m3)

  # Error for lack of models or incorrect inputs
  expect_error(model_archetype("test"))

  # Basic output
  if (isTRUE(requireNamespace("tibble", quietly = TRUE))) {
    tibble::tibble(x) |>
      print() |>
      expect_output("<md>")
  }
})

test_that("model specs can also be used to generate archetypes", {

  # Model spec of parsip models
  if (isTRUE(requireNamespace("parsnip", quietly = TRUE))) {
    pm <-
      parsnip::linear_reg() |>
      parsnip::set_engine("lm") |>
      parsnip::fit(mpg ~ ., data = mtcars)

    mp <- model_archetype(
      pm,
      term_labels = list(mpg ~ "Mileage"),
      label = "Parsnip"
    )

    expect_length(mp, 1)
    expect_output(print(mp), "model_fit_lm")
  }
})

test_that("list of models will dispatch appropriately", {

  # Multiple objects as a vector
  m1 <- lm(mpg ~ hp + cyl, mtcars)
  m2 <- glm(am ~ hp + cyl, mtcars, family = "binomial")
  m3 <- lm(mpg ~ wt + gear, mtcars)
  x <- list(first = m1, second = m2, m3)

  ma <- model_archetype(x)
  expect_length(ma, 3)

  # Scripts to be extended
  f <- mpg + wt ~ X(hp) + X(cyl) + drat + qsec
  t <- term_archetype(f, tier = list(drat + qsec ~ "secondary"))
  x <- prescribe(t, pattern = "sequential")
  lof <- construct_script(x)

})
