test_that("models can be made from base regressions", {

  # Empty
  expect_length(model_archetype(), 0)

  m <- lm(mpg ~ hp + cyl, mtcars)
  m1 <- model_archetype(
    x = m,
    label = list(mpg ~ "Mileage"),
    role = list(hp ~ "exposure")
  )
  expect_length(m1, 1)
  expect_output(print(m1), "lm")

  m <- glm(am ~ hp + cyl, mtcars, family = "binomial")
  m2 <- model_archetype(
    m,
    label = list(am ~ "Automatic", cyl ~ "Cylinders"),
    role = list(am ~ "outcome")
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
      label = list(mpg ~ "Mileage")
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

  # Handling list of models
  m <- md(x)
  expect_length(m, 3)

  # If unnamed
  x <- list(m1, m2, m3)
  m <- md(x)

  # Expect error
  expect_error(md(list(first = m1, second = "test")))

})


test_that("strata information can be passed along safely", {

  fit <- lm(mpg ~ hp, data = subset(mtcars, am == 0))
  m <- md(fit, strata_info = am ~ 0)
  expect_type(vec_data(m)$strata_info, "character")
  f <- as.formula(vec_data(m)$strata_info)
  expect_s3_class(f, "formula")
  expect_type(f[[3]], "double")
  expect_type(f[[2]], "symbol")

})
