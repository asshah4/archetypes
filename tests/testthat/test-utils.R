test_that("term matching works correctly", {

	# Create list of formulas to test term matchign
	t <- tm(mpg + wt ~ hp + am + vs + cyl)
	f <- deparse1(stats::formula(t)) # Trace/pare
	fl <- fmls(t, order = 1) # Unit level paths
	tl <- tm()

	for (i in seq_along(fl)) {
		tl <- append(tl, match_terms(t, lhs(fl[i])))
	}

	expect_length(unique(tl), 2)
})


test_that("list manipulation occurs for formulas, tables, and named lists", {

	# Named lists of different varieties
  label <- list(mpg ~ "Mileage", hp ~ Horsepower)
  tier <- list("qsec" ~ "speed", wt ~ "hardware")
  role <- list(cyl + gear ~ "confounder")

  # Convert to formulas
  expect_named(formula_to_named_list(label), c("mpg", "hp"))
  expect_named(formula_to_named_list(tier), c("qsec", "wt"))
  expect_named(formula_to_named_list(role), c("cyl", "gear"))

  # Convert back to named list
  x <- formula_to_named_list(role)
  y <- named_list_to_formula(x)
  expect_length(y, 2)
  expect_equal(y[[1]], cyl ~ "confounder", ignore_attr = TRUE)


})

test_that("sides can be obtained from formulaic objects", {

	x <- rx(mpg + wt ~ X(hp) + M(cyl) + qsec)
	fl <-
		fmls(x, order = 1:4) |>
		{
			\(.x) .x[field(.x, "n") == 2]
		}()
	# Number 5 doesn't have a "left" hand side, which is wrong.
	f <- fl[5]
	expect_equal(lhs(f), "cyl")


})