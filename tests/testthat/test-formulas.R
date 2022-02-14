test_that("basic formula vector can be made and displayed", {

	# Construction
	f <- mpg + wt ~ X(hp) + X(cyl) + gear + drat + log(qsec)
	t <- term_rx(f)
	f1 <- formula_rx(t, pattern = "direct")
	labels <- list(mpg ~ "Mileage", cyl ~ "Cylinders")
	expect_length(rhs(f1), 5)
	expect_length(lhs(f1), 2)

	expect_error(validate_class(f1, "formula"))
	expect_silent(validate_class(t, "term_rx"))
	expect_s3_class(f1, "formula_rx")
	expect_equal(f1, formula_rx(x = term_rx(f), pattern = "direct"))
	expect_s3_class(frx(f), "formula_rx") # Until formal implementation is made

	# Vectorization
	t1 <- term_rx(mpg ~ wt)
	t2 <- term_rx(mpg ~ hp)
	f1 <- formula_rx(t1)
	f2 <- formula_rx(t2)
	f <- c(f1, f2)
	expect_length(f, 2)

	# Printing
	expect_output(print(f1), "[1]")
	expect_output(print(new_formula_rx()), "[0]")
	if (isTRUE(requireNamespace("tibble", quietly = TRUE))) {
		tibble::tibble(f1) |>
			print() |>
			expect_output("<fx>")
	}

})

test_that("formula_rx() inputs are acceptable", {

	# Groups
	t1 <- term_rx(mpg ~ wt + hp + drat + qsec)
	t2 <- term_rx("gear", side = "right", group = "hardware")
	t3 <- term_rx("cyl", side = "right", group = "hardware")
	t4 <- c(t1, t2, t3)
	expect_length(t4, 7)
	f1 <- formula_rx(t4)
	groups <- list(hardware ~ c(wt), speed ~ c(drat, qsec))
	f2 <- formula_rx(t4, groups = groups)

	# Patterns
	t <- term_rx(mpg + wt ~ X(hp) + X(cyl) + gear + drat + log(qsec))
	expect_error(formula_rx(x = t, pattern = "error"))

	# Using a formula directly
	expect_length(formula_rx(formula()), 0)
	x <- mpg + qsec ~ X(wt) + M(hp)
	f <- formula_rx(x)
	expect_error(formula_rx(x, pattern = "error"))

	# Modifiers such as roles, labels, and groups are incorporated
	f <- formula_rx(x, labels = list(hp ~ "Horsepower"))
	expect_length(labels(f), 1) # Currently erroring

})

test_that("complex formulas can be made", {

	# Survival
	x <- Surv(stop, status) + Surv(stop, censor) ~ X(primary) + secondary + tertiary
	t <- trx(x)
	f1 <- frx(x)
	f2 <- frx(t)
	expect_equal(f1, f2)

	# Mediation
	x <- Surv(stop, status) + Surv(stop, censor) ~ X(primary) + M(secondary) + tertiary
	t <- trx(x)
	f1 <- frx(x)
	f2 <- frx(t)
	expect_equal(f1, f2)

	# Multiple exposures and outcomes
	x <- Surv(stop, status) + Surv(stop, censor) ~ X(primary) + X(secondary) + tertiary
	f1 <- frx(Surv(stop, status) + Surv(stop, censor) ~ X(primary) + X(secondary) + tertiary)
	f2 <- frx(x)
	expect_equal(f1, f2)

})

test_that("vctrs casting and coercion work appropriately", {

	f <- mpg + wt ~ X(hp) + X(cyl) + gear + drat + log(qsec)
	t <- term_rx(f)
	f1 <- formula_rx(x = t, pattern = "direct")

	# formula_rx
	f2 <- vec_cast(x = f, to = formula_rx())
	expect_equal(f1, f2)
	expect_output(print(vec_ptype2(f1, t)), "term_rx")
	expect_output(print(vec_ptype2(f1, f2)), "formula_rx")

	# Characters
	expect_type(as.character(f1), "character")
	expect_type(vec_ptype2(formula_rx(), character()), "character")
	expect_type(vec_ptype2(character(), formula_rx()), "character")

	# Between terms and formulas
	x <- mpg + qsec ~ X(wt) + M(hp)
	f0 <- formula_rx(x)
	t1 <- term_rx(x)
	t2 <- term_rx(f0)
	expect_equal(t1, t2)
	f1 <- formula_rx(t1)
	expect_equal(f0, f1)

})

test_that("formula vectors can be modified in place", {


	# Updates to the right
	x <- mpg + wt ~ hp + cyl + gear
	t <- term_rx(x)
	f1 <- formula_rx(t)
	object <- f1
	parameters <- ~ drat - gear
	expect_length(rhs(parameters, tidy = FALSE), 1)
	f2 <- update(object, parameters)
	expect_length(f2, 1)
	expect_no_match(f2, "gear")

	# Updates to the left
	object <- f2
	parameters <- gear - wt ~ wt
	expect_length(lhs(parameters, tidy = FALSE), 1)
	f3 <- update(object, parameters)
	expect_length(f3, 1)
	expect_length(term_rx(f3), 6)
	expect_match(f3, "mpg\ \\+\ gear")


	# Complex addition and subtraction via updates
	object <- f3
	parameters <- -mpg ~ -cyl - drat -wt
	f4 <- update(object, parameters)
	expect_match(f4, "gear ~ hp")

	# Addition
	x <- mpg + wt ~ X(hp) + X(cyl) + gear
	t <- term_rx(x)
	f1 <- formula_rx(t)
	f2 <- formula_rx(t[1:4])
	f3 <- add(f2, t[5])
	expect_equal(f1, f3)
	expect_s3_class(update(f2, ~ gear), "formula_rx")


})
