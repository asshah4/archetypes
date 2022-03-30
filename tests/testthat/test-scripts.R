test_that("a formula can be upgraded into a prescribe object", {

	f <- prescribe(
		mpg + wt ~ hp + cyl + gear + drat + qsec,
		role = list(hp ~ "exposure", cyl ~ "mediator")
	)
	expect_length(f, 1)
	expect_length(rhs(f), 5)
	expect_length(lhs(f), 2)
})

test_that("basic formula vector can be made and displayed", {

	# Construction
	f <- mpg + wt ~ X(hp) + X(cyl) + gear + drat + log(qsec)
	t <- term_archetype(f)
	f1 <- prescribe(t, label = list(mpg ~ "Mileage", cyl ~ "Cylinders"))
	expect_length(rhs(f1), 5)
	expect_length(lhs(f1), 2)

	expect_silent(validate_class(f1, "script"))
	expect_silent(validate_class(t, "term_archetype"))
	expect_s3_class(f1, "script")
	expect_equal(f1,
				 prescribe(x = term_archetype(
				 	f,
				 	label = list(mpg ~ "Mileage", cyl ~ "Cylinders")
				 )))
	expect_s3_class(rx(f), "script") # Until formal implementation is made

	# Vectorization
	t1 <- term_archetype(mpg ~ wt)
	t2 <- term_archetype(mpg ~ hp)
	f1 <- prescribe(t1)
	f2 <- prescribe(t2)
	f <- c(f1, f2)
	expect_length(f, 2)

	# Printing
	expect_output(print(f1), "[1]")
	expect_output(print(new_script()), "[0]")
	if (isTRUE(requireNamespace("tibble", quietly = TRUE))) {
		tibble::tibble(f1) |>
			print() |>
			expect_output("<rx>")
	}

})

test_that("prescribe() inputs are acceptable", {

	# Groups
	t1 <- term_archetype(mpg ~ wt + hp + drat + qsec)
	t2 <- term_archetype("gear", side = "right", group = "hardware")
	t3 <- term_archetype("cyl", side = "right", group = "hardware")
	t4 <- c(t1, t2, t3)
	expect_length(t4, 7)
	f1 <- prescribe(t4)
	groups <- list(gear + cyl ~ "hardware")
	f2 <- prescribe(t4, group = groups)
	expect_equal(f1, f2)

	t <- term_archetype(mpg + wt ~ X(hp) + X(cyl) + gear + drat + log(qsec))
	expect_length(lhs(rx(t)), 2)

	# Using a formula directly
	expect_message(prescribe(formula()))
	x <- mpg + qsec ~ X(wt) + M(hp)
	f <- prescribe(x)
	expect_error(prescribe("x"))

	# Modifiers such as roles, labels, and groups are incorporated
	f <- prescribe(x, label = list(hp ~ "Horsepower"))
	expect_length(labels(f), 1) # Currently erroring

})

test_that("complex survival formulas can be made", {

	# Survival
	x <- Surv(stop, status) + Surv(stop, censor) ~ X(primary) + secondary + tertiary
	t <- tm(x)
	f1 <- rx(x)
	f2 <- rx(t)
	expect_equal(f1, f2)

	# Mediation
	x <- Surv(stop, status) + Surv(stop, censor) ~ X(primary) + M(secondary) + tertiary
	t <- tm(x)
	f1 <- rx(x)
	f2 <- rx(t)
	expect_equal(f1, f2)

	# Multiple exposures and outcomes
	x <- Surv(stop, status) + Surv(stop, censor) ~ X(primary) + X(secondary) + tertiary
	f1 <- rx(Surv(stop, status) + Surv(stop, censor) ~ X(primary) + X(secondary) + tertiary)
	f2 <- rx(x)
	expect_equal(f1, f2)

})

test_that("vctrs casting and coercion work appropriately", {

	f <- mpg + wt ~ X(hp) + X(cyl) + gear + drat + qsec
	t <- term_archetype(f)
	f1 <- prescribe(x = t)

	# prescribe()
	f2 <- prescribe(x = f)
	expect_equal(f1, f2)
	expect_output(print(vec_ptype2(f1, t)), "term_archetype")
	expect_output(print(vec_ptype2(f1, f2)), "script")

	# character()
	expect_type(as.character(f1), "character")
	expect_type(vec_ptype2(rx(x = character()), character()), "character")
	expect_type(vec_ptype2(character(), rx(x = character())), "character")

	# Between terms and formulas
	x <- mpg + qsec ~ X(wt) + M(hp)
	f0 <- prescribe(x)
	t1 <- term_archetype(x)
	t2 <- term_archetype(f0)
	expect_equal(t1, t2)
	f1 <- prescribe(t1)
	expect_equal(f0, f1)

	# Into formulas
	expect_s3_class(stats::formula(f0), "formula")

})

test_that("formula vectors can be modified in place", {

	# Updates to the right
	x <- mpg + wt ~ hp + cyl + gear
	t <- term_archetype(x)
	f1 <- prescribe(t)
	object <- f1
	parameters <- ~ drat - gear
	expect_length(rhs(parameters, tidy = FALSE), 1)
	f2 <- update(object, parameters)
	expect_length(f2, 1)
	expect_no_match(format(f2), "gear")

	# Updates to the left
	object <- f2
	parameters <- gear - wt ~ wt
	expect_length(lhs(parameters, tidy = FALSE), 1)
	f3 <- update(object, parameters)
	expect_length(f3, 1)
	expect_length(term_archetype(f3), 6)
	expect_match(format(f3), "mpg\ \\+\ gear")


	# Complex addition and subtraction via updates
	object <- f3
	parameters <- -mpg ~ -cyl - drat -wt
	f4 <- update(object, parameters)
	expect_match(format(f4), "gear ~ hp")

	# Addition
	x <- mpg + wt ~ X(hp) + X(cyl) + gear
	t <- term_archetype(x)
	f1 <- prescribe(t)
	f2 <- prescribe(t[1:4])
	f3 <- add(f2, t[5])
	expect_equal(f1, f3)
	expect_s3_class(update(f2, ~ gear), "script")


})

test_that("scripts can be re-expanded into formulas", {

	# Direct
	f <- mpg + wt ~ X(hp) + X(cyl) + gear
	t <- term_archetype(f)
	x <- prescribe(t)
	lof <- deconstruct_patterns(x, pattern = "direct")
	tbl <- reconstruct_patterns(lof, t)
	expect_length(lof, 4)
	expect_length(tbl, 7)

	# Sequential
	f <- mpg + wt ~ X(hp) + X(cyl) + drat + qsec
	t <- term_archetype(f, group = list(drat + qsec ~ "secondary"))
	x <- prescribe(t)
	lof <- deconstruct_patterns(x, pattern = "sequential")
	tbl <- reconstruct_patterns(lof, t)
	expect_length(lof, 8)
	expect_length(tbl, 7)

	# Parallel
	f <- mpg + wt ~ X(hp) + X(cyl) + drat + qsec
	t <- term_archetype(f, group = list(qsec ~ "measurement"))
	x <- prescribe(t)
	lof <- deconstruct_patterns(x, pattern = "parallel")
	tbl <- reconstruct_patterns(lof, t)
	expect_length(lof, 8)
	expect_length(tbl, 7)


})
