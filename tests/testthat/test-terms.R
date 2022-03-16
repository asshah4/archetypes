test_that("new terms can be made from character/atomic components", {

	ty <- term_rx(
		"Y",
		side = "left",
		role = "outcome",
		label = "Dependent Variable",
		description = "Artificially created",
		distribution = "normal",
		type = "numeric",
		subtype = "continuous"
	)

	tx <- term_rx(
		"X",
		side = "right",
		role = "exposure",
		label = "Independent Variable",
		description = "Artificially created",
		distribution = "normal",
		type = "numeric",
		subtype = "dichotomous"
	)

	tm <- term_rx(
		"M",
		side = "right",
		role = "mediator",
		label = "Independent Variable",
		description = "Artificially created",
		distribution = "normal",
		type = "integer",
		subtype = "continuous"
	)

	tc <- term_rx(
		"C",
		side = "right",
		role = "covariate",
		label = "Independent Variable",
		description = "Artificially created",
		distribution = "normal",
		type = "character",
		subtype = "categorical"
	)

	ts <- term_rx(
		"S",
		side = "meta",
		role = "strata",
		label = "Stratification Variable",
		description = "Levels for data set",
		distribution = "binary",
		type = "character",
		subtype = "dichotomous"
	)

	t <- c(ty, tx, tm, tc, ts)

	expect_length(t, 5)
})


test_that("term_rx() makes term object or errors", {

	t1 <- term_rx("y", side = "left", role = "outcome", label = "Dependent Variable")
	t2 <- tx("x", side = "right", role = "exposure", label = "Independent Variable")
	expect_s3_class(t1, "term_rx")
	expect_true(is_term_rx(t1))
	expect_error(new_term("x"))
	expect_equal(length(t2), vec_size(t1))
	expect_length(term_rx(formula()), 0)

	# Field size should be the same
	expect_error(term_rx(c("x", "y")))

	# Expected class of input matters
	expect_error(term_rx(as.name("x")))

})

test_that("terms can be generated from formulas", {

	# Simple formula for terms to be broken down
	fs <- mpg + wt ~ hp + cyl + gear
	ts <- term_rx(
		x = fs,
		groups = list(cyl ~ "engine", gear ~ "engine"),
		labels = list(mpg ~ "Mileage")
	)
	expect_length(ts, 5)

	# Complex formula with term and data operations
	fc <- mpg + wt ~ X(hp) + M(cyl) + gear + drat + log(qsec)
	tc <- term_rx(

	)


	t1 <- term_rx(f)
	t2 <- term_rx(f, labels = list(mpg ~ "Mileage"), groups = list(qsec + drat ~ "speed"))
	expect_equal(vec_size(t1), 7)
	expect_equal(vec_size(t1), length(t1))
	expect_length(groups.term_rx(t2), 2)

	# Adding roles and labels works
	tm <-
		term_rx(f, labels = list(gear ~ "Gears")) |>
		vec_data()
	expect_equal(tm$role[tm$term == "cyl"], "mediator")
	expect_equal(tm$label[tm$term == "gear"], "Gears")

})

test_that("terms can be made from a fitted model", {

})
test_that("formatting is correct", {

	t1 <- term_rx("y", side = "left", role = "outcome", label = "Dependent Variable")
	t2 <- term_rx("x", side = "right", role = "exposure", label = "Independent Variable")
	vt <- c(t1, t2)
	expect_output(print(t1), "y")
	expect_output(print(new_term()), "[0]")

	if (isTRUE(requireNamespace("tibble", quietly = TRUE))) {
		tibble::tibble(vt) |>
			print() |>
			expect_output("<tx>")
	}

})

test_that("vctr based operations work", {

	# Basic casting
	x1 <- term_rx("x1", side = "right", role = "exposure", label = "Independent Variable")
	x2 <- term_rx("x2", side = "right", role = "covariate", label = "Independent Variable")
	y <- "y"
	expect_type(c(x1, y), "character")
	expect_s3_class(c(x1, x2), "term_rx")
	expect_type(vec_c(x1, y), "character")

})

