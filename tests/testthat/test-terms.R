test_that("new terms can be made from character/atomic components", {

	ty <- term(
		x = "Y",
		side = "left",
		role = "outcome",
		label = "Dependent Variable",
		description = "Artificially created",
		distribution = "normal",
		type = "numeric",
		subtype = "continuous"
	)

	tx <- term(
		"X",
		side = "right",
		role = "exposure",
		label = "Independent Variable",
		description = "Artificially created",
		distribution = "normal",
		type = "numeric",
		subtype = "dichotomous"
	)

	tm <- term(
		"M",
		side = "right",
		role = "mediator",
		label = "Independent Variable",
		description = "Artificially created",
		distribution = "normal",
		type = "integer",
		subtype = "continuous"
	)

	tc <- term(
		"C",
		side = "right",
		role = "covariate",
		label = "Independent Variable",
		description = "Artificially created",
		distribution = "normal",
		type = "character",
		subtype = "categorical"
	)

	ts <- term(
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

test_that("term() makes term object or errors", {

	# Messages for zero length objects
	expect_message(term(formula()))
	expect_message(term(character()))
	expect_message(term(data.frame()))

	t1 <- term("y", side = "left", role = "outcome", label = "Dependent Variable")
	t2 <- tx("x", side = "right", role = "exposure", label = "Independent Variable")
	expect_s3_class(t1, "term")
	expect_true(is_term(t1))
	expect_error(new_term("x"))
	expect_length(t1, 1)
	expect_length(t2, 1)
	expect_length(suppressMessages(term(formula())), 0)

	# Field size should be the same
	expect_error(term(c("x", "y")))

	# Expected class of input matters
	expect_error(term(as.name("x")))

})

test_that("formatting is correct", {

	t1 <- term("y", side = "left", role = "outcome", label = "Dependent Variable")
	t2 <- term("x", side = "right", role = "exposure", label = "Independent Variable")
	vt <- c(t1, t2)
	expect_output(print(t1), "y")
	expect_output(print(new_term()), "[0]")

	if (isTRUE(requireNamespace("tibble", quietly = TRUE))) {
		tibble::tibble(vt) |>
			print() |>
			expect_output("<tm>")
	}

})

test_that("vctr based operations work", {

	# Basic casting
	x1 <- term("x1", side = "right", role = "exposure", label = "Independent Variable")
	x2 <- term("x2", side = "right", role = "covariate", label = "Independent Variable")
	y <- "y"
	expect_type(c(x1, y), "character")
	expect_s3_class(c(x1, x2), "term")
	expect_type(vec_c(x1, y), "character")

})


test_that("terms can be generated from formulas", {

	# Simple formula for terms to be broken down
	ts <- term.formula(
		x = mpg + wt ~ hp + cyl + gear,
		group = list(cyl ~ "engine", gear ~ "engine"),
		label = list(mpg ~ "Mileage")
	)
	expect_length(ts, 5)

	# Complex formula with term and data operations
	f <- mpg + wt ~ X(hp) + M(cyl) + gear + drat + log(qsec)
	t <- term(
		x = f,
		group = list(drat + qsec ~ "spec"),
		label = list(mpg ~ "Mileage", wt ~ "Weight")
	)
	expect_length(t, 7)


	t1 <- term(f)
	t2 <- term(f, label = list(mpg ~ "Mileage"), group = list(qsec + drat ~ "speed"))
	expect_equal(vec_size(t1), 7)
	expect_equal(vec_size(t1), length(t1))
	expect_length(groups.term(t2), 2)

	# Adding roles and labels works
	tm <-
		term(f, label = list(gear ~ "Gears")) |>
		vec_data()
	expect_equal(tm$role[tm$term == "cyl"], "mediator")
	expect_equal(tm$label[tm$term == "gear"], "Gears")

})

test_that("terms can be made from a fitted model", {

	# lm models
	m_lm <- lm(mpg ~ wt + hp + cyl, mtcars)
	t_lm <- term(m_lm)
	expect_length(t_lm, 4)

	# glm
	m_glm <- glm(am ~ wt + hp, mtcars, family = "binomial")
	t_glm <- term(m_glm, label = list(am ~ "Automatic Transmission"))
	expect_length(t_glm, 3)
	expect_equal(labels(t_glm)$am, "Automatic Transmission")

	# Model spec of parsip models
	if (isTRUE(requireNamespace("parsnip", quietly = TRUE))) {

		m_parsnip <-
			parsnip::linear_reg() |>
			parsnip::set_engine("lm") |>
			parsnip::fit(mpg ~ ., data = mtcars)
		t_parsnip <- term(m_parsnip)
		expect_length(t_parsnip, 11)
	}

})
