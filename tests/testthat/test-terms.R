test_that("term_rx() makes term object or errors", {

	t1 <- term_rx("y", side = "left", role = "outcome", label = "Dependent Variable")
	t2 <- trx("x", side = "right", role = "exposure", label = "Independent Variable")
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

test_that("formula can be broken into terms", {

	# Complex formula with term and data operations
	f <- mpg + wt ~ X(hp) + M(cyl) + gear + drat + log(qsec)
	t1 <- term_rx(f)
	t2 <- term_rx(f, labels = list(mpg ~ "Mileage"), groups = list(qsec + drat ~ "speed"))
	expect_equal(vec_size(t1), 7)
	expect_equal(vec_size(t1), length(t1))
	expect_length(getComponent(t2, "group"), 2)

	# Adding roles and labels works
	tm <-
		term_rx(f, labels = list(gear ~ "Gears")) |>
		vec_data()
	expect_equal(tm$role[tm$term == "cyl"], "mediator")
	expect_equal(tm$label[tm$term == "gear"], "Gears")

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

