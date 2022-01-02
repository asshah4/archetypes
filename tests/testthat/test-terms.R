test_that("term() makes term object or errors", {

	t1 <- term_rcrd("y", sides = "lhs", roles = "outcome", labels = "Dependent Variable")
	t2 <- term_rcrd("x", sides = "rhs", roles = "exposure", labels = "Independent Variable")
	expect_s3_class(t1, "term_rcrd")
	expect_true(is_term(t1))
	expect_error(new_term("x"))
	expect_equal(length(t2), vec_size(t1))

	# Field size should be the same
	expect_error(term_rcrd(c("x", "y")))

	# Expected class of input matters
	expect_error(term_rcrd(as.name("x")))

})

test_that("formula can be broken into terms", {

	# Complex formula with term and data operations
	f <- mpg + wt ~ X(hp) + X(cyl) + gear + drat + log(qsec)
	t1 <- term_rcrd(f)
	t2 <- term_rcrd(f, labels = list(mpg = "Mileage"))
	expect_equal(vec_size(t1), 7)
	expect_equal(vec_size(t1), length(t1))

	# Adding roles and labels works
	t3 <- term_rcrd(f, roles = list(gear = "M"), labels = list(gear = "Gears")) |>
		vec_data()
	expect_equal(t3$roles[t3$terms == "gear"], "M")
	expect_equal(t3$labels[t3$terms == "gear"], "Gears")

})


test_that("formatting is correct", {

	t1 <- term_rcrd("y", sides = "left", roles = "outcome", labels = "Dependent Variable")
	t2 <- term_rcrd("x", sides = "right", roles = "exposure", labels = "Independent Variable")
	vt <- c(t1, t2)
	expect_output(print(t1), "y")
	expect_output(print(new_term()), "[0]")

	if (isTRUE(requireNamespace("tibble", quietly = TRUE))) {
		tibble::tibble(vt) |>
			print() |>
			expect_output("<t_rcrd>")
	}

})

test_that("coercion and casting works", {
	x1 <- term_rcrd("x1", sides = "right", roles = "exposure", labels = "Independent Variable")
	x2 <- term_rcrd("x2", sides = "right", roles = "covariate", labels = "Independent Variable")
	y <- "y"
	expect_type(c(x1, y), "character")
	expect_s3_class(c(x1, x2), "term_rcrd")
	expect_type(vec_c(x1, y), "character")

})
