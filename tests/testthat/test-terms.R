# Class creation ----
test_that("term() makes term object or errors", {

	t1 <- term("y", sides = "lhs", roles = "outcome", labels = "Dependent Variable")
	t2 <- term("x", sides = "rhs", roles = "exposure", labels = "Independent Variable")
	t3 <- tx("m", sides = "rhs", roles = "mediator", labels = "Mediation Variable")
	expect_s3_class(t1, "term_vctr")
	expect_true(is_term(t1))
	expect_error(new_term("x"))
	expect_equal(length(t3), vec_size(t3))

	# Field size should be the same
	expect_error(term(c("x", "y")))

	# Expected class of input matters
	expect_error(term(as.name("x")))

})

test_that("formula can be broken into terms", {
	# Complex formula with term and data operations
	f <- mpg + wt ~ X(hp) + X(cyl) + gear + drat + log(qsec)
	t1 <- term(f)
	t2 <- term(f, labels = list(mpg = "Mileage"))
	expect(vec_size(t1), 7)
	expect_equal(vec_size(t1), length(t1))

})


# Output ----
test_that("formatting is correct", {

	t1 <- term("y", sides = "lhs", roles = "outcome", labels = "Dependent Variable")
	t2 <- term("x", sides = "rhs", roles = "exposure", labels = "Independent Variable")
	vt <- c(t1, t2)
	expect_output(print(t1), "y")
	expect_output(print(new_term()), "[0]")

	if (isTRUE(requireNamespace("tibble", quietly = TRUE))) {
		tibble::tibble(vt) |>
			print() |>
			expect_output("<tm>")
	}

})

# Coercion ----
test_that("coercion and casting works", {
	x1 <- term("x1", sides = "rhs", roles = "exposure", labels = "Independent Variable")
	x2 <- term("x2", sides = "rhs", roles = "covariate", labels = "Independent Variable")
	y <- "y"
	expect_type(c(x1, y), "character")
	expect_s3_class(c(x1, x2), "term_vctr")
	expect_type(vec_c(x1, y), "character")

})
