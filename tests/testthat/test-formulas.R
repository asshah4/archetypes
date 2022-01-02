test_that("basic formula vector can be made and displayed", {

	# Construction
	f <- mpg + wt ~ X(hp) + X(cyl) + gear + drat + log(qsec)
	t <- term_rcrd(f)
	f1 <- formula_vctr(t, pattern = "default")

	expect_s3_class(f1, "formula_vctr")
	expect_equal(f1, formula_vctr(x = term_rcrd(f), pattern = "default"))
	expect_error(formula_vctr(f)) # Until formual implementation is made

	# Printing
	expect_output(print(f1), "mpg \\+ wt")
	expect_output(print(new_formula_vctr()), "[0]")
	if (isTRUE(requireNamespace("tibble", quietly = TRUE))) {
		tibble::tibble(f1) |>
			print() |>
			expect_output("<f_vctr>")
	}

})

test_that("vctrs casting and coercion work appropriately", {

	f <- mpg + wt ~ X(hp) + X(cyl) + gear + drat + log(qsec)
	t <- term_rcrd(f)
	f1 <- formula_vctr(x = t, pattern = "default")

	# Formula_vctrs
	f2 <- vec_cast(f1, formula_vctr())
	expect_equal(f1, f2)
	expect_error(vec_ptype2(f1, t))
	expect_output(print(vec_ptype2(f1, f2)), "formula_vctr")

	# Characters
	expect_type(as.character(f1), "character")
	expect_type(vec_ptype2(formula_vctr(), character()), "character")
	expect_type(vec_ptype2(character(), formula_vctr()), "character")

})

test_that("formula_vctr() inputs are acceptable", {

	t <- term_rcrd(mpg + wt ~ X(hp) + X(cyl) + gear + drat + log(qsec))
	expect_error(formula_vctr(x = t, pattern = "error"))

})

