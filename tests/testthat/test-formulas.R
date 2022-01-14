test_that("basic formula vector can be made and displayed", {

	# Construction
	f <- mpg + wt ~ X(hp) + X(cyl) + gear + drat + log(qsec)
	t <- term_rx(f)
	f1 <- formula_rx(t, pattern = "direct")

	expect_s3_class(f1, "formula_rx")
	expect_equal(f1, formula_rx(x = term_rx(f), pattern = "direct"))
	expect_error(formula_rx(f)) # Until formal implementation is made

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

})


test_that("vctrs casting and coercion work appropriately", {

	f <- mpg + wt ~ X(hp) + X(cyl) + gear + drat + log(qsec)
	t <- term_rx(f)
	f1 <- formula_rx(x = t, pattern = "direct")

	# formula_rxs
	f2 <- vec_cast(f1, formula_rx())
	expect_equal(f1, f2)
	expect_error(vec_ptype2(f1, t))
	expect_output(print(vec_ptype2(f1, f2)), "formula_rx")

	# Characters
	expect_type(as.character(f1), "character")
	expect_type(vec_ptype2(formula_rx(), character()), "character")
	expect_type(vec_ptype2(character(), formula_rx()), "character")

})
