test_that("custom formuals can be initialized", {

	# Formula
	f <- mpg ~ wt + hp
	x <- formula_archetype(f)
	expect_s3_class(x, "formula_archetype")

	# Terms
	f <- mpg + wt ~ X(hp) + X(cyl) + gear + drat + log(qsec)
	t <- term_archetype(f)
	x <- formula_archetype(t)
	expect_s3_class(x, "formula_archetype")


})
