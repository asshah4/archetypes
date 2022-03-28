test_that("special formuals can be initialized", {

	f <- mpg ~ wt + hp
	x <- formula_archetype(f)
	expect_s3_class(x, "formula_archetype")


})
