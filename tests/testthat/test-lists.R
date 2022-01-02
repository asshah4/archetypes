test_that("simple list of formulas can be generated", {

	t1 <- term_rcrd(mpg ~ wt)
	t2 <- term_rcrd(mpg ~ hp)
	f1 <- formula_vctr(t1, pattern = "default")
	f2 <- formula_vctr(t2, pattern = "default")
	f <- c(f1, f2)

})
