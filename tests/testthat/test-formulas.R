test_that("new formula frames can be made", {
	f <- mpg + wt ~ X(hp) + X(cyl) + gear + drat + log(qsec)
	t <- term(f)
	ff <- formula_frame(x = t, pattern = "default")


})
