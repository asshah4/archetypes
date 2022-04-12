test_that("term matching works correctly", {

	# Create list of formulas to test term matchign
	t <- tm(mpg + wt ~ hp + am + vs + cyl)
	f <- deparse1(stats::formula(t)) # Trace/pare
	fl <- fmls(t, order = 1) # Unit level paths
	tl <- tm()

	for (i in seq_along(fl)) {
		tl <- append(tl, match_terms(t, lhs(fl[i])))
	}

	expect_length(unique(tl), 2)
})
