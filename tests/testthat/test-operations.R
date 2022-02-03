test_that("adding of groups works for operations", {

	t1 <- term_rx(mpg + wt ~ hp + drat + qsec + gear + cyl)
	groups <- list(gear ~ c(hardware), c(qsec, drat) ~ speed)
	grps <- formula_args_to_list(groups)
	t2 <- setGroups(t1, groups = grps)
	tm <- vec_data(t2)
	expect_length(tm$group[!is.na(tm$group)], 3)

	f1 <- formula_rx(t2)
	ops <- attr(f1, "operations")


})

test_that("operations work for special roles", {

	# Exposure expansion
	f <- mpg + wt ~ X(hp) + X(cyl) + gear + drat + log(qsec)
	t1 <- term_rx(f)
	f1 <- formula_rx(t1, pattern = "sequential")
	ops <- attr(f1, "operations")
	expect_equal(ops$expand_by_pattern, "sequential")

	# Simple mediation
	f1 <- frx(mpg ~ X(wt) + M(hp) + gear, pattern = "direct")
	ops <- attr(f1, "operations")
	expect_equal(ops$number_of_mediators, 1)
	lof <- list_of_formulas(f1)
	expect_length(lof, 3)
	expect_match(names(lof), "_dir")


})
