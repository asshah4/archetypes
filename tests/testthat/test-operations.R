test_that("adding of groups works for operations", {

	t1 <- term_rcrd(mpg + wt ~ hp + drat + qsec + gear + cyl)
	groups <- list(gear ~ c(hardware), c(qsec, drat) ~ speed)
	grps <- formula_args_to_list(groups)
	t2 <- setGroups(t1, groups = grps)
	tm <- vec_data(t2)
	expect_length(tm$group[!is.na(tm$group)], 3)

	f1 <- formula_vctr(t2)


})
