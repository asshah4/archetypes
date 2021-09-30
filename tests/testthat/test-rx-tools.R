test_that("deparsing a formula returns a label list", {

	f <- mpg + wt ~ hp + cyl + gear
	flist <- deparse_formula(f)
	expect_named(flist, expected = c("lhs", "rhs"), ignore.order = TRUE)

})


test_that("arithmetic works", {

	f1 <- mpg + wt ~ hp + cyl + gear
	f2 <- wt ~ hp + cyl

	x <- rx(f1)
	y <- f2

	# Merge
	merge_z <- add(x, y)
	add_z <- x + y
	expect_s3_class(merge_z, "rx")
	expect_equal(merge_z, add_z, ignore_attr = TRUE)

	# Split
	split_x <- subtract(x, y)
	split_z <- add_z - y
	expect_s3_class(split_x, "rx")
	expect_equal(split_x, split_z, ignore_attr = TRUE)
	expect_error({
		x <- rx(mpg ~ hp)
		y <- rx(wt ~ hp)
		x - y
	})
	expect_error(x/y)

	# Divide



})

