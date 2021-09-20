test_that("deparsing of formulas works", {

	f1 <- mpg ~ hp
	f2 <- mpg ~ X(hp) + cyl
	f3 <- mpg + wt ~ X(hp) + X(cyl) + gear + drat + qsec
	f4 <- mpg + wt ~ X(hp) + X(cyl) + gear + drat + F(qsec)
	set_rx_labels(list(X = "exposure", "F" = "fixed"))
	labs1 <- deparse_formula(f1)
	labs4 <- deparse_formula(f4)
	expect_true(is.named(labs1))
	expect_true(is.named(labs4))
	reset_rx_labels()


})

test_that("basic rx() works appropriately", {

	# Rx should not work for certain classes
	expect_error(rx("test"))

	# Ensure roles are possible
	f <- mpg + wt ~ X(hp) + X(cyl) + gear + drat + qsec
	set_rx_labels(list(X = "exposure"))
	expect_error(validate_class(f, "rx"))

	x <- rx(f)
	expect_s3_class(x, c("rx", "formula"))
	expect_error(validate_roles(x, roles = list(opts = "sunroof")))
	expect_equal(sum(attributes(x)$roles$exposure), 2)
	reset_rx_labels()

})
