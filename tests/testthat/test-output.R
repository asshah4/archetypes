test_that("printing works", {

	# Color printing
	f1 <- mpg + wt ~ cyl + am
	x <- capture.output(print(rx(f1)))
	expect_type(x, "character")

	# Themed color printing
	f2 <- mpg + cyl ~ X(wt) + hp + F(vs)
	set_rx_theme("murmur")
	y <- capture.output(print(rx(f2)))
	expect_type(y, "character")
	reset_rx_roles()

	# Should print without color
	f3 <- mpg + cyl ~ X(wt) + Y(hp) + Z(vs)
	rls <- list(lhs = "left", X = "ex", Y = "why", Z = "zed")
	set_rx_roles(rls)
	expect_equal(getOption("rx.lhs"), "left")
	reset_rx_roles()

})
