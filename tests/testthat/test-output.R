test_that("printing works", {

	# Color printing
	f1 <- mpg + wt ~ cyl + am
	x <- rx(f1)
	expect_output(print.rx(x), regexp = "\033")

	# Themed color printing
	f2 <- mpg + cyl ~ X(wt) + hp + F(vs)
	set_rx_theme("murmur")
	y <- rx(f2)
	expect_output(print.rx(y), regexp = "\033")
	reset_rx_roles()

	# Should print without color
	f3 <- mpg + cyl ~ X(wt) + Y(hp) + Z(vs)
	rls <- list(lhs = "left", X = "ex", Y = "why", Z = "zed")
	set_rx_roles(rls)
	expect_equal(getOption("rx.lhs"), "left")
	z <- rx(f3)
	expect_output(print.rx(z), "mpg \\+ cyl ~ wt \\+ hp \\+ vs")
	reset_rx_roles()

})
