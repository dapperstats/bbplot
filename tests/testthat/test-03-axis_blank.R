context("Test axis_blank")

test_that("axis_blank", {

  f <- function(side, at, minor, tck){
    blank(xlim = c(0,50), ylim = c(0,100))
    axis_blank(side=side,at=at,minor=minor,tck=tck)
  }
  expect_error(
    f(5, NULL, TRUE, -0.025)
  )
  expect_silent(
    f(1, NULL, TRUE, -0.025)
  )
  expect_silent(
    f(2, NULL, TRUE, -0.025)
  )
  expect_silent(
    f(1, seq(0,50,1), TRUE, -0.025)
  )
  expect_silent(
    f(2, seq(0,100,1), FALSE, -0.025)
  )
  expect_error(
    f("x", NULL, TRUE, -0.025)
  )
})