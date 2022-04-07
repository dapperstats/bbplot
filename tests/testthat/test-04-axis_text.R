context("Test axis_text")

test_that("axis_text", {
  
  f <- function(side, at, minor, tck, text, line){
    blank(xlim = c(0,50), ylim = c(0,100))
    axis_blank(side=side,at=at,minor=minor,tck=tck)
    axis_text(text = text, side = side, line = line, at = at)
  }
  expect_error(
    f(2, NULL, TRUE, -0.025, text = "yo")
  )
  expect_silent(
    f(1, NULL, TRUE, -0.025, text = "Hello", line = 2)
  )
  expect_silent(
    f(2, NULL, TRUE, -0.025, text = "World", line = 2)
  )
  expect_silent(
    f(2, NULL, FALSE, -0.025,text = NULL, line = 1)
  )
})