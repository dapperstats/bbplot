context("Test ribbon")

test_that("ribbon", {
  tmp <- data.frame(
    x = 1:10,
    lo = qnorm(0.025,1:10, 3),
    hi = qnorm(0.975, 1:10, 3)
  )
  f <- function(x,y,col,alpha){
    blank(xlim = range(tmp$x),ylim=range(unlist(tmp)))
    ribbon(x=x, y=y, col=col,alpha=alpha)
  }
  expect_silent(
    f(tmp$x,tmp[c("lo","hi")], "blue", 0.5)
  )
  expect_silent(
    f(
      c(tmp$x, rev(tmp$x)),
      c(tmp$lo, rev(tmp$hi)),
      "blue",
      0.5
    )
  )
  expect_silent(
    f(tmp$x,tmp[c("lo","hi")], "#0000FF99", alpha = NULL)
  )
  expect_error(
    f(tmp$x[1:5],tmp[c("lo","hi")], "blue", alpha = NULL)
  )
  expect_warning(
    f(tmp$x,tmp[c("lo","hi")], "#0000FF99", alpha = 0.5)
  )
  expect_warning(
    f(tmp$x,tmp[c("lo","hi")], c("blue", "green"), 0.5)
  )
})