#' @title Add a polygon to a plot
#'
#' @description A slight modification to \code{\link[graphics]{polygon}} 
#'   with quality of life improvements to make it easier to add colored confidence
#'   intervals to a plot. Default values are taken for all 
#'   \code{\link[graphics]{polygon}} arguments, except for \code{border}, which 
#'   is set to \code{NA}. Additionally, an \code{alpha} argument has been added to
#'   accommodate partially transparent confidence intervals. Any of the input values
#'   can be overwritten within this function's call.
#'
#' @param x vector containing the x-axis coordinates of the polygon vertices. See details for more information..
#'
#' @param y Either a vector containing the y-axis coordinates of the polygon vertices, or a two column data.frame / matrix with the vertices. See details for more information. 
#'
#' @param density Set to \code{NULL}. See \code{\link[graphics]{polygon}}.
#'
#' @param angle Set to \code{45}. See \code{\link[graphics]{polygon}}.
#'
#' @param col Set to \code{NA}. See \code{\link[graphics]{polygon}}.
#'
#' @param border Set to \code{NA}. See \code{\link[graphics]{polygon}}.
#'
#' @param lty Set to \code{par("lty")}. See \code{\link[graphics]{polygon}}.
#' 
#' @param ... Additional arguments such as \code{xpd}, \code{lend}, \code{ljoin}, and \code{lmitre} can be given as arugments.
#'
#' @param fillOddEven Set to \code{FALSE}. See \code{\link[graphics]{polygon}}.
#' 
#' @param alpha new alpha level in [0,1]. If \code{col} is a HEX color that already includes
#' an alpha channel, the \code{alpha} argument will be ignored.
#'
#' @details 
#' 
#' If \code{y} is a two column data.frame or matrix, \code{ribbon} will convert \code{y} to a vector
#' such that \code{y = c(y[,1], rev(y[,2]))} in order to create the lower and upper bounds of
#' the polygon. Additionally, when \code{y} is a two column data.frame or matrix, \code{x} can have the same
#' length as the number of rows in \code{y}, and \code{ribbon} will concatenate the reverse of the
#' vector \code{x} to ensure it has equal length.
#'
#' @examples
#' \dontrun{
#'    # Load data
#'    data(cars)
#'    # fit model
#'    m1 <- lm(
#'      dist ~ speed,
#'      data = cars
#'    )
#'    # make predictions
#'    preds <- predict(
#'      m1, 
#'      newdata = data.frame(speed = 10:25),
#'      interval = "confidence"
#'    )
#'    # base plot
#'    blank(
#'      xlim = c(10,25),
#'      ylim = c(15,120),
#'      xlab = "Speed",
#'      ylab = "Stopping distance",
#'      xaxt = "s",
#'      yaxt = "s",
#'      bty = "l",
#'      las = 1
#'    )
#'    # add 95% confidence interval
#'    ribbon(
#'      x=10:25,
#'      y=preds[,c("lwr","upr")],
#'      col = "purple",
#'      alpha = 0.5
#'    )
#'    # add mean prediction
#'    lines(
#'      x=10:25,
#'      y = preds[,"fit"],
#'      lwd =2,
#'      col = "purple"
#'    )
#'    # add data
#'    points(
#'      x = cars$speed,
#'      y = cars$dist,
#'      pch = 16
#'    )
#' }
#'
#' @export
ribbon <- function(x, y, density=NULL, angle=45, border=NA,
                   col=NA, lty= par("lty"),...,fillOddEven=FALSE,
                   alpha = NULL
  ){
  # error checks
  if(!any(is.na(col)) & length(col)>1){
    warning("Two values input to col. Only first element used.")
    col <- col[1]
  }
  # check if y is a matrix
  if(is.matrix(y)|is.data.frame(y)){
    y <- c(y[,1], rev(y[,2]))
    # check if x is half the length of y
    if(length(y)/length(x) == 2){
      x <- c(x, rev(x))
    }
  }
  # evaluate color and alpha channel
  if( is.na(col) ){
    my_col <- NA
  } else { # otherwise go through color process
    # 
    if(
      length(grep("^#", col)) == 1 & # if start with hash
      nchar(col)>7 # & alpha channel is present
      ){
        if(!is.null(alpha)){
          warning("col already has alpha channel, ignoring alpha argument.")
          my_col <- col
        } else {
          my_col <- col
        }
      } else {
      # get rgb
      my_rgbs <- col2rgb(col)
      # set color
      my_col <- rgb(
        my_rgbs[1],my_rgbs[2],my_rgbs[3],max = 255,alpha = 255 * alpha
      )
    } 
  }
  polygon(
    x = x, y = y, density = density, angle = angle,
    border = border, col = my_col, lty = lty,
    fillOddEven = fillOddEven, ...
  )
}


