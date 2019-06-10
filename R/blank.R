
#' @title Create a plot with a blank space as the default
#'
#' @description A simple wrapper on \code{\link[graphics]{plot}} with all
#'   default values set to create a blank plot space. \cr \cr
#'   Specifically, \code{x = 1}, \code{y = 1}, \code{type = "n"}, 
#'   \code{xlab = ""}, \code{ylab = ""}, \code{xaxt = "n"}, \code{yaxt = "n"}, 
#'   and \code{bty = "n"}. Any of the input values can be overwritten within
#'   this function's call.
#'
#' @param x Set to \code{1}. See \code{\link[graphics]{plot}}.
#'
#' @param y Set to \code{1}. See \code{\link[graphics]{plot}}. 
#'
#' @param type Set to \code{"n"}. See \code{\link[graphics]{plot}}.
#'
#' @param xlab Set to \code{""}. See \code{\link[graphics]{plot}}.
#'
#' @param ylab Set to \code{""}. See \code{\link[graphics]{plot}}.
#'
#' @param xaxt Set to \code{"n"}. See \code{\link[graphics]{plot}}.
#'
#' @param yaxt Set to \code{"n"}. See \code{\link[graphics]{plot}}.
#'
#' @param bty Set to \code{"n"}. See \code{\link[graphics]{plot}}.
#'
#' @examples
#' \dontrun{
#'   blank()   
#' }
#'
#' @export
#'
blank <- function(x = 1, y = 1, type = "n", xlab = "", ylab = "", 
                  xaxt = "n", yaxt = "n", bty = "n", ...){
  plot(x = x, y = y, type = type, xlab = xlab, ylab = ylab, 
                  xaxt = xaxt, yaxt = yaxt, bty = bty, ...)
}
