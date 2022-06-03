#' @title Add an axis to a plot without labels as the default
#'
#' @description A wrapper function for \code{\link[graphics]{axis}}
#'   with three changes.
#'    
#'   1. \code{labels = FALSE} is now the default.
#'   2. \code{tck} has been added as an argument, which is used to specify the size of the tick mark.
#'   3. \code{minor} has been added as an argument. Used to add minor (i.e., smaller) tickmarks equally between the first axis.

#'
#' @param side an integer specifying which side of the plot the axis is to be drawn on. The axis is placed as follows: 1=below, 2=left, 3=above and 4=right.
#' 
#' @param at The points at which tick-marks are to be drawn. Non-finite (infinite, NaN or NA) values are omitted. By default (when NULL) tickmark locations are computed using \code{\link[graphics]{axTicks}}.
#'
#' @param labels Set to \code{FALSE}. See \code{\link[graphics]{axis}}.
#'
#' @param tick See \code{\link[graphics]{axis}}.
#'
#' @param line See \code{\link[graphics]{axis}}.
#'
#' @param pos  See \code{\link[graphics]{axis}}.
#'
#' @param outer See \code{\link[graphics]{axis}}.
#' 
#' @param font See \code{\link[graphics]{axis}}.
#' 
#' @param lty See \code{\link[graphics]{axis}}.
#' 
#' @param lwd See \code{\link[graphics]{axis}}.
#' 
#' @param lwd.ticks See \code{\link[graphics]{axis}}.
#' 
#' @param col See \code{\link[graphics]{axis}}.
#' 
#' @param col.ticks See \code{\link[graphics]{axis}}.
#' 
#' @param hadj See \code{\link[graphics]{axis}}.
#' 
#' @param padj See \code{\link[graphics]{axis}}.
#' 
#' @param gap.axis See \code{\link[graphics]{axis}}.
#' 
#' @param tck The length of tick marks as a fraction of the smaller of the width or height of the plotting region. If tck >= 0.5 it is interpreted as a fraction of the relevant side, so if tck = 1 grid lines are drawn. The default setting is (tck = -0.025).
#' 
#' @param minor Whether or not to add smaller tick marks spaced equally between the larger tickmarks specified by the at argument. Tick length is set to \code{tck * 0.5}. 
#' 
#' @param ... Other graphical parameters that may apply to \code{\link[graphics]{axis}}.
#' 
#'
#'
#' @examples
#' \dontrun{
#'    blank(
#'      xlim = c(0,50),
#'      ylim = c(0,100),
#'      bty ="l"
#'    )
#'    
#'    axis_blank(1, at = seq(0,50,10))
#'    axis_blank(2, at = seq(0,100,20))
#'    
#' }
#'
#' @export
axis_blank <- function(side, at = NULL, labels = FALSE, tick = TRUE, line = NA,
                       pos = NA, outer = FALSE, font = NA, lty = "solid",
                       lwd = 1, lwd.ticks = lwd, col = NULL, col.ticks = NULL,
                       hadj = NA, padj = NA, gap.axis = NA, ..., tck = -0.02, minor = TRUE){
  if(!is.numeric(side)){
    stop("side must be numeric. 1=below, 2=left, 3=above and 4=right.")
  }
  if(is.null(at)){
    is_logged <- ifelse(side %in% c(1,3), par("xlog"), par("ylog"))
    
    at <- axTicks(side = side, log = is_logged)
  }
  axis(side = side, at = at, labels = labels, tick = tick, line = line, pos = pos,
       outer = outer, font = font, lty = lty, lwd = lwd, lwd.ticks = lwd, col = col,
       col.ticks = col.ticks, hadj = hadj, padj = padj, gap.axis = gap.axis,
       tck = tck, ...)
  if(minor){
    smaller_seq <- seq(at[1], max(at), (at[2] - at[1])/2)
    axis(side = side, at = smaller_seq, labels = labels, tick = tick, line = line, pos = pos,
         outer = outer, font = font, lty = lty, lwd = lwd, lwd.ticks = lwd, col = col,
         col.ticks = col.ticks, hadj = hadj, padj = padj, gap.axis = gap.axis,
         tck = tck/2, ...)
    
  }
}
