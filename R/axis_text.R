#' @title Add labels to the axis labels
#'
#' @description A wrapper function for \code{\link[graphics]{mtext}}
#'   with two changes.
#'    
#'   1. The default for \code{text} and \code{at} is now \code{NULL}. If both are \code{NULL}, \code{\link[graphics]{axTicks}} will be used to determine what values to include on the x (\code{side = 1}) or y (\code{side = 2})  axis. 
#'   2. If \code{length(text) == 1} and \code{at = NULL}, then the appropriate \code{side} will find the central point of the axis to put the text (e.g., for an axis title).

#' @param text a character or expression vector specifying the text to be written. See \code{\link[graphics]{mtext}}.
#'
#' @param side an integer specifying which side of the plot the axis is to be drawn on. The axis is placed as follows: 1=below, 2=left, 3=above and 4=right.
#' 
#' @param line on which MARgin line, starting at 0 counting outwards.
#' 
#' @param outer use outer margins if available.
#'  
#' @param at The location of each string in user coordinates (i.e., the text). See \code{\link[graphics]{mtext}}.
#'
#' @param labels Set to \code{FALSE}. See \code{\link[graphics]{axis}}.
#' 
#' @param adj adjustment for each string in reading direction. For strings parallel to the axes, adj = 0 means left or bottom alignment, and adj = 1 means right or top alignment. See \code{\link[graphics]{mtext}}.
#' 
#' @param padj adjustment for each string perpendicular to the reading direction (which is controlled by \code{adj}). See \code{\link[graphics]{mtext}}.
#' 
#' @param cex character expansion factor. Can be a vector.
#' 
#' @param col color to use. Can be a vector. \code{NA} values (the default) means use \code{par("col")}.
#' 
#' @param font font for text. Can be a vector. \code{NA} values (the default) means use \code{par("font")}.


#' @param ... Other graphical parameters that may apply to \code{\link[graphics]{mtext}}.
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
#'    axis_text(side = 1)
#'    axis_blank(2, at = seq(0,100,20))
#'    axis_text(side = 2)
#'    
#' }
#'
#' @export
axis_text <- function(text = NULL, side = 3, line = 0, outer = FALSE, at = NULL,
                      adj = NA, padj = NA, cex = NA, col = NA, font = NA, ...){
  if(!is.numeric(side)){
    stop("side must be numeric. 1=below, 2=left, 3=above and 4=right.")
  }
  if(is.null(at) & is.null(text)){
    is_logged <- ifelse(side %in% c(1,3), par("xlog"), par("ylog"))
    
    at <- text <- axTicks(side = side, log = is_logged)
  }
  if(length(text == 1) & is.null(at)){
    if(side %% 2 == 1){
      at <- mean(par("usr")[1:2])
    } else {
      at <- mean(par("usr")[3:4])
    }
  }
  mtext(text = text, side = side, line = line, outer = outer,
        at = at, adj = adj, padj = padj, cex = cex, col = col, font = font,
        ...)
}
