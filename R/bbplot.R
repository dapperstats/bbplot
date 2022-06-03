#' @importFrom graphics axis axTicks mtext par plot points polygon rect
#' @importFrom grDevices col2rgb rgb
#' @importFrom stats density ecdf na.omit

#' @title Extending Base Plotting for bbs
#'
#' @description Providing additional flexibility and power to R graphics via the base R plotting functionality provided by the graphics and grDevices packages loaded by default with R. The functions are designed to specifically mimic the base plotting functions in the R API (e.g., have the same variable inputs named the same and in the same order) as well as in function-level syntax. It follows the painters model of R graphics, where output production occurs in steps that are placed with subsequent steps being layered on top of earlier steps. 
#'
#' @name bbplot
#'
#' @docType package
#'
#' @keywords package
#'
#' 
NULL