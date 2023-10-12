#' Sparkline
#'
#' @param dat the data you want to create a sparkline for
#' @param trans transparency level
#' @param xlab label for the x-axis 
#' @param ylab label for the y-axis
#' @param title title of the plot
#'
#' @return
#' @export
#'
#' Details: This function creates a sparkplot from a data.frame
#' @examples sparkplot(mtcars, 0.4, "x", "y", "sparkplot")

sparkplot <- function(dat, trans, xlab, ylab, title){
  
  # double check the function inputs
  
  if(class(dat) != data.frame){
    stop("This object is NOT a data.frame")
  }
  
  if(class(trans) != numeric){
    stop("Transparency level must be a number between 0 and 1")
  }
  
  if(class(c(xlab, ylab, title)) != character){
    stop("Rename the axis and title")
  }
  
  
  # plot the first column 
  plot(x = 1:nrow(dat), y = dat[,1], type = "l", xlab = xlab, ylab = ylab, main = title)
  
  for(i in 2:ncol(dat)){
    
    # plot columns as transparent lines
    lines(x = 1:nrow(dat), y = dat[,i], col = rgb(0,0,0,alpha = trans))
  }
}
