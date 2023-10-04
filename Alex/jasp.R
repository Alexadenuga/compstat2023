#' Just Another Spark-line plot function
#'
#' @param dat 
#' @param trans 
#' @param col 
#' @param label_x 
#' @param label_y 
#' @param the_title 
#'
#' @return
#' @export
#' Details: takes a dataframe and turns it into a sparkline plot where only the first line is not transparent to a specified opacity level
#' 
#' @examples dat <- data.frame(matrix(runif(50*3, 0, 1 ), 50))
#'           the_spark_plot(dat=dat, trans = 0.3, col = "red", label_x = "Data point", label_y = "Value", the_title = "Sparkline Plot")
#' 
the_spark_plot <- function(dat, trans, col, label_x, label_y, the_title) {
  # Check if 'dat' is a dataframe
  if (!is.data.frame(dat)) {
    stop("Input 'dat' must be a dataframe.")
  }
  
  plot(1, type = "n", xlim = c(1, nrow(dat)), ylim = c(0, max(dat)), xlab = label_x, ylab = label_y, xaxt = "n")
  
  # Plot the first column (X1) as a normal line
  lines(1:nrow(dat), dat[, 1], col = col)
  
  # Plot the remaining columns as transparent lines
  for (i in 2:ncol(dat)) {
    lines(1:nrow(dat), dat[, i], col = rgb(1, 0, 0, alpha = trans))
  }
  
  # Add a title
  title(the_title)
  
  # Add axis labels
  axis(1, at = 1:nrow(dat), labels = 1:nrow(dat))
  axis(2, at = seq(0, 1, by = 0.5))
  
  # Add a legend
  legend("topright", legend = colnames(dat), col = c(col, rep(rgb(1, 0, 0, alpha = 0.5), ncol(dat) - 1)), lty = 1)
}