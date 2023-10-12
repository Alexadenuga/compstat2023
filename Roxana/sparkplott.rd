#' Spark-line plot function
#'
#' This function creates a sparkline plot where only the first line is not transparent, 
#' with a specified opacity level.
#'
#' @param dat A dataframe containing the data to be plotted.
#' @param trans The opacity level for the transparent lines.
#' @param col The color for the main (non-transparent) line.
#' @param label_x The label for the x-axis.
#' @param label_y The label for the y-axis.
#' @param the_title The title of the plot.
#' @param show_legend Logical, whether to display the legend (default is TRUE).
#'
#' @return None
#'
#' @details
#' The function takes a dataframe and generates a sparkline plot. The first line is 
#' drawn with full opacity (non-transparent), and the remaining lines are drawn with 
#' the specified level of transparency (trans). The plot includes labels for the axes 
#' and a title.
#'
#' @examples
#' # Create a sample dataframe 'dat'
#' dat <- data.frame(x = 1:100,
#'                   y = rnorm(100),
#'                   y2 = rnorm(100) + 2,
#'                   y3 = rnorm(100) - 2)
#'
#' # Call the 'the_spark_plot' function to create a sparkline plot without legend
#' the_spark_plot(dat = dat, trans = 0.3, col = "pink", label_x = "Data point", 
#'                label_y = "Value", the_title = "Spark Plot", show_legend = FALSE)
#'
#' @export
#'
the_spark_plot <- function(dat, trans, col, label_x, label_y, the_title, show_legend = TRUE) {
  # Check if 'dat' is a dataframe
  if (!is.data.frame(dat)) {
    stop("Input 'dat' must be a dataframe.")  }
  
  # check transparency 
  if(class(trans) != numeric){
    stop("Transparency level must be a number between 0 and 1")
  }
  
  if(class(c(xaxis, yaxis, title)) != character){
    stop("Rename the axis and title")
  }
  
  
  # Create an empty plot with appropriate labels and axis settings
  plot(1, type = "n", xlim = c(1, nrow(dat)), ylim = c(0, max(dat)),
       xlab = label_x, ylab = label_y, xaxt = "n")
  
  # Plot the first column (X1) as a normal line with the specified color
  lines(1:nrow(dat), dat[, 1], col = col)
  
  # Plot the remaining columns as transparent lines with the specified opacity
  for (i in 2:ncol(dat)) {
    lines(1:nrow(dat), dat[, i], col = rgb(1, 0, 0, alpha = trans))
  }
  
  # Add a title to the plot
  title(the_title)
  
  # Add axis labels and ticks
  axis(1, at = 1:nrow(dat), labels = 1:nrow(dat))
  
  # Add a legend to distinguish columns if show_legend is TRUE
  if (show_legend) {
    legend("topright", legend = colnames(dat),
           col = c(col, rep(rgb(1, 0, 0, alpha = 0.5), ncol(dat) - 1)), lty = 1)
  }
}