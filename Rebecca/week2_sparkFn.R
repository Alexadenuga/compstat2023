
#
#  step 1: get lines of code that make a plot like I described
#  step 2: turn that into a basic function
#  my_spark <- function(dat) {
#    ... stuff
#  }
#  step 3: make the function better!
#  step 4: test the function with different arguments, make sure it works
# function(dat,ltr, bcol, x_lab, y_lab,title)

#' Title
#'
#' @param dat 
#' @param l_col_trans 
#' @param x_lab 
#' @param y_lab 
#' @param title 
#'
#' @return 
#' @export
#'
#' @examples

#'num_data =20
#'dat<-data.frame(col1=runif(num_data),col2=runif(num_data),
#'                col3=runif(num_data),col4=runif(num_data),
#'                col5=runif(num_data),col6=runif(num_data))
#'sparkplot(dat,l_col_trans = 0.3, 
#'          x_lab="xaxis", y_lab="dataframe column values",
#'          title ="Transparent Lines / Spark Plot")



sparkplot <- function(dat,l_col_trans, x_lab, y_lab, title){
  num_lines<- dim(dat)[2]-1 # Number of lines in the figure
  x_lim<- nrow(dat) # Number of dots for each line
  # plot the first column of data without showing
  plot(1:x_lim, dat[,1], type = "n",xlab =x_lab, ylab = y_lab,main = title)
  # plot the rest columns of data in transparent lines
  for (i in 1:num_lines){
    
    lines(1:x_lim, dat[, i+1],col = rgb(runif(1),runif(1),runif(1),alpha = l_col_trans)
,ylim =c(0,20), lwd = 2)  }
}

