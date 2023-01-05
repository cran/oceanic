#' @title idfoutliers
#' @description This function help you to find out the Outliers
#' @param x Input the data filed(should be a list and the data format must be numeric)
#' @param i Input the multiple of IQR(default is 3)
#' @param min probabilities of values between 0 and 1(default is 0.25)
#' @param max probabilities of values between 0 and 1(default is 0.75)
#' @param na.rm removes the NA values (default value is TRUE)
#' @export
#' @import stats
#' @examples
#' dt <- data.frame(x=c(1,1,1,1,1,1,1,1,1,1,10))
#' idfoutliers(dt$x)

idfoutliers <- function(x, i=3 , min=0.25, max=0.75, na.rm = TRUE) {
			qnt <- quantile(x, probs=c(min, max), na.rm = na.rm)
			H <- i * IQR(x, na.rm = na.rm)
			y <- x
			y[x < (qnt[1] - H)] <- NA
			y[x > (qnt[2] + H)] <- NA
			return(y)
 }
