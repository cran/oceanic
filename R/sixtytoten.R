#' @title sixtytoten
#' @description This function allows you to transfer the coordinate system from sexagesimal to decimal
#' @param num Input a value of longitude or latitude.
#' @export
#' @import sf
#' @import spData
#' @examples
#' sixtytoten(121.49)
sixtytoten <- function(num){
num1 <- trunc(num)
num2 <- round((num-trunc(num))/0.6,2)
new <- num1+num2
return(new)
}
