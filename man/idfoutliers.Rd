% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/idfoutliers.R
\name{idfoutliers}
\alias{idfoutliers}
\title{idfoutliers}
\usage{
idfoutliers(x, i = 3, min = 0.25, max = 0.75, na.rm = TRUE)
}
\arguments{
\item{x}{Input the data filed(should be a list and the data format must be numeric)}

\item{i}{Input the multiple of IQR(default is 3)}

\item{min}{probabilities of values between 0 and 1(default is 0.25)}

\item{max}{probabilities of values between 0 and 1(default is 0.75)}

\item{na.rm}{removes the NA values (default value is TRUE)}
}
\description{
This function help you to find out the Outliers
}
\examples{
dt <- data.frame(x=c(1,1,1,1,1,1,1,1,1,1,10))
idfoutliers(dt$x)
}
