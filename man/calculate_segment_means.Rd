% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MarketingAnalytics_functions.R
\name{calculate_segment_means}
\alias{calculate_segment_means}
\title{Calculate Segment Means}
\usage{
calculate_segment_means(df)
}
\arguments{
\item{df}{The data frame of segmentation variables that also inclueds a segment column for segment membership}
}
\description{
This function calculates segment means for each segmentation variable
based on the segment memberships uncovered in the cluster analysis.
}
\examples{
mydf <- data.frame(age=c(23,45,64,56,55,32,23,45,31,60), wealth=c(1,2,3,3,3,2,1,2,2,3),segment=c(1,2,2,2,2,1,1,2,1,2))
calculate_segment_means(mydf)

}
\keyword{averages}
\keyword{calculate}
