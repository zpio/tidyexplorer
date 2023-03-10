% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_BoxPlot_Outlier.R
\name{data_BoxPlot_Outlier}
\alias{data_BoxPlot_Outlier}
\title{Generate statistics for the boxplot with and without outliers}
\usage{
data_BoxPlot_Outlier(.data)
}
\arguments{
\item{.data}{A data frame or tibble with at least one numeric column.}
}
\value{
A data frame with statistics for the boxplot with and without outliers.
}
\description{
This function generates statistics for the boxplot with and without outliers for a numeric column of a data frame.
}
\examples{
library(dplyr)

iris \%>\% select(Sepal.Width) \%>\%
  data_BoxPlot_Outlier()

}
