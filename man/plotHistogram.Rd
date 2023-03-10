% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotHistogram.R
\name{plotHistogram}
\alias{plotHistogram}
\title{Plot Histograms in ggplot2}
\usage{
plotHistogram(
  .data,
  fill_var = NULL,
  interactive = FALSE,
  facet_grid = FALSE,
  ncol_facet = 3,
  bins = 30,
  alpha = 1,
  scales = "free"
)
}
\arguments{
\item{.data}{A data.frame or tibble.}

\item{fill_var}{(Optional) A categorical variable to group and fill the plot.}

\item{interactive}{A boolean value that specifies whether the plot should be returned static (ggplot2) or interactive (Plotly).}

\item{facet_grid}{A boolean value that specifies whether the chart should be displayed in facet_grid when there are only two numeric variables and one categorical variable specified in fill_var.}

\item{ncol_facet}{An integer value indicating the number of columns for facet_grid. Defaults to 3.}

\item{bins}{An integer value indicating the number of bins for the histogram. Defaults to 30.}

\item{alpha}{A numeric value indicating the alpha (transparency) for the histogram bars. Defaults to 1.}

\item{scales}{A string value indicating how the axes scales should be computed. Defaults to "free".}
}
\value{
A static \code{ggplot2} plot or an interactive \code{plotly} plot
}
\description{
This function plots a Density Chart for all numeric variables,
with the option to fill by a categorical variable.
}
\examples{

library(dplyr)

mtcars <- mtcars \%>\% as_tibble() \%>\%
   mutate(across(c(am, carb, cyl, gear, vs), as.factor))

iris \%>\%
   plotHistogram()

mtcars \%>\%
   plotHistogram()

iris \%>\%
   plotHistogram(fill_var = Species, alpha = 0.7)

iris \%>\% select(Sepal.Length, Species) \%>\%
   plotHistogram(fill_var = Species, facet_grid = TRUE)

}
