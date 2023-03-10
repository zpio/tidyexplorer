% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotRaincloud.R
\name{plotRaincloud}
\alias{plotRaincloud}
\title{Plot a Raincloud Chart in ggplot2}
\usage{
plotRaincloud(
  .data,
  x,
  fill_var = NULL,
  interactive = FALSE,
  fill_ind = "#4e79a7",
  color_ind = "#4e79a7"
)
}
\arguments{
\item{.data}{A data.frame or tibble.}

\item{x}{A numeric variable.}

\item{fill_var}{(Optional) A categorical variable to group and fill the plot.}

\item{interactive}{A boolean value that specifies whether the plot should be returned static (ggplot2) or interactive (Plotly).}

\item{fill_ind}{A character string with a color code for the fill of the raincloud.}

\item{color_ind}{A character string with a color code for the color of the raincloud.}
}
\value{
A static \code{ggplot2} plot or an interactive \code{plotly} plot
}
\description{
This function plots a Density Chart for a numeric variable,
with the option to fill by a categorical variable.
}
\examples{

library(dplyr)

iris \%>\%
   plotRaincloud(x = Sepal.Width)

iris \%>\%
   plotRaincloud(x = Sepal.Width, fill_var = Species)

}
