#' Plotly Themes
#'
#' @param plot A Plotly chart object.
#' @param config.scrollZoom A logical value indicating whether to enable scroll-to-zoom functionality in the chart.
#' @param config.displayModeBar A logical value indicating whether to display the mode bar in the chart.
#' @param config.responsive A logical value indicating whether the chart should be responsive.
#' @param config.staticPlot A logical value indicating whether the chart should be static.
#' @param layout.dragmode A logical value indicating whether the chart should be draggable.
#' @param layout.autosize A logical value indicating whether the chart should automatically adjust its size.
#' @param layout.width The width of the chart in pixels.
#' @param font.color The font color for the chart.
#' @param font.family The font family for the chart.
#' @param layout.background The background color for the chart.
#' @param ... Additional arguments to be passed to the Plotly chart object.
#'
#' @return A plotly object with a dark theme or white theme
#'
#' @examples
#'
#'
#' library(dplyr)
#' library(plotly)
#'
#' iris %>%
#'   plot_ly() %>%
#'   add_trace(x = ~Sepal.Width, y = ~Sepal.Length, color = ~Species,
#'             type = "scatter", mode = "markers") %>%
#'   plotly_dark() %>%
#'   layout(
#'     title = list(text = "<b>Iris Sepal.Length vs Sepal.Width<b>"),
#'     legend=list(title=list(text='Species'))
#'   )

#'
#' @rdname plotly-themes
#' @export
plotly_dark <- function(plot,
                        config.scrollZoom = FALSE,
                        config.displayModeBar = FALSE,
                        config.responsive = TRUE,
                        config.staticPlot = FALSE,
                        layout.dragmode = TRUE,
                        layout.autosize = TRUE,
                        layout.width = NULL,
                        font.color = "#ffffff",
                        font.family = "Segoe UI",
                        layout.background = "#292c30",
                        ...) {
  plot %>%
    plotly::config(
      scrollZoom = config.scrollZoom,
      displayModeBar = config.displayModeBar,
      responsive = config.responsive,
      staticPlot = config.staticPlot
    ) %>%
    plotly::layout(
      dragmode = layout.dragmode,
      margin = list(l = 40, r = 10, b = 40, t = 60),
      autosize = layout.autosize,
      width = layout.width, #700
      font = list(color = font.color, family = font.family),
      title = list(x = 0.07, font = list(size=16)),
      xaxis = list(
        showline = FALSE,
        gridcolor = '#3b3b3b'
      ),
      yaxis = list(gridcolor = '#3b3b3b'),
      plot_bgcolor = layout.background,
      paper_bgcolor = layout.background,
      legend = list(
        orientation = "v", # h
        y = 0.5,           # -0.2
        # yanchor = "botom",
        # xanchor = "left"
        title = list(font = list(size=14))
      ),
      hoverlabel = list(
        font = list(
          color = "white",
          family = "Consolas"
        ),
        bordercolor = "transparent"
      )
    ) %>%
    plotly::layout(...)
}




#' @rdname plotly-themes
#' @export
plotly_white <- function(plot,
                         config.scrollZoom = FALSE,
                         config.displayModeBar = FALSE,
                         config.responsive = TRUE,
                         config.staticPlot = FALSE,
                         layout.dragmode = TRUE,
                         layout.autosize = TRUE,
                         layout.width = NULL,
                         font.color = "#1c1c1c" ,
                         font.family = "Segoe UI",
                         layout.background = "#ffffff",
                         ...) {
  plot %>%
    plotly::config(
      scrollZoom = config.scrollZoom,
      displayModeBar = config.displayModeBar,
      responsive = config.responsive,
      staticPlot = config.staticPlot
    ) %>%
    plotly::layout(
      margin = list(l = 40, r = 10, b = 40, t = 50),
      autosize = layout.autosize,
      width = layout.width, #700
      font = list(color = font.color, family = font.family),
      title = list(x = 0.07, font = list(size=15)),
      xaxis = list(gridcolor = '#e6e6e6'),
      yaxis = list(gridcolor = '#e6e6e6'),
      plot_bgcolor = layout.background,
      paper_bgcolor = layout.background,
      legend = list(
        orientation = "v", # h
        y = 0.5,           # -0.2
        # yanchor = "botom",
        # xanchor = "left"
        title = list(font = list(size=14))
      ),
      hoverlabel = list(
        font = list(
          color = "white",
          family = "Consolas"
        ),
        bordercolor = "transparent"
      )
    ) %>%
    plotly::layout(...)
}
