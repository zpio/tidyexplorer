#' Plotly Themes
#'
#' @param plot A plotly object
#' @param config.scrollZoom logical. FALSE default
#' @param config.displayModeBar logical. FALSE default
#' @param config.responsive logical. TRUE default
#' @param config.staticPlot logical. FALSE default
#' @param layout.dragmode logical. TRUE default
#' @param layout.autosize logical. TRUE default
#' @param layout.width A number. NULL default
#' @param font.color A string. "#ffffff" default
#' @param font.family A string. "Segoe UI" default
#' @param layout.background A string. "#292c30" default
#' @param ... Parameters passed to `plotly` functions.
#'
#' @return A plotly object
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
