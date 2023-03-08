#' Bubble Chart
#'
#' Plot Bubble Chart in ggplot2
#'
#' @param data A data frame.
#' @param x A numeric variable.
#' @param y A numeric variable.
#' @param size A numeric variable indicating the size of the bubbles.
#' @param fill A variable that controls the fill color of the bubbles.
#' @param scale_size_range A numeric vector with the minimum and maximum size of the bubbles.
#' @param alpha_point A numeric value indicating the transparency of the bubbles.
#' @param color_point A character string indicating the color of the points.
#' @param stroke_point A numeric value indicating the width of the stroke of the points.
#' @param legend.position A character string indicating the position of the legend in the plot.
#' @param title_plot A character string indicating the title of the plot.
#' @param interactive A boolean value that specifies whether the plot should be returned static (ggplot2) or interactive (Plotly).
#'
#' @return A static `ggplot2` plot or an interactive `plotly` plot
#'
#' @examples
#' library(dplyr)
#' library(gapminder)
#'
#' data <- gapminder %>% filter(year=="2002") %>%
#'   dplyr::select(-year)
#'
#' data_bubble <- data %>%
#'   mutate(pop=pop/1000000) %>%
#'   arrange(desc(pop)) %>%
#'   mutate(country = factor(country, country)) %>%
#'   mutate(
#'     annotation = case_when(
#'       gdpPercap > 5000 & lifeExp < 60 ~ "yes",
#'       lifeExp < 30 ~ "yes",
#'       gdpPercap > 40000 ~ "yes"
#'     )
#'   )
#'
#'
#' bubble_chart(
#'   data = data_bubble,
#'   x = gdpPercap,
#'   y = lifeExp,
#'   size = pop,
#'   fill = continent
#' )
#'
#' bubble_chart(
#'   data = data_bubble,
#'   x = gdpPercap,
#'   y = lifeExp,
#'   size = pop,
#'   fill = continent
#' )+
#'   ggrepel::geom_text_repel(
#'     data = data_bubble %>% filter(annotation=="yes"),
#'     ggplot2::aes(label = country),
#'     size = 3
#'   )
#'
#' @export
bubble_chart <- function(data, x, y,
                         size,
                         fill,
                         scale_size_range = c(0.5, 20),
                         alpha_point = 0.8,
                         color_point = "black",
                         stroke_point = 0.2,
                         legend.position = "right",
                         title_plot = "",
                         interactive=FALSE){

  x  <- rlang::enquo(x)
  y  <- rlang::enquo(y)
  size  <- rlang::enquo(size)
  fill  <- rlang::enquo(fill)

  p <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = !!x, y= !!y, size= !!size, fill = !!fill)) +
    ggplot2::geom_point(alpha = alpha_point, shape = 21, color = color_point, stroke = stroke_point) +
    ggplot2::scale_size(range = scale_size_range, guide = "none")+
    ggplot2::labs(title = title_plot)+
    ggthemes::scale_fill_tableau()+
    ggplot2::theme_minimal()+
    ggplot2::theme(
      text = ggplot2::element_text(size = 9, color = "gray30", family = "sans"),
      axis.text = ggplot2::element_text(size = 8, color = "gray30"),
      axis.title = ggplot2::element_text(size = 11, hjust = 1, color = "gray30"),
      plot.title = ggplot2::element_text(
        size = 12, face="bold", hjust = 0.5, color = "grey30",
        margin = ggplot2::margin(b=10)
      ),
      legend.position = legend.position,
      legend.text = ggplot2::element_text(size = 9, color = "gray30"),
      legend.title = ggplot2::element_text(size = 11, color = "gray30"),
      plot.background = ggplot2::element_rect(fill = "white", color = "white"),
      panel.background = ggplot2::element_rect(fill = "white", color = "white"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "gray91")

    )+
    ggplot2::scale_x_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale()))+
    ggplot2::scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale()))


  if(!interactive){
    p1 <- p +
      ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(size = 3)))

    return(p1)

  } else {
    pt <- plotly::ggplotly(p) %>%
      plotly::layout(
        showlegend = TRUE,
        title = list(size = 11),
        font = list(family = "Segoe UI"),
        margin = list(t = 60, b=50),
        hoverlabel = list(
          font = list(
            color = "black",
            family = "Consolas"
          ),
          bordercolor = "transparent"
        )
      )

    return(pt)
  }

}
