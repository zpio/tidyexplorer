#' Plot Normality of Numeric Variables
#'
#' This function generates a grid of plots for each numeric variable in the input data-frame, showing their distribution, Q-Q plot, and the distribution after applying the sqrt or log transformation.
#'
#' @param .data The input data-frame or tibble.
#' @param interactive A boolean value that specifies whether the plot should be returned static (ggplot2) or interactive (Plotly).
#' @param color_density A string indicating the color to be used for the density plots. Default is "steelblue".
#' @param color_qq A string indicating the color to be used for the Q-Q plots. Default is "steelblue".
#' @param color_qq_line A string indicating the color to be used for the line in the Q-Q plots. Default is "steelblue4".
#' @param color_sqrt A string indicating the color to be used for the density plots of the sqrt-transformed variables. Default is "darkorange".
#' @param color_log A string indicating the color to be used for the density plots of the log-transformed variables. Default is "salmon".
#'
#' @return A static `ggplot2` plot or an interactive `plotly` plot
#'
#' @examples
#'
#' library(dplyr)
#'
#' iris %>%
#'    select(Petal.Width) %>%
#'    plotNormality()
#'
#' iris %>%
#'    select(Petal.Width, Sepal.Length) %>%
#'    plotNormality()
#'
#' @export
plotNormality <- function(.data, interactive = FALSE,
                          color_density = "steelblue",
                          color_qq = "steelblue",
                          color_qq_line = "steelblue4",
                          color_sqrt = "darkorange",
                          color_log = "salmon") {

  if (!is.data.frame(.data)) {
    stop(call. = FALSE, ".data is not a data-frame or tibble. Please supply a data.frame or tibble.")
  }

  df <- dplyr::as_tibble(.data)

  df_num <- df %>% dplyr::select(c(tidyselect::where(~ is.numeric(.x))))

  if (length(df_num)==0){
    stop(call. = FALSE, "Please supply a data-frame or tibble with a numeric column")
  }

  df_num$do <- "Original Distribution"
  df_num$tqq <- "Q-Q Plot"
  df_num$tsqrt <- "Sqrt Transformation"
  df_num$tlog <- "Log Transformation"


  variable <- df_num %>% dplyr::select(-c(do, tqq, tsqrt, tlog)) %>% names()

  for (i in variable) {

    vars <- rlang::sym(i)

    my_theme <- ggplot2::theme_minimal()+
      ggplot2::theme(
        axis.text = ggplot2::element_text(size = 8),
        axis.title = ggplot2::element_text(size = 9),
        strip.text = ggplot2::element_text(face = "bold")
      )

    p1 <- df_num %>% dplyr::filter(!is.na(!!vars)) %>%
      ggplot2::ggplot(ggplot2::aes(x = !!vars) ) +
      ggplot2::geom_density(fill = color_density, color = color_density, alpha = 0.7) +
      ggplot2::scale_x_continuous(labels = scales::comma) +
      ggplot2::facet_wrap(~ do) +
      my_theme

    p2 <- df_num %>% dplyr::filter(!is.na(!!vars)) %>%
      ggplot2::ggplot(ggplot2::aes(sample = !!vars))+
      ggplot2::stat_qq(color = color_qq, alpha = 0.7) +
      ggplot2::stat_qq_line(color = color_qq_line, alpha = 1)+
      ggplot2::facet_wrap(~ tqq) +
      my_theme

    p3 <- df_num %>% dplyr::filter(!is.na(!!vars)) %>%
      ggplot2::ggplot(ggplot2::aes(x = sqrt(!!vars))) +
      ggplot2::geom_density(fill = color_sqrt, color = color_sqrt, alpha = 0.7) +
      ggplot2::scale_x_continuous(labels = scales::comma) +
      ggplot2::facet_wrap(~ tsqrt) +
      my_theme

    p4 <- df_num %>% dplyr::filter(!is.na(!!vars)) %>%
      ggplot2::ggplot(ggplot2::aes(x = log(!!vars))) +
      ggplot2::geom_density(fill = color_log, color = color_log, alpha = 0.7) +
      ggplot2::scale_x_continuous(labels = scales::comma) +
      ggplot2::facet_wrap(~ tlog) +
      my_theme


    if(!interactive){
      p <- ggpubr::ggarrange(p1,p2,p3,p4,ncol = 2,nrow = 2) %>%
        ggpubr::annotate_figure(
          top = ggpubr::text_grob(paste(i), face = "bold", size = 11)
        )
      print(p)
    }else{
      pt1 <- plotly::ggplotly(p1)
      pt2 <- plotly::ggplotly(p2)
      pt3 <- plotly::ggplotly(p3)
      pt4 <- plotly::ggplotly(p3)

      pt <- plotly::subplot(p1,p2,p3,p4, nrows = 2, margin = 0.07) %>%
        plotly::layout(
          title = list(text = paste('<b>', i, '</b>')),
          font = list(family = "Segoe UI"),
          margin = list(t = 70),
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

}

