#' Visualization of outliers
#'
#' @param .data A `tibble` or `data.frame`
#' @param interactive Returns either a static (`ggplot2`) visualization or an interactive (`plotly`) visualization
#'
#' @return A static `ggplot2` plot or an interactive `plotly` plot
#'
#' @examples
#' library(dplyr)
#' iris %>%
#'    select(Sepal.Width) %>%
#'    plotOutliers()
#'
#' @export
#'
plotOutliers <- function(.data, interactive=FALSE) {

  if (!is.data.frame(.data)) {
    stop(call. = FALSE, ".data is not a data-frame or tibble. Please supply a data.frame or tibble.")
  }

  df_num <- .data %>% dplyr::select(c(tidyselect::where(~ is.numeric(.x))))

  if (length(df_num)==0){
    stop(call. = FALSE, "Please supply a data-frame or tibble with a numeric column")
  }

  name <- df_num %>% names()

  pal_discrete <-
    rep(ggthemes::ggthemes_data$tableau$`color-palettes`$regular$`Tableau 10`$value, 10)

  for (i in name) {

    var <- rlang::sym(i)

    # with outlier

    data_with_outlier <-
      df_num %>%
      dplyr::select(!!var) %>%
      dplyr::filter(!is.na(!!var)) %>%
      dplyr::mutate(name = 'With Outliers')

    # without outlier

    quartiles <- df_num %>%
      dplyr::select(!!var) %>%
      dplyr::filter(!is.na(!!var)) %>%
      stats::quantile(probs=c(.25, .75), na.rm = TRUE) #vector 2 elements

    IQR <- df_num %>%
      dplyr::select(!!var) %>%
      dplyr::filter(!is.na(!!var)) %>%
      dplyr::pull() %>%
      stats::IQR(na.rm = TRUE) #vector 1 element

    Lower <- quartiles[1] - 1.5*IQR
    Upper <- quartiles[2] + 1.5*IQR

    data_without_outlier <-
      df_num %>%
      dplyr::select(!!var) %>%
      dplyr::filter(!is.na(!!var)) %>%
      dplyr::filter((!!var) > Lower) %>%
      dplyr::filter((!!var) < Upper) %>%
      dplyr::mutate(name = 'Without Outliers')

    data <- dplyr::bind_rows(data_with_outlier, data_without_outlier)

    box <- data %>%
      dplyr::group_by(name) %>%
      dplyr::summarise(
        median = stats::median(!!var, na.rm = TRUE),
        mean = mean(!!var, na.rm = TRUE),
        p25 = stats::quantile(!!var, .25, na.rm = TRUE),
        p75 = stats::quantile(!!var, .75, na.rm = TRUE),
        dens_height = max(density(!!var, na.rm = TRUE)$y),
        iqr = p75 - p25,
        lower_whisker =  p25 - 1.5*iqr,
        upper_whisker =  p75 + 1.5*iqr
      ) %>% dplyr::ungroup()


    p1 <- data %>%
      ggplot2::ggplot()+

      #density
      ggplot2::geom_density(
        ggplot2::aes(x = !!var, y =  ggplot2::after_stat(scaled), fill=name, color=name),
        alpha = 0.7, size = 1
      )+

      #points
      ggplot2::geom_point(
        data = data %>% filter((!!var) < Lower | (!!var) > Upper),
        ggplot2::aes(x = !!var, y = -0.2, fill = name, color = name),
        alpha = 0.4, size=1
      )+

      # errorbar
      ggplot2::stat_boxplot(
        ggplot2::aes(x=!!var, y = -0.2, color=name),
        geom = "errorbar", width = 0.10
      )+

      # box
      ggplot2::geom_rect(
        data = box,
        ggplot2::aes(
          xmin = p25, xmax = median,
          ymin = - 0.3, ymax = - 0.1,
          fill = name, color = name
        ),
        alpha = 0.9, linewidth = .5
      )+
      ggplot2::geom_rect(
        data = box,
        ggplot2::aes(
          xmin = median, xmax = p75,
          ymin = - 0.3, ymax = - 0.1,
          fill = name, color = name
        ),
        alpha = 0.9, linewidth = .5
      )+

      # vertical line
      ggplot2::geom_segment(
        data = box,
        ggplot2::aes(
          x = median, xend = median,
          y = - 0.3, yend = - 0.1
        ),
        linewidth = 0.5, color ="white"
      )+

      ggplot2::scale_x_continuous(labels = scales::comma) +
      ggplot2::scale_fill_manual(values = pal_discrete, guide="none")+
      ggplot2::scale_color_manual(values = pal_discrete, guide="none")+
      ggplot2::facet_wrap(~ name, scales="free", ncol=2)+
      ggplot2::theme_minimal()+
      ggplot2::labs(y="", title = paste("Outlier Plot:", i))+
      ggplot2::theme(
        axis.text =  ggplot2::element_text(size = 8),
        axis.title =  ggplot2::element_text(size = 9),
        strip.text.x = ggplot2::element_text(face="bold"),
        plot.title = ggplot2::element_text(size = 11, face="bold", hjust = 0.5)
      )

    if(!interactive){
      print(p1)
    }else{
      pt <- plotly::ggplotly(p1, tooltip = c("fill", "x", "scaled")) %>%
        plotly::layout(
          title = paste('<b>Outlier Plot:</b>', i),
          font = list(family = "Segoe UI"),
          showlegend = FALSE,
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


