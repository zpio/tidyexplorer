#' Visualize Distributions with Raincloud Plots
#'
#' @param .data .data A `tibble` or `data.frame`
#' @param interactive Returns either a static (`ggplot2`) visualization or an interactive (`plotly`) visualization
#' @param fill_ind fill
#' @param color_ind color
#'
#' @return A static `ggplot2` plot or an interactive `plotly` plot
#'
#' @examples
#'
#' library(dplyr)
#'
#' iris %>%
#'    group_by(Species) %>%
#'    select(Sepal.Width) %>%
#'    plotRaincloud()
#'
#' @export
plotRaincloud <- function(.data,
                          interactive = FALSE,
                          fill_ind = '#4e79a7',
                          color_ind = '#4e79a7') {

  if (!is.data.frame(.data)) {
    stop(call. = FALSE, ".data is not a data-frame or tibble. Please supply a data.frame or tibble.")
  }

  df_num <- .data %>% dplyr::ungroup() %>%
    dplyr::select(c(tidyselect::where(~is.numeric(.x))))

  if (length(df_num)==0){
    stop(call. = FALSE, "Please supply a data-frame or tibble with a numeric column")
  }

  if (length(df_num) > 1){
    stop(call. = FALSE, "Please supply 1 numeric column")
  }

  pal_discrete <-
    rep(ggthemes::ggthemes_data$tableau$`color-palettes`$regular$`Tableau 10`$value, 10)

  names <- df_num %>% names()

  group_var <- .data %>% dplyr::group_vars()

  if(length(group_var)==0){

    name <- df_num %>% names()

    var <- rlang::sym(name)

    box <-  df_num %>%
      dplyr::summarise(
        median = stats::median(!!var, na.rm = TRUE),
        mean = mean(!!var, na.rm = TRUE),
        p25 = stats::quantile(!!var, .25, na.rm = TRUE),
        p75 = stats::quantile(!!var, .75, na.rm = TRUE),
        iqr = p75 - p25,
        lower_whisker =  p25 - 1.5*iqr,
        upper_whisker =  p75 + 1.5*iqr
      )

    p1 <- df_num %>%
      ggplot2::ggplot()+
      ggplot2::geom_density(
        data = df_num,
        ggplot2::aes(x = !!var, y = ggplot2::after_stat(scaled)),
        fill = fill_ind, color = color_ind,
        alpha = 0.8, size = 1
      )+
      ggplot2::geom_jitter(
        data = df_num %>% dplyr::sample_n(100, replace = TRUE),
        ggplot2::aes(
          x = !!var,
          y = -0.2
        ),
        fill = fill_ind,
        color = color_ind,
        height = 0.2,
        width = 0.2,
        alpha = 0.5
      )+
      #errorbar
      ggplot2::stat_boxplot(
        data = df_num,
        ggplot2::aes(x = !!var, y = -0.2),
        geom = "errorbar", width = 0.15, size = 1, color=color_ind
      )+
      # box
      ggplot2::geom_rect(
        data = box,
        ggplot2::aes(
          xmin = p25,
          xmax = median,
          ymin = - 0.3,
          ymax = - 0.1
        ),
        fill = fill_ind,
        color = color_ind,
        alpha = 0.9,
        size = 1
      )+
      ggplot2::geom_rect(
        data = box,
        ggplot2::aes(
          xmin = median,
          xmax = p75,
          ymin = - 0.3,
          ymax = - 0.1
        ),
        fill = fill_ind,
        color = color_ind,
        alpha = 0.9,
        size = 1
      )+
      # vertical line
      ggplot2::geom_segment(
        data = box,
        ggplot2::aes(
          x = median,
          #color = Species,
          xend = median,
          y = - 0.3,
          yend = - 0.1
        ),
        color = "white",
        size = 0.8,
        alpha = 0.8
      )+
      ggplot2::labs(y="", title = paste("Distribution of", name))+
      ggplot2::theme_minimal()+
      ggplot2::theme(
        legend.position = "none",
        axis.text =  ggplot2::element_text(size = 8),
        axis.title =  ggplot2::element_text(size = 9),
        strip.text.x = ggplot2::element_text(face="bold"),
        plot.title = ggplot2::element_text(size = 11, face="bold", hjust = 0.5),
        panel.grid.minor = ggplot2::element_blank()
      )

  } else if(length(group_var)==1){

    group <- .data %>% dplyr::group_vars()

    group_vars <- rlang::sym(group)

    var <- rlang::sym(df_num %>% names())

    name <- df_num %>% names()

    box <-  .data %>%
      dplyr::summarise(
        median = stats::median(!!var, na.rm = TRUE),
        mean = mean(!!var, na.rm = TRUE),
        p25 = stats::quantile(!!var, .25, na.rm = TRUE),
        p75 = stats::quantile(!!var, .75, na.rm = TRUE),
        dens_height = max(stats::density(!!var, na.rm = TRUE)$y),
        jitter_height = min(stats::density(!!var, na.rm = TRUE)$y),
        iqr = p75 - p25,
        lower_whisker =  p25 - 1.5*iqr,
        upper_whisker =  p75 + 1.5*iqr
      )

    p1 <- .data %>%
      ggplot2::ggplot()+
      ggplot2::geom_density(
        data = .data,
        ggplot2::aes(x = !!var, y = ggplot2::after_stat(scaled), fill = !!group_vars, color = !!group_vars),
        alpha = 0.8, size = 1
      )+
      ggplot2::geom_jitter(
        data = .data %>% dplyr::sample_n(100, replace = TRUE),
        ggplot2::aes(
          x = !!var,
          y = -0.2,
          fill = !!group_vars,
          color = !!group_vars
        ),
        height = 0.2,
        width = 0.2,
        alpha = 0.5
      )+
      #errorbar
      ggplot2::stat_boxplot(
        data = .data,
        ggplot2::aes(x = !!var, y = -0.2, color=!!group_vars),
        geom = "errorbar", width = 0.15, size = 1
      )+
      # box
      ggplot2::geom_rect(
        data = box,
        ggplot2::aes(
          xmin = p25,
          xmax = median,
          ymin = - 0.3,
          ymax = - 0.1,
          fill = !!group_vars,
          color = !!group_vars
        ),
        alpha = 0.9,
        size = 1
      )+
      ggplot2::geom_rect(
        data = box,
        ggplot2::aes(
          xmin = median,
          xmax = p75,
          ymin = - 0.3,
          ymax = - 0.1,
          fill = !!group_vars,
          color = !!group_vars
        ),
        alpha = 0.9,
        size = 1
      )+
      # vertical line
      ggplot2::geom_segment(
        data = box,
        ggplot2::aes(
          x = median,
          #color = Species,
          xend = median,
          y = - 0.3,
          yend = - 0.1
        ),
        color = "white",
        size = 0.8,
        alpha = 0.8
      )+
      ggplot2::facet_grid(dplyr::vars(!!group_vars), scales = 'free_y')+
      ggplot2::scale_fill_manual(values = pal_discrete, guide="none")+
      ggplot2::scale_color_manual(values = pal_discrete, guide="none")+
      ggplot2::labs(y="", title = paste("Distribution of", name, "by", group))+
      ggplot2::theme_minimal()+
      ggplot2::theme(
        legend.position = "none",
        axis.text =  ggplot2::element_text(size = 8),
        axis.title =  ggplot2::element_text(size = 9),
        strip.text.x = ggplot2::element_text(face="bold"),
        plot.title = ggplot2::element_text(size = 11, face="bold", hjust = 0.5),
        panel.grid.minor = ggplot2::element_blank()
      )

  } else{

    stop("Please supply group_by with 1 column")
  }

  if(!interactive){
    return(p1)
  }else{

    if(length(group_var)==0){
      title = paste('<b>', "Distribution of", names, '<b>')
    } else{
      title = paste('<b>','Distribution of', names, 'by', group_var,'<b>')
    }

    p <- plotly::ggplotly(p1, tooltip = c("x", "y", "fill")) %>%
      plotly::layout(
        showlegend = F,
        title = list(text= title, size = 1),
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

    return(p)
  }

}
