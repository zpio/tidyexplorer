#' Visualize Distributions
#'
#' @param .data A `tibble` or `data.frame`
#' @param interactive Returns either a static (`ggplot2`) visualization or an interactive (`plotly`) visualization
#' @param facet_grid facet
#' @param ncol_facet Number of facet columns
#'
#' @return A static `ggplot2` plot or an interactive `plotly` plot
#'
#' @examples
#'
#' library(dplyr)
#'
#' mtcars <- mtcars %>% as_tibble() %>%
#'    mutate(across(c(am, carb, cyl, gear, vs), as.factor))
#'
#' iris %>%
#'    plotDensity()
#'
#' mtcars %>%
#'    plotDensity()
#'
#' @export
plotDensity <- function(.data,
                        interactive = FALSE,
                        facet_grid = FALSE,
                        ncol_facet = 3) {

  if (!is.data.frame(.data)) {
    stop(call. = FALSE, ".data is not a data-frame or tibble. Please supply a data.frame or tibble.")
  }

  df_num <- .data %>% dplyr::select(c(tidyselect::where(~ is.numeric(.x))))

  if (length(df_num)==0){
    stop(call. = FALSE, "Please supply a data-frame or tibble with a numeric column")
  }

  pal_discrete <-
    rep(ggthemes::ggthemes_data$tableau$`color-palettes`$regular$`Tableau 10`$value, 10)

  group_var <- df_num %>% dplyr::group_vars()

  if(length(group_var)==0){

    data <- df_num %>%
      tibble::rownames_to_column() %>%
      tidyr::pivot_longer(
        !rowname, names_to = "variable", values_to = "value"
      ) %>%
      dplyr::select(-rowname) %>%
      dplyr::arrange(variable) %>%
      dplyr::filter(!is.na(value))

    p1 <- data %>%
      ggplot2::ggplot(
        ggplot2::aes(x=value, y = -0.25, fill=variable, color=variable)
      )+
      ggplot2::geom_boxplot(alpha = 0.5, lwd=1, width = 0.20)+
      ggplot2::stat_boxplot(geom = "errorbar", width = 0.15)+
      ggplot2::geom_density(
        ggplot2::aes(x = value, y = ggplot2::after_stat(scaled),fill=variable, color=variable),
        inherit.aes = FALSE, alpha = 0.7, linewidth = 1
      )+
      ggplot2::scale_fill_manual(values = pal_discrete, guide="none")+
      ggplot2::scale_color_manual(values = pal_discrete, guide="none")+
      ggplot2::scale_x_continuous(labels = scales::comma) +
      ggplot2::facet_wrap(
        dplyr::vars(variable), scales = "free", ncol = ncol_facet
      )+
      ggplot2::labs(y="", x="", title = "Numerical Variables")+
      ggplot2::theme_minimal()+
      ggplot2::theme(
        axis.text = ggplot2::element_text(size = 8),
        plot.title = ggplot2::element_text(size = 11, face="bold", hjust = 0.5),
        panel.spacing.y = ggplot2::unit(1, "lines")
      )

  } else if(length(group_var)==1){

    group_var <- rlang::sym(df_num %>% dplyr::group_vars())

    data <- df_num %>%
      tidyr::pivot_longer(
        !dplyr::any_of(group_var),
        names_to = "variable",
        values_to = "value"
      ) %>%
      dplyr::arrange(variable) %>%
      dplyr::filter(!is.na(value))

    p1 <- data %>%
      ggplot2::ggplot(
        ggplot2::aes(x = value, fill = !!group_var, color = !!group_var)
      )+
      ggplot2::geom_density(alpha = 0.5, linewidth = 1)+
      ggplot2::facet_wrap(dplyr::vars(variable), scales = "free", ncol = ncol_facet) +
      ggplot2::scale_x_continuous(labels = scales::comma) +
      ggplot2::scale_fill_manual(values = pal_discrete)+
      ggplot2::scale_color_manual(values = pal_discrete)+
      ggplot2::labs(y="", x = "", title = "Numerical Variables")+
      ggplot2::theme_minimal()+
      ggplot2::theme(
        axis.text = ggplot2::element_text(size = 8),
        plot.title = ggplot2::element_text(size = 11, face="bold", hjust = 0.5),
        panel.spacing.y = ggplot2::unit(1, "lines")
      )

    if(facet_grid==TRUE && length(df_num)==2){

      group_var <- rlang::sym(df_num %>% group_vars())

      var_x <- rlang::sym(df_num %>% dplyr::ungroup() %>% dplyr::select(tidyselect::where(is.numeric)) %>% names())

      p1 <- df_num %>%
        ggplot2::ggplot(
          ggplot2::aes(x = !!var_x, y = -0.25, fill = !!group_var, color = !!group_var)
        )+
        ggplot2::geom_boxplot(alpha = 0.3, lwd=1, width = 0.20, outlier.shape = NA)+
        ggplot2::stat_boxplot(geom = "errorbar", width = 0.15)+
        ggplot2::geom_density(
          aes(x = !!var_x, y = ggplot2::after_stat(scaled), fill=!!group_var, color=!!group_var),
          inherit.aes = FALSE, alpha = 0.5, linewidth = 1)+
        ggplot2::scale_x_continuous(labels = scales::comma) +
        ggplot2::scale_fill_manual(values = pal_discrete, guide='none')+
        ggplot2::scale_color_manual(values = pal_discrete, guide='none')+
        ggplot2::facet_grid(dplyr::vars(!!group_var), scales = "free") +
        ggplot2::labs(y="", title = paste(var_x,"by",group_var))+
        ggplot2::theme_minimal()+
        ggplot2::theme(
          axis.text = ggplot2::element_text(size = 8),
          plot.title = ggplot2::element_text(size = 11, face="bold", hjust = 0.5),
          strip.text = ggplot2::element_text(face="bold"),
          panel.spacing.y = ggplot2::unit(1, "lines")
        )
    }

  } else{

    stop("Please supply group_by with 1 column")
  }

  if(!interactive){
    return(p1)
  }else{
    p <- plotly::ggplotly(p1, tooltip = c("x", "y", "fill")) %>%
      plotly::layout(
        showlegend = T,
        #title = list(text = paste('<b>Numerical Variables</b>'), size=1),
        font = list(family = "Segoe UI"),
        margin = list(t = 50, b=40),
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
