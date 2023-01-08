#' Scatter Plot based on target variable
#'
#' @param .data A `tibble` or `data.frame`
#' @param target A variable numeric
#' @param smooth smooth
#' @param interactive Returns either a static (`ggplot2`) visualization or an interactive (`plotly`) visualization
#' @param ncol_facet Number of facet columns
#'
#' @return A static `ggplot2` plot or an interactive `plotly` plot
#'
#' @examples
#'
#' library(dplyr)
#'
#' mtcars %>%
#'    plotScatter(target = disp)
#'
#' @export
plotScatter <- function(.data,
                        target,
                        smooth = TRUE,
                        interactive = FALSE,
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

  obj <- df_num %>% dplyr::select({{target}}) %>% names()

  group_var <- df_num %>% dplyr::group_vars()

  if(length(group_var)==0){
    p1 <- df_num %>%
      tidyr::pivot_longer(
        !dplyr::any_of(obj),
        names_to = "variable",
        values_to = "value"
      ) %>%
      dplyr::arrange(variable) %>%
      ggplot2::ggplot(ggplot2::aes(x = value, y = {{target}}, fill = variable, color = variable))+
      ggplot2::scale_fill_manual(values = pal_discrete, guide="none")+
      ggplot2::scale_color_manual(values = pal_discrete, guide="none")

  }else if(length(group_var)==1){
    group_var <- rlang::sym(df_num %>% dplyr::group_vars())

    p1 <- df_num %>%
      tidyr::pivot_longer(
        !dplyr::any_of(obj),
        names_to = "variable",
        values_to = "value"
      ) %>%
      dplyr::arrange(variable) %>%
      ggplot2::ggplot(ggplot2::aes(x = value, y = {{target}}, fill = !!group_var, color = !!group_var))+
      ggplot2::scale_fill_manual(values = pal_discrete)+
      ggplot2::scale_color_manual(values = pal_discrete)

  } else{

    stop("Please supply group_by with 1 column")
  }

  if(smooth){
    p1 <- p1 +
      ggplot2::geom_point(alpha = 0.7)+
      ggplot2::geom_smooth(alpha = 0.2, size=0.5)
  } else{
    p1 <- p1 +
      ggplot2::geom_point(alpha = 0.5)
  }

  p1 <- p1 +
    ggplot2::labs(title = "Numerical Variables")+
    ggplot2::scale_x_continuous(labels = scales::comma) +
    ggplot2::facet_wrap(dplyr::vars(variable), scales = "free", ncol = ncol_facet)+
    ggplot2::theme_minimal()+
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 8),
      plot.title = ggplot2::element_text(size = 11, face="bold", hjust = 0.5),
      panel.spacing.y = ggplot2::unit(1, "lines")
    )

  if(!interactive){
    return(p1)
  }else{
    p <- plotly::ggplotly(p1, tooltip = c("x", "y", "fill")) %>%
      plotly::layout(
        showlegend = FALSE,
        #title = paste("Scatter Plot"),
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
