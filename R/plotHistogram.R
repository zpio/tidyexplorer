#' Plot Histograms in ggplot2
#'
#' This function plots a Density Chart for all numeric variables,
#' with the option to fill by a categorical variable.
#'
#' @param .data A data.frame or tibble.
#' @param fill_var (Optional) A categorical variable to group and fill the plot.
#' @param interactive A boolean value that specifies whether the plot should be returned static (ggplot2) or interactive (Plotly).
#' @param facet_grid A boolean value that specifies whether the chart should be displayed in facet_grid when there are only two numeric variables and one categorical variable specified in fill_var.
#' @param ncol_facet An integer value indicating the number of columns for facet_grid. Defaults to 3.
#' @param bins An integer value indicating the number of bins for the histogram. Defaults to 30.
#' @param alpha A numeric value indicating the alpha (transparency) for the histogram bars. Defaults to 1.
#' @param scales A string value indicating how the axes scales should be computed. Defaults to "free".
#'
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
#'    plotHistogram()
#'
#' mtcars %>%
#'    plotHistogram()
#'
#' iris %>%
#'    plotHistogram(fill_var = Species, alpha = 0.7)
#'
#' iris %>% select(Sepal.Length, Species) %>%
#'    plotHistogram(fill_var = Species, facet_grid = TRUE)
#'
#' @export
plotHistogram <- function(.data,
                          fill_var = NULL,
                          interactive = FALSE,
                          facet_grid = FALSE,
                          ncol_facet = 3,
                          bins = 30,
                          alpha = 1,
                          scales = "free") {

  if (!is.data.frame(.data)) {
    stop(call. = FALSE, ".data is not a data-frame or tibble. Please supply a data.frame or tibble.")
  }

  fill  <- rlang::enquo(fill_var)

  if (!rlang::quo_is_null(fill)) {

    df_fill <- .data %>%  dplyr::select(!!fill) %>%
      dplyr::select(c(tidyselect::where(~ is.character(.x)|is.factor(.x)|is.ordered(.x))))

    if (length(df_fill)==0){
      stop(call. = FALSE, "Please supply a data-frame or tibble with a categorical column")
    } else {
      data <- .data %>% dplyr::group_by(!!fill)
    }

  } else {
    data <- .data
  }


  df_num <- data %>% dplyr::select(c(tidyselect::where(~ is.numeric(.x))))

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
      ggplot2::ggplot()+
      ggplot2::geom_histogram(
        ggplot2::aes(x = value, fill=variable),
        alpha = alpha, bins = bins, color = "white"
      )+
      ggplot2::scale_fill_manual(values = pal_discrete, guide="none")+
      ggplot2::scale_color_manual(values = pal_discrete, guide="none")+
      ggplot2::scale_x_continuous(labels = scales::comma) +
      ggplot2::facet_wrap(
        dplyr::vars(variable), scales = scales, ncol = ncol_facet
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
      ggplot2::ggplot()+
      ggplot2::geom_histogram(
        ggplot2::aes(x = value, fill = !!group_var),
        alpha = alpha, bins = bins, color = "white"
      )+
      ggplot2::facet_wrap(dplyr::vars(variable), scales = scales, ncol = ncol_facet) +
      ggplot2::scale_x_continuous(labels = scales::comma)+
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
        ggplot2::ggplot()+
        ggplot2::geom_histogram(
          ggplot2::aes(x = !!var_x, fill=!!group_var),
          alpha = alpha, bins = bins, color = "white"
        )+
        ggplot2::scale_x_continuous(labels = scales::comma) +
        ggplot2::scale_fill_manual(values = pal_discrete, guide='none')+
        ggplot2::scale_color_manual(values = pal_discrete, guide='none')+
        ggplot2::facet_grid(dplyr::vars(!!group_var), scales = scales) +
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
