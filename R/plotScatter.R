#' Plot Scatter Chart in ggplot2
#'
#' This function plots a Scatter Chart for all numeric variables and a target numeric variable (y-axis),
#' with the option to fill by a categorical variable.
#'
#' @param .data A data.frame or tibble.
#' @param y A target numeric variable to be plotted on the y-axis.
#' @param fill_var (Optional) A categorical variable to group and fill the plot.
#' @param smooth A boolean that specifies whether a linear regression line should be added to the plot. Default is TRUE.
#' @param interactive A boolean that specifies whether the plot should be returned static (ggplot2) or interactive (Plotly).
#' @param ncol_facet A Boolean that specifies number of columns for the facet_wrap function. Default is 3.
#'
#' @return A static `ggplot2` plot or an interactive `plotly` plot
#'
#' @examples
#'
#' library(dplyr)
#'
#' mtcars %>%
#'    plotScatter(y = disp)
#'
#' iris %>%
#'    select(Petal.Width, Sepal.Length, Species) %>%
#'    plotScatter(y = Petal.Width, fill_var = Species)
#'
#' @export
plotScatter <- function(.data,
                        y,
                        fill_var = NULL,
                        smooth = TRUE,
                        interactive = FALSE,
                        ncol_facet = 3) {

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

  obj <- df_num %>% dplyr::select({{y}}) %>% names()

  group_var <- df_num %>% dplyr::group_vars()

  if(length(group_var)==0){
    p1 <- df_num %>%
      tidyr::pivot_longer(
        !dplyr::any_of(obj),
        names_to = "variable",
        values_to = "value"
      ) %>%
      dplyr::arrange(variable) %>%
      ggplot2::ggplot(ggplot2::aes(x = value, y = {{y}}, fill = variable, color = variable))+
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
      ggplot2::ggplot(ggplot2::aes(x = value, y = {{y}}, fill = !!group_var, color = !!group_var))+
      ggplot2::scale_fill_manual(values = pal_discrete)+
      ggplot2::scale_color_manual(values = pal_discrete)

  } else{

    stop("Please supply group_by with 1 column")
  }

  if(smooth){
    p1 <- p1 +
      ggplot2::geom_point(shape = 21, alpha = 1, size = 2, color="black")+
      ggplot2::geom_smooth(method = lm, se = TRUE, alpha = 0.1, size = 0.5)
  } else{
    p1 <- p1 +
      ggplot2::geom_point(shape = 21, alpha = 1, size = 2, color="black")
  }

  p1 <- p1 +
    ggplot2::labs(title = "Numerical Variables", x = "")+
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
