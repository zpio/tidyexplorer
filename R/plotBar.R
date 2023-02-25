#' Bar Plot based on target variable
#'
#' @param .data A `tibble` or `data.frame`
#' @param fill_var A categorical column that can be used to change the color
#' @param interactive  Returns either a static (`ggplot2`) visualization or an interactive (`plotly`) visualization
#' @param top Top
#' @param position_group Position
#' @param pct Pct
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
#' mtcars %>%
#'    plotBar()
#'
#' mtcars %>%
#'    plotBar(fill_var = cyl, position_group = "fill")
#'
#' @export
plotBar <- function(.data,
                    fill_var = NULL,
                    interactive = FALSE,
                    top = 10,
                    pct = TRUE,
                    position_group = "fill",
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


  df_cat <- data %>%
    dplyr::select(c(tidyselect::where(~ is.character(.x)|is.factor(.x)|is.ordered(.x))))


  if (length(df_cat)==0){
    stop(call. = FALSE, "Please supply a data-frame or tibble with a categorical column")
  }

  pal_discrete <-
    rep(ggthemes::ggthemes_data$tableau$`color-palettes`$regular$`Tableau 10`$value, 10)

  group_var <- df_cat %>% dplyr::group_vars()

  if(length(group_var)==0){
    data <- df_cat %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
      tibble::rownames_to_column() %>%
      tidyr::gather(key = "variable", value = "levels", -rowname) %>%
      dplyr::select(-rowname) %>%
      dplyr::count(variable, levels) %>%
      dplyr::group_by(variable) %>%
      dplyr::arrange(dplyr::desc(n), .by_group=TRUE) %>%
      dplyr::slice_head(n = 10) %>%
      dplyr::mutate(levels = tidytext::reorder_within(levels, n, variable)) %>%
      dplyr::mutate(pct= n/sum(n))

    if(pct){
      p1 <- data %>%
        ggplot2::ggplot(ggplot2::aes(x=levels, y=pct, fill=variable))
    } else{
      p1 <- data %>%
        ggplot2::ggplot(ggplot2::aes(x=levels, y=n, fill=variable))
    }

    p1 <- p1+
      ggplot2::geom_bar(stat="identity", alpha=0.8, position = "stack")+
      ggplot2::scale_fill_manual(values = pal_discrete, guide="none")+
      ggplot2::facet_wrap(dplyr::vars(variable), scales = "free", ncol = ncol_facet)+
      ggplot2::coord_flip()+
      tidytext::scale_x_reordered()+
      ggplot2::labs(title = 'Catgorical Variables', y = "", x="")+
      ggplot2::theme_minimal()+
      ggplot2::theme(
        axis.text = ggplot2::element_text(size = 8),
        plot.title = ggplot2::element_text(size = 11, face="bold", hjust = 0.5),
        panel.spacing.y = ggplot2::unit(1, "lines")
      )

  }else if(length(group_var)==1){

    group_var <- rlang::sym(df_cat %>% group_vars())

    p1 <- df_cat %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
      tidyr::gather(key = "variable", value = "levels", -!!group_var) %>%
      dplyr::count(!!group_var, variable, levels) %>%
      dplyr::group_by(variable) %>%
      dplyr::arrange(dplyr::desc(n), .by_group=TRUE) %>%
      dplyr::slice_head(n = 10) %>%
      dplyr::mutate(levels = tidytext::reorder_within(levels, n, variable)) %>%

      ggplot2::ggplot(ggplot2::aes(x=levels, y=n, fill=!!group_var))+
      ggplot2::geom_bar(stat="identity", alpha=0.8, position = position_group)+
      ggplot2::scale_fill_manual(values = pal_discrete)+
      ggplot2::scale_y_continuous(labels = scales::comma)+
      ggplot2::facet_wrap(ggplot2::vars(variable), scales = "free", ncol = ncol_facet)+
      ggplot2::coord_flip()+
      tidytext::scale_x_reordered()+
      ggplot2::labs(title = 'Catgorical Variables', y = "", x="")+
      ggplot2::theme_minimal()+
      ggplot2::theme(
        axis.text = ggplot2::element_text(size = 8),
        plot.title = ggplot2::element_text(size = 11, face="bold", hjust = 0.5),
        panel.spacing.y = ggplot2::unit(1, "lines")
      )

  } else{

    stop("Please supply group_by with 1 column")
  }

  if(!interactive){
    return(p1)
  }else{
    p <- plotly::ggplotly(p1) %>%
      plotly::layout(
        showlegend = T,
        #title = paste('<b>Catgorical Variables</b>'),
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
