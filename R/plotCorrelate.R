#' Visualize correlation coefficient
#'
#' @param .data A `tibble` or `data.frame`
#' @param method a character string: "pearson", "kendall" or "spearman".
#' @param interactive TRUE/FLASE. Returns either a static (`ggplot2`) visualization or an interactive (`plotly`) visualization
#'
#' @return A static `ggplot2` plot or an interactive `plotly` plot.
#'
#' @examples
#'
#' library(dplyr)
#'
#' iris %>%
#'    plotCorrelate(interactive = TRUE)
#'
#'
#' @export
plotCorrelate <- function(.data,
                          method = "pearson",
                          interactive = FALSE){
  if (!is.data.frame(.data)) {
    stop(call. = FALSE, ".data is not a data-frame or tibble. Please supply a data.frame or tibble.")
  }

  df_num <- .data %>% dplyr::ungroup() %>% dplyr::select(c(tidyselect::where(~ is.numeric(.x))))

  if (length(df_num)==0){
    stop(call. = FALSE, "Please supply a data-frame or tibble with a numeric column")
  }

  matrix_cor <- stats::cor(
    x = df_num,
    method = "pearson",
    use = "pairwise.complete.obs"
  ) %>% round(3)

  matrix_cor[lower.tri(matrix_cor, diag = TRUE)] <- ""

  df_cor <- matrix_cor %>% dplyr::as_tibble() %>%
    dplyr::mutate_if(is.character, as.numeric)

  df <- df_cor %>%
    dplyr::mutate(var1 = names(df_cor)) %>%
    tidyr::gather(key = "var2", value = "cor", -var1) %>%
    dplyr::filter(cor != is.na(cor)) %>%
    dplyr::mutate(
      cor = round(cor, 2),
      variables = paste(var1, "-", var2)
    ) %>%
    dplyr::arrange(dplyr::desc(cor)) %>%
    dplyr::mutate(variables = stats::reorder(variables, cor))

  p1 <-
    ggplot2::ggplot(
      df, ggplot2::aes(x = cor, y = variables, color = cor, fill = cor)
    ) +
    ggplot2::geom_bar(stat = "identity")+
    ggplot2::geom_text(
      ggplot2::aes(label = cor, hjust = dplyr::if_else(cor < 0, 1.10, -0.10)),
      size = 3, color = "grey20"
    )+
    ggplot2::scale_x_continuous(limits=c(-1.1,1.1)) +
    ggthemes::scale_fill_gradient2_tableau(palette = "Red-Blue Diverging")+
    ggthemes::scale_color_gradient2_tableau(palette = "Red-Blue Diverging")+
    ggplot2::labs(title = 'Correlation Coefficient', y = '')+
    ggplot2::theme_minimal()+
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 8),
      plot.title = ggplot2::element_text(size = 11, face="bold", hjust = 0.5)
    )

  if(!interactive){
    return(p1)
  }else{
    p <- plotly::ggplotly(p1, tooltip = c("y", "fill")) %>%
      plotly::layout(
        showlegend = T,
        title = paste('<b>Correlation Coefficient</b>'),
        font = list(family = "Segoe UI"),
        hoverlabel = list(
          font = list(
            color = "white",
            family = "Consolas"
          ),
          bordercolor = "transparent"
        )
      )

    return(p)
  }

}
