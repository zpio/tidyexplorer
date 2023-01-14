#' Visualize missing data
#'
#' @param .data A `tibble` or `data.frame`
#' @param interactive Returns either a static (`ggplot2`) visualization or an interactive (`plotly`) visualization
#'
#' @return A static `ggplot2` plot or an interactive `plotly` plot
#'
#' @examples
#'
#' iris %>%
#'    plotMissing()
#'
#' @export
plotMissing <- function(.data,
                        interactive = FALSE) {

  if (!is.data.frame(.data)) {
    stop(call. = FALSE, ".data is not a data-frame or tibble. Please supply a data.frame or tibble.")
  }

  dfDescription <- .data %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::everything(),
        list(
          type = ~ class(.x)[1],
          n = ~ length(.x),
          NAs = ~ sum(is.na(.x) | is.null(.x) | .x==""),
          NAsPct = ~ round(sum(is.na(.x) | is.null(.x) | .x=="")/length(.x),3),
          unique = ~ length(unique(.x)),
          uniquePct = ~ round(length(unique(.x))/length(.x),3)
        ),
        .names = "{.col}%%{.fn}"
      )
    ) %>%
    tibble::rownames_to_column() %>%
    tidyr::pivot_longer(
      !c(rowname),
      names_to = c("set", ".value"),
      names_pattern = "(.*)%%(.*)"
    ) %>%
    dplyr::select(variable = set, dplyr::everything(), -rowname) %>%
    dplyr::mutate(variable = stats::reorder(variable, NAsPct),
                  pct = sprintf("%.1f%%", NAsPct*100)) %>%
    dplyr::ungroup()

  pal_discrete <-
    rep(ggthemes::ggthemes_data$tableau$`color-palettes`$regular$`Tableau 10`$value, 10)

  p1 <-
    dfDescription %>%
    ggplot2::ggplot(
      ggplot2::aes(x = NAsPct, y = variable, fill=variable, label = pct)
    ) +
    ggplot2::geom_col() +
    ggplot2::scale_x_continuous(labels = scales::percent, limits=c(0,1))+
    ggplot2::scale_fill_manual(values = pal_discrete, guide = "none")+
    ggplot2::labs(title = "Percentage of Missing Values", x = "", y = "")+
    ggplot2::theme_minimal()+
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 8),
      plot.title = ggplot2::element_text(size = 11, face="bold", hjust = 0.5)
    )

  if(!interactive){

    p1 <- p1 +
      ggplot2::geom_text(
        ggplot2::aes(label = scales::percent(NAsPct), x = NAsPct + .025, y = variable),
        hjust = 0, colour = "gray30", size=3, inherit.aes = FALSE
      )

    return(p1)

  }else{
    p <- plotly::ggplotly(p1, tooltip = c("label", "y")) %>%
      plotly::layout(
        showlegend = F,
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
