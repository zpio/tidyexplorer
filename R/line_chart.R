#' Line Chart
#'
#' Plot Line Chart in ggplot2
#'
#'
#' @param data A data frame or tibble
#' @param x A numeric variable
#' @param y A numeric variable
#' @param color_var (Optional) A categorical variable to group and color the plot.
#' @param color A string indicating the color to use for the line when color_var is not provide.
#' @param label_var (Optional) A categorical variable to use for labeling the lines.
#' @param facet_wrap_var (Optional) A categorical variable to use for faceting the plot.
#' @param facet_ncol An integer indicating the number of columns to use for facet_wrap. Defaults to 2.
#' @param scales A string indicating the type of scaling to use in facet (default: "free").
#' @param title A string indicating the title.
#' @param linewidth The width of the line.
#'
#' @return A ggplot object
#'
#' @examples
#'
#' library(dplyr)
#' library(gapminder)
#'
#' line_chart(ggplot2::economics,
#'            x = date,
#'            y = unemploy)
#'
#' economics_long2 <-
#'   dplyr::filter(
#'     ggplot2::economics_long,
#'     variable %in% c("pop", "uempmed", "unemploy")
#'   )
#'
#' line_chart(economics_long2,
#'            x = date,
#'            y = value01,
#'            color_var = variable)
#'
#' line_chart(economics_long2,
#'            x = date,
#'            y = value01,
#'            color_var = variable,
#'            facet_wrap_var = variable,
#'            facet_ncol = 2)
#'
#'
#' multiple_line_df <- gapminder::gapminder %>%
#'   filter(country %in% c("China", "United States", "Ecuador", "Peru")) %>%
#'   mutate(label = if_else(year == max(year), as.character(country), NA_character_))
#'
#'
#' line_chart(multiple_line_df,
#'            x = year,
#'            y = lifeExp,
#'            color_var = country)
#'
#' line_chart(multiple_line_df,
#'            x = year,
#'            y = lifeExp,
#'            color_var = country,
#'            facet_wrap_var = country)
#'
#' @export
line_chart <- function(data, x, y,
                       color_var = NULL,
                       color = "#4E79A7",
                       label_var = NULL,
                       facet_wrap_var = NULL,
                       facet_ncol = 2,
                       scales = "free",
                       title = "Plot Time Series",
                       linewidth = 1){

  if (!is.data.frame(data)) {
    stop(call. = FALSE, ".data is not a data-frame or tibble. Please supply a data.frame or tibble.")
  }

  x  <- rlang::enquo(x)
  y  <- rlang::enquo(y)
  color_var  <- rlang::enquo(color_var)
  label_var  <- rlang::enquo(label_var)
  facet_wrap_var  <- rlang::enquo(facet_wrap_var)


  pal_discrete <-
    rev(rep(ggthemes::ggthemes_data$tableau$`color-palettes`$regular$`Tableau 10`$value, 10))


  # color var
  if (!rlang::quo_is_null(color_var)){

    color_var_ch <- data %>% dplyr::select(!!color_var) %>%
      dplyr::select(c(tidyselect::where(~ is.character(.x)|is.factor(.x)|is.ordered(.x))))

    if (length(color_var_ch)==0){
      stop(call. = FALSE, "Please supply a color_var with a categorical column")
    }

    p1 <- ggplot2::ggplot(
      data, ggplot2::aes(x = !!x, y = !!y, color = !!color_var)
    ) +
      ggplot2::geom_line(linewidth = linewidth) +
      ggplot2::scale_color_manual(values = rev(pal_discrete))

    ## Facet
    if (!rlang::quo_is_null(facet_wrap_var)){

      facet_wrap_var_ch <- data %>% dplyr::select(!!facet_wrap_var) %>%
        dplyr::select(c(tidyselect::where(~ is.character(.x)|is.factor(.x)|is.ordered(.x))))

      if (length(facet_wrap_var_ch)==0){
        stop(call. = FALSE, "Please supply a facet_wrap_var with a categorical column")
      }

      p1 <- p1 +
        ggplot2::facet_wrap(dplyr::vars(!!facet_wrap_var), scales = scales, ncol = facet_ncol)

    } else{

      p1
    }

    ## Label
    if (!rlang::quo_is_null(label_var)){

      label_var_ch <- data %>% dplyr::select(!!label_var) %>%
        dplyr::select(c(tidyselect::where(~ is.character(.x)|is.factor(.x)|is.ordered(.x))))

      if (length(label_var_ch)==0){
        stop(call. = FALSE, "Please supply a label_var with a categorical column")
      }

      p1 <- p1 +
        ggrepel::geom_text_repel(
          ggplot2::aes(label = !!label_var)
        )

    } else{

      data_label <- data %>%
        dplyr::mutate(label = ifelse(!!x == max(!!x), as.character(!!color_var), NA_character_))


      if (!rlang::quo_is_null(facet_wrap_var)){

        facet_wrap_var_ch <- data %>% dplyr::select(!!facet_wrap_var) %>%
          dplyr::select(c(tidyselect::where(~ is.character(.x)|is.factor(.x)|is.ordered(.x))))

        if (length(facet_wrap_var_ch)==0){
          stop(call. = FALSE, "Please supply a facet_wrap_var with a categorical column")
        }

        p1 <- p1

      } else{

        p1 <- p1 +
          ggrepel::geom_text_repel(
            data = data_label,
            ggplot2::aes(label = label),
            nudge_x = 2, size = 3, na.rm = TRUE
          )
      }

    }

    # color
  } else{

    p1 <- ggplot2::ggplot(
      data, ggplot2::aes(x = !!x, y = !!y)
    ) +
      ggplot2::geom_line(linewidth = linewidth, color = color)


    ## Facet
    if (!rlang::quo_is_null(facet_wrap_var)){

      facet_wrap_var_ch <- data %>% dplyr::select(!!facet_wrap_var) %>%
        dplyr::select(c(tidyselect::where(~ is.character(.x)|is.factor(.x)|is.ordered(.x))))

      if (length(facet_wrap_var_ch)==0){
        stop(call. = FALSE, "Please supply a var_x with a categorical column")
      }

      p1 <- p1 +
        ggplot2::facet_wrap(vars(!!facet_wrap_var), scales = scales, ncol = facet_ncol)

    }else{

      p1
    }

  }
  # plot
  p1 <- p1 +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(scale_cut = scales::cut_short_scale())
    ) +
    ggplot2::labs(x="", title = title) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.minor = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = 8, color = "gray30"),
      axis.title = ggplot2::element_text(size = 11, hjust = 1, color = "gray30"),
      plot.title = ggplot2::element_text(color = "grey30")
    )

  return(p1)

}
