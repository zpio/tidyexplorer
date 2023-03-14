#' Bar Chart
#'
#' Plot a Bar Chart in ggplot2 with the option to fill by a categorical variable
#'
#' @param data A data.frame or tibble.
#' @param x A categorical variable that represents the x-axis.
#' @param y A numeric variable that represents the y-axis.
#' @param fill_var (Optional) A categorical variable to group and fill the plot.
#' @param label_var (Optional) A variable used to label the bars.
#' @param label_color A string indicating the color of the label text when label_var is provide. (Default: "black").
#' @param label_size An integer specifying the size of the label text when label_var is provide. (Default: 3).
#' @param title A string indicating the title.
#' @param fill A string indicating the color to fill bars when fill_var is not provided. (Default: "#4E79A7").
#' @param orientation A string indicating the orientation of the chart. Can be "h" for horizontal or "v" for vertical. (Default "h").
#' @param position A string indicating the position of the bars. Can be "dodge", "stack", or "fill". (Default "dodge").
#' @param scale_axis_pct A boolean value indicating whether the y-axis should be scaled by percent. (Default FALSE).
#' @param legend_position A string indicating the position of the legend. Can be "right", "left", "top", or "bottom". (Default "right").
#'
#' @return A ggplot object
#'
#' @examples
#' library(dplyr)
#'
#' mtcars <- mtcars %>% as_tibble() %>%
#'   mutate(across(c(am, carb, cyl, gear, vs), as.factor))
#'
#' data1_mtcars <- mtcars %>%
#'   count(cyl) %>%
#'  mutate(
#'    pct = round(n/sum(n),3),
#'    label = paste(pct*100, '%')
#'    )
#'
#' bar_chart(data1_mtcars, cyl, pct)
#'
#' bar_chart(data1_mtcars, cyl, pct,
#'          fill_var = cyl,
#'          scale_axis_pct = TRUE,
#'          label_var = label,
#'          orientation = "v")
#'
#'
#' data2_mtcars <- mtcars %>%
#'   count(am, cyl) %>%
#'  group_by(am) %>%
#'  mutate(
#'    pct = round(n/sum(n),3),
#'    label = paste(pct*100, '%')
#'    )
#'
#' bar_chart(data2_mtcars, am, pct,
#'          label_var = label,
#'          scale_axis_pct = TRUE,
#'          label_color = "white",
#'          fill_var = cyl,
#'          orientation = "h",
#'          position = "stack")
#'
#' @importFrom rlang :=
#' @export
bar_chart <- function(data, x, y,
                      fill_var = NULL,
                      label_var = NULL,
                      label_color = "black",
                      label_size = 3,
                      title = "",
                      fill = "#4E79A7",
                      orientation = "h",
                      position = "dodge",
                      scale_axis_pct = FALSE,
                      legend_position = "right") {

  if (!is.data.frame(data)) {
    stop(call. = FALSE, ".data is not a data-frame or tibble. Please supply a data.frame or tibble.")
  }

  x_var  <- rlang::enquo(x)
  y_var  <- rlang::enquo(y)
  fill_var  <- rlang::enquo(fill_var)
  label_var  <- rlang::enquo(label_var)

  var_x <- data %>% dplyr::ungroup() %>% dplyr::select(!!x_var) %>%
    dplyr::select(c(tidyselect::where(~ is.character(.x)|is.factor(.x)|is.ordered(.x))))

  if (length(var_x)==0){
    stop(call. = FALSE, "Please supply a var_x with a categorical column")
  }

  fill_var_cat <- data %>% dplyr::ungroup() %>% dplyr::select(!!fill_var) %>%
    dplyr::select(c(tidyselect::where(~ is.character(.x)|is.factor(.x)|is.ordered(.x))))

  fill_var_num <- data %>% dplyr::ungroup() %>% dplyr::select(!!fill_var) %>%
    dplyr::select(c(tidyselect::where(~ is.numeric(.x))))


  pal_discrete <-
    rev(rep(ggthemes::ggthemes_data$tableau$`color-palettes`$regular$`Tableau 10`$value, 10))


  if (!rlang::quo_is_null(fill_var)) {

    if (length(fill_var_cat)==1){

      data_mod <- data %>%
        dplyr::mutate(!!fill_var := forcats::fct_reorder(!!fill_var, -!!y_var))

    } else if(length(fill_var_num)==1){

      data_mod <- data %>%
        dplyr::mutate(!!x_var := forcats::fct_reorder(!!x_var, -!!fill_var))

    } else {

      stop(call. = FALSE, "Please supply fill_var with a categorical or numerical column")
    }

  } else { # fill_var is null

    data_mod <- data %>%
      dplyr::mutate(!!x_var := forcats::fct_reorder(!!x_var, -!!y_var))

  }

  # orientation

  if(orientation == "h"){

    p <- ggplot2::ggplot(data_mod, ggplot2::aes(x = stats::reorder(!!x_var, !!y_var), y = !!y_var))

  }else if(orientation == "v"){

    p <- ggplot2::ggplot(data_mod, ggplot2::aes(x = stats::reorder(!!x_var, -!!y_var), y = !!y_var))

  }else{

    stop(call. = FALSE, "Please supply orientation with 'h' for horizontal or 'v' for vertical")

  }


  if (!rlang::quo_is_null(fill_var)) {

    p <- p + ggplot2::aes(fill = !!fill_var)+
      ggplot2::geom_bar(stat = "identity", position = position, width = 0.8)

  } else {

    p <- p + ggplot2::geom_bar(stat = "identity", fill = fill, width = 0.8)

  }

  if(!rlang::quo_is_null(label_var)){

    label_var_cat <- data %>% dplyr::ungroup() %>% dplyr::select(!!label_var) %>%
      dplyr::select(c(tidyselect::where(~ is.character(.x)|is.factor(.x)|is.ordered(.x))))

    label_var_num <- data %>% dplyr::ungroup() %>% dplyr::select(!!label_var) %>%
      dplyr::select(c(tidyselect::where(~ is.numeric(.x))))

    if(position == "dodge"){

      if(orientation == "h"){

        if (length(label_var_cat)==1){

          p <- p +
            ggplot2::geom_text(
              ggplot2::aes(
                label= !!label_var,
                hjust = ifelse(!!y_var < 0, 1.10, -0.15)
              ),
              position = ggplot2::position_dodge(1),
              color = label_color, size = label_size
            )


        } else if(length(label_var_num)==1){
          p <- p +
            ggplot2::geom_text(
              ggplot2::aes(
                label= ifelse(
                  abs(!!label_var) >= 1e6, paste0(round(!!label_var/1e6, 1), "M"),
                  ifelse(abs(!!label_var) >= 1e3, paste0(round(!!label_var/1e3, 1), "K"), round(!!label_var,3))
                ),
                hjust = ifelse(!!y_var < 0, 1.10, -0.15)
              ),
              position = ggplot2::position_dodge(1),
              color = label_color, size = label_size
            )
        }


      } else{

        if (length(label_var_cat)==1){

          p <- p +
            ggplot2::geom_text(
              ggplot2::aes(
                label= !!label_var,
                vjust = ifelse(!!y_var < 0, 1.5, -0.5)
              ),
              position = ggplot2::position_dodge(1),
              color = label_color, size = label_size
            )


        } else if(length(label_var_num)==1){
          p <- p +
            ggplot2::geom_text(
              ggplot2::aes(
                label= ifelse(
                  abs(!!label_var) >= 1e6, paste0(round(!!label_var/1e6, 1), "M"),
                  ifelse(abs(!!label_var) >= 1e3, paste0(round(!!label_var/1e3, 1), "K"), round(!!label_var,3))
                ),
                vjust = ifelse(!!y_var < 0, 1.5, -0.5)
              ),
              position = ggplot2::position_dodge(1),
              color = label_color, size = label_size
            )
        }

      }

    } else if(position == "stack"){

      if (length(label_var_cat)==1){

        p <- p +
          ggplot2::geom_text(
            ggplot2::aes(label=!!label_var),
            position = ggplot2::position_stack(vjust = 0.5),
            color = label_color, size = label_size
          )


      } else if(length(label_var_num)==1){
        p <- p +
          ggplot2::geom_text(
            ggplot2::aes(
              label= ifelse(
                abs(!!label_var) >= 1e6, paste0(round(!!label_var/1e6, 1), "M"),
                ifelse(abs(!!label_var) >= 1e3, paste0(round(!!label_var/1e3, 1), "K"), round(!!label_var,3))
              )
            ),
            position = ggplot2::position_stack(vjust = 0.5),
            color = label_color, size = label_size
          )
      }

    } else if(position == "fill"){
      if (length(label_var_cat)==1){

        p <- p +
          ggplot2::geom_text(
            ggplot2::aes(label=!!label_var),
            position = ggplot2::position_fill(vjust = 0.5),
            color = label_color, size = label_size
          )


      } else if(length(label_var_num)==1){
        p <- p +
          ggplot2::geom_text(
            ggplot2::aes(
              label= ifelse(
                abs(!!label_var) >= 1e6, paste0(round(!!label_var/1e6, 1), "M"),
                ifelse(abs(!!label_var) >= 1e3, paste0(round(!!label_var/1e3, 1), "K"), round(!!label_var,3))
              )
            ),
            position = ggplot2::position_fill(vjust = 0.5),
            color = label_color, size = label_size
          )
      }
    } else {
      stop(call. = FALSE, "Error")
    }
  }

  p <- p + ggplot2::labs(title = title, x = x_var, y = y_var)

  max_limit <- max(data_mod %>% dplyr::ungroup() %>% dplyr::select(!!y_var) %>% dplyr::pull())*1.25

  min <- min(data_mod %>% dplyr::ungroup() %>% dplyr::select(!!y_var) %>% dplyr::pull())

  if(min>=0){
    min_limit <- 0
  }else{
    min_limit <- min*1.25
  }

  if(scale_axis_pct){

    if(position == "stack" || position == "fill"){
      p <- p +
        ggplot2::scale_y_continuous(
          labels = scales::percent_format(scale = 100, accuracy = 1) #scale = 1, *1 not *100
        )
    } else{
      p <- p +
        ggplot2::scale_y_continuous(
          limits = c(min_limit,max_limit),
          labels = scales::percent_format(scale = 100, accuracy = 1) #scale = 1, *1 not *100
        )
    }
  } else{
    if(position == "stack" || position == "fill"){
      p <- p +
        ggplot2::scale_y_continuous(
          labels = scales::label_number(scale_cut = scales::cut_short_scale())
        )

    } else{
      p <- p +
        ggplot2::scale_y_continuous(
          limits = c(min_limit,max_limit),
          labels = scales::label_number(scale_cut = scales::cut_short_scale())
        )
    }
  }

  length <- data %>% dplyr::ungroup() %>%
    dplyr::select(!!x_var) %>% dplyr::pull() %>% length()

  p <- p +
    ggplot2::geom_vline(
      xintercept = seq(0.5, length+1, by = 1),
      color = "gray", linewidth = 0.5, alpha = 0.5
    )


  if(orientation == "h"){

    if (length(fill_var_cat)==1){

      p <- p +
        ggplot2::coord_flip() +
        ggplot2::scale_fill_manual(values = rev(pal_discrete))

    } else if(length(fill_var_num)==1){

      p <- p +
        ggplot2::coord_flip() +
        #ggplot2::scale_fill_gradient2(low = "red",mid = "white",high = "blue", midpoint = 0)+
        ggthemes::scale_fill_gradient2_tableau(palette = "Orange-Blue Diverging")

    } else {

      p <- p +
        ggplot2::coord_flip() +
        ggplot2::scale_fill_manual(values = rev(pal_discrete))
    }


  }else{

    if (length(fill_var_cat)==1){

      p <- p +
        ggplot2::scale_fill_manual(values = rev(pal_discrete))

    } else if(length(fill_var_num)==1){

      p <- p +
        #ggplot2::scale_fill_gradient2(low = "red",mid = "white",high = "blue", midpoint = 0)+
        ggthemes::scale_fill_gradient2_tableau(palette = "Orange-Blue Diverging")

    } else {

      p <- p +
        ggplot2::scale_fill_manual(values = rev(pal_discrete))
    }

  }


  p <- p +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 9, color = "gray30"),
      axis.text = ggplot2::element_text(size = 8, color = "gray30"),
      axis.title = ggplot2::element_text(size = 11, hjust = 1, color = "gray30"),
      plot.title = ggplot2::element_text(
        size = 12, face="bold", hjust = 0.5, color = "grey30",
        margin = ggplot2::margin(b=10)
      ),
      legend.position = legend_position,
      legend.text = ggplot2::element_text(size = 9, color = "gray30"),
      legend.title = ggplot2::element_text(size = 11, color = "gray30"),
      plot.background = ggplot2::element_rect(fill = "white", color = "white"),
      panel.background = ggplot2::element_rect(fill = "white", color = "white"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank() #ggplot2::element_line(color = "gray91")
    )

  return(p)

}

