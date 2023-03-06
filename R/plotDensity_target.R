#' Visualize Distributions based on target variable and categorical variables
#'
#' @param .data A data frame.
#' @param target_num The name of the target numeric variable to analyze.
#' @param dir_cat The direction of facet_wrap. Options are "h" for horizontal and "v" for vertical.
#' @param ncol_cat The number of columns in facet_wrap.
#' @param interactive A boolean value that specifies whether the plot should be returned static (ggplot2) or interactive (Plotly).
#'
#' @return A static `ggplot2` plot or an interactive `plotly` plot
#'
#' @examples
#' library(dplyr)
#'
#' mtcars <- mtcars %>% as_tibble() %>%
#'    mutate(across(c(am, carb, cyl, gear, vs), as.factor))
#'
#' mtcars %>% select(mpg, cyl, gear) %>%
#'     plotDensity_target(target_num = mpg)
#'
#' iris %>%
#'     plotDensity_target(target_num = Petal.Length)
#'
#' @export
plotDensity_target <- function(.data,
                               target_num,
                               dir_cat="h",
                               ncol_cat = 4,
                               interactive = FALSE) {

  tar <- .data %>% dplyr::select({{target_num}}) %>%
    dplyr::select(c(tidyselect::where(~ is.numeric(.x)))) %>%
    names()

  if(length(tar)!=0){

    df_cat <- .data %>% dplyr::select(tidyselect::where(~ is.character(.x)|is.factor(.x)|is.ordered(.x)))

    if(length(df_cat)==0){
      stop(call. = FALSE, "Please supply a data-frame with a categorical column")
    }

    df_cat_num <- .data %>% dplyr::select({{target_num}}, c(tidyselect::where(~ is.character(.x)|is.factor(.x)|is.ordered(.x))))

    pal_discrete <-
      rep(ggthemes::ggthemes_data$tableau$`color-palettes`$regular$`Tableau 10`$value, 10)

    obj <- .data %>% dplyr::select({{target_num}}) %>% names()

    p1 <- df_cat_num %>%
      tidyr::pivot_longer(
        !dplyr::any_of(obj),
        names_to = "variable",
        values_to = "Categories"
      ) %>%
      dplyr::arrange(variable) %>%
      ggplot2::ggplot(
        ggplot2::aes(x = {{target_num}}, y = -0.25, fill = Categories, color = Categories)
      )+
      ggplot2::geom_boxplot(alpha = 0.3, lwd=1, width = 0.20, outlier.shape = NA)+
      ggplot2::stat_boxplot(geom = "errorbar", width = 0.15)+
      ggplot2::geom_density(
        ggplot2::aes(x = {{target_num}}, y = ggplot2::after_stat(scaled), fill=Categories, color=Categories),
        inherit.aes = FALSE, alpha = 0.5, size = 1)+
      ggplot2::scale_x_continuous(labels = scales::comma) +
      ggplot2::scale_fill_manual(values = pal_discrete, guide="none")+
      ggplot2::scale_color_manual(values = pal_discrete, guide="none")+
      ggplot2::facet_wrap(dplyr::vars(variable, Categories), dir=dir_cat, ncol=ncol_cat, scales = "free")+
      ggplot2::labs(y="", title = paste(obj, "-", "categorical variables"))+
      ggplot2::theme_minimal()+
      ggplot2::theme(
        axis.text = ggplot2::element_text(size = 8),
        plot.title = ggplot2::element_text(size = 11, face="bold", hjust = 0.5),
        strip.text = ggplot2::element_text(face="bold"),
        panel.spacing.y = ggplot2::unit(2, "lines")
      )
  }

  if(!interactive){
    return(p1)
  }else{
    p <- plotly::ggplotly(p1, tooltip = c("x", "y", "fill")) %>%
      plotly::layout(
        showlegend = F,
        #title = list(text=paste(""), size=1),
        font = list(family = "Segoe UI"),
        margin = list(t = 80, b=50),
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
