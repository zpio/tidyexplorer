#' Explore Outliers
#'
#' This function explores the outliers of all numeric variables in a data frame.
#'
#' @param .data A data frame or tibble containing numeric columns.
#'
#' @return A data frame with the following columns: The name of the numeric variable,
#' number of non-missing observations, number of missing observations,
#' number of outliers detected using the boxplot method,
#' ratio of outliers to the total number of observations, The mean value of the outliers,
#' mean value of the variable including outliers, mean value of the variable after removing outliers.
#'
#' @examples
#'
#' library(dplyr)
#'
#' iris %>%
#'    exploreOutliers()
#'
#' iris %>%
#'    group_by(Species) %>%
#'    exploreOutliers()
#'
#' @export
#'
exploreOutliers <- function(.data) {

  if (!is.data.frame(.data)) {
    stop(call. = FALSE, ".data is not a data-frame or tibble. Please supply a data.frame or tibble.")
  }

  df_num <- .data %>% dplyr::select( c(tidyselect::where(~ is.numeric(.x))))

  if (length(df_num)==0){
    stop(call. = FALSE, "Please supply a data-frame or tibble with a numeric column")
  }

  group_vars <- df_num %>% dplyr::group_vars()

  dfDescription <- df_num %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::everything(),
        list(
          n = ~ length(.x),

          na = ~ sum(is.na(.x) | as.character(.x)==""),

          outliersCount = ~ length(grDevices::boxplot.stats(.x)$out),

          outliersRatio = ~ round(length(grDevices::boxplot.stats(.x)$out)/length(.x), 3),

          outliersMean = ~ round(mean(
            ifelse(.x %in% grDevices::boxplot.stats(.x)$out, .x, NA),
            na.rm = TRUE), 2),

          meanWithOutliers = ~ round(mean(.x, na.rm = TRUE), 2),

          meanWithoutOutliers = ~ round(mean(
            ifelse(.x %in% grDevices::boxplot.stats(.x)$out, NA, .x),
            na.rm = TRUE), 2)
        ),
        .names = "{.col}%%{.fn}"
      ),
      .groups = 'keep') %>%
    tidyr::pivot_longer(
      !dplyr::any_of(group_vars),
      names_to = c("set", ".value"),
      names_pattern = "(.*)%%(.*)"
    ) %>%
    dplyr::select(variable = set, dplyr::everything()) %>%
    dplyr::arrange(variable) %>% dplyr::ungroup()

  return(dfDescription)
}





