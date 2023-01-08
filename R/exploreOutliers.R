#' Describe the outliers of the numeric variables of the data frame
#'
#' @param .data A `tibble` or `data.frame`
#'
#' @return A `tibble`
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
          na = ~ sum(is.na(.x)),
          outliersCount = ~ length(grDevices::boxplot.stats(.x)$out),

          outliersRatio = ~ round(length(grDevices::boxplot.stats(.x)$out)/length(.x), 3),

          outliersMean = ~ round(mean(
            ifelse(.x %in% grDevices::boxplot.stats(.x)$out, .x, NA),
            na.rm = TRUE), 2),

          meanWithOutliers = ~ round(mean(.x, na.rm = TRUE), 2),

          meanWithoutOutliers = ~ round(mean(
            ifelse(.x %in% grDevices::boxplot.stats(.x)$out, NA, .x),
            na.rm = TRUE), 2)
        )),
      .groups = 'keep') %>%
    tidyr::pivot_longer(
      !dplyr::any_of(group_vars),
      names_to = c("set", ".value"),
      names_pattern = "(.*)_(.*)"
    ) %>%
    dplyr::select(variable = set, dplyr::everything()) %>%
    dplyr::arrange(variable) %>% dplyr::ungroup()

  return(dfDescription)
}





