#' Summarize Numeric Data
#'
#' This function summary numeric variables including count, missing values, minimum, maximum, median, quartiles, mean, standard deviation,
#' number of zeros, number of negatives, and number of outliers.
#'
#' @param .data a data frame or tibble
#'
#' @return a tibble with one row for each numeric variable and columns for count, missing values,
#' minimum, maximum, median, quartiles, mean, standard deviation, number of zeros, number of negatives,
#' and number of outliers.
#'
#' @examples
#' library(dplyr)
#'
#' iris %>%
#'    exploreNumeric()
#'
#' iris %>%
#'    select(Sepal.Length, Sepal.Width) %>%
#'    exploreNumeric()
#'
#' iris %>%
#'   group_by(Species) %>%
#'   exploreNumeric()
#'
#' mtcars <- mtcars %>% as_tibble() %>%
#'   mutate(across(c(am, carb, cyl, gear, vs), as.factor))
#'
#' mtcars %>%
#'    dplyr::group_by(cyl, am) %>%
#'    exploreNumeric()
#'
#' @export
#'
exploreNumeric <- function(.data) {

  if (!is.data.frame(.data)) {
    stop(call. = FALSE, ".data is not a data-frame or tibble. Please supply a data.frame or tibble.")
  }

  df_num <- .data %>% dplyr::select(c(tidyselect::where(~ is.numeric(.x))))

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
          naPct = ~ round(sum(is.na(.x) | as.character(.x)=="")/length(.x),3),
          min = ~ min(.x, na.rm = TRUE),
          max = ~ max(.x, na.rm = TRUE),
          Q1 = ~ stats::quantile(.x, 0.25, na.rm = TRUE),
          mean = ~ round(mean(.x, na.rm = TRUE),2),
          median = ~ round(stats::median(.x, na.rm = TRUE),2),
          Q3 = ~ stats::quantile(.x, 0.75, na.rm = TRUE),
          sd = ~ round(stats::sd(.x, na.rm = TRUE),2),
          zeros = ~ sum(.x == 0, na.rm = TRUE),
          negatives = ~ sum(.x < 0, na.rm = TRUE),
          outliers = ~ length(grDevices::boxplot.stats(.x)$out)
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





