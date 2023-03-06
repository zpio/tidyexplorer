#' Descriptive Statistic
#'
#' This function calculate descriptive statistics for numeric variables in a data frame
#'
#'
#' @param .data A data.frame or tibble.
#'
#' @return A tibble with descriptive statistics for all numeric variables in .data.
#'
#' @examples
#'
#' library(dplyr)
#'
#' iris %>%
#'    descriptiveStats()
#'
#' iris %>%
#'    group_by(Species) %>%
#'    descriptiveStats()
#'
#'
#' @export
descriptiveStats <- function(.data) {

  if (!is.data.frame(.data)) {
    stop(call. = FALSE, ".data is not a data-frame or tibble. Please supply a data.frame or tibble.")
  }

  df_num <- .data %>% dplyr::select( c(tidyselect::where(~ is.numeric(.x))))

  if (length(df_num)==0){
    stop("There are no numeric variables")
  }

  group_vars <- df_num %>% dplyr::group_vars()

  dfDescription <- df_num %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::everything(),
        list(
          n = ~ sum(!is.na(.x)),
          na = ~ sum(is.na(.x)),
          mean = ~ round(mean(.x, na.rm = TRUE),2),
          median = ~ round(stats::median(.x, na.rm = TRUE),2),
          sd = ~ round(stats::sd(.x, na.rm = TRUE),2),
          se =  ~ round(stats::sd(.x, na.rm = TRUE)/sqrt(sum(!is.na(.x))),3),
          skewness = ~ moments::skewness(.x, na.rm = TRUE),
          kurtosis = ~ moments::kurtosis(.x, na.rm = TRUE),
          min = ~ min(.x, na.rm = TRUE),
          max = ~ max(.x, na.rm = TRUE),
          IQR = ~ stats::IQR(.x, na.rm = TRUE),
          Q1 = ~ stats::quantile(.x, 0.25, na.rm = TRUE),
          Q3 = ~ stats::quantile(.x, 0.75, na.rm = TRUE),
          p10 = ~ stats::quantile(.x,  probs = 0.1),
          p25 = ~ stats::quantile(.x,  probs = 0.25),
          p50 = ~ stats::quantile(.x,  probs = 0.50),
          p75 = ~ stats::quantile(.x,  probs = 0.75)
        ),
        .names = "{.col}%%{.fn}"
      )
    ) %>%
    tidyr::pivot_longer(
      !any_of(group_vars),
      names_to = c("set", ".value"),
      names_pattern = "(.*)%%(.*)"
    ) %>%
    dplyr::select(variable = set, dplyr::everything()) %>%
    dplyr::arrange(variable) %>% dplyr::ungroup()

  return(dfDescription)
}










