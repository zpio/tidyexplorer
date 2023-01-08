#' Statistical Data: Box Plot
#'
#' @param .data .data A `tibble` or `data.frame`
#'
#' @return A `tibble`
#'
#' @examples
#' library(dplyr)
#'
#' iris %>% select(Sepal.Width) %>%
#'   data_BoxPlot_Outlier()
#'
#' @export
data_BoxPlot_Outlier <- function(.data) {

  if (!is.data.frame(.data)) {
    stop(call. = FALSE, ".data is not a data-frame or tibble. Please supply a data.frame or tibble.")
  }

  df_num <- .data %>% dplyr::select(c(tidyselect::where(~ is.numeric(.x))))

  if (length(df_num)==0){
    stop(call. = FALSE, "Please supply a data-frame or tibble with a numeric column")
  }

  if (length(df_num)>1){
    stop(call. = FALSE, "Please supply 1 numeric column")
  } else{
    name <- df_num %>% names()
  }

  var <- rlang::sym(name)

  # with outlier

  data_with_outlier <-
    df_num %>%
    dplyr::select(!!var) %>%
    dplyr::filter(!is.na(!!var)) %>%
    dplyr::mutate(name = 'With Outliers')

  # without outlier

  quartiles <- df_num %>%
    dplyr::select(!!var) %>%
    dplyr::filter(!is.na(!!var)) %>%
    stats::quantile(probs=c(.25, .75), na.rm = TRUE)

  IQR <- df_num %>%
    dplyr::select(!!var) %>%
    dplyr::filter(!is.na(!!var)) %>%
    dplyr::pull() %>%
    stats::IQR(na.rm = TRUE)

  Lower <- quartiles[1] - 1.5*IQR
  Upper <- quartiles[2] + 1.5*IQR

  data_without_outlier <-
    df_num %>%
    dplyr::select(!!var) %>%
    dplyr::filter(!is.na(!!var)) %>%
    dplyr::filter((!!var) > Lower) %>%
    dplyr::filter((!!var) < Upper) %>%
    dplyr::mutate(name = 'Without Outliers')

  data <- dplyr::bind_rows(data_with_outlier, data_without_outlier)

  box <- data %>%
    dplyr::group_by(name) %>%
    dplyr::summarise(
      n = dplyr::n(),
      median = stats::median(!!var, na.rm = TRUE),
      mean = mean(!!var, na.rm = TRUE),
      p25 = stats::quantile(!!var, .25, na.rm = TRUE),
      p75 = stats::quantile(!!var, .75, na.rm = TRUE),
      iqr = p75 - p25,
      lower_whisker =  p25 - 1.5*iqr,
      upper_whisker =  p75 + 1.5*iqr
    ) %>% dplyr::ungroup()

  return(box)

}
