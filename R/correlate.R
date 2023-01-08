#' Correlation coefficient
#'
#' @param .data A `tibble` or `data.frame`
#' @param method a character string: "pearson", "kendall" or "spearman".
#'
#' @return A `tibble`
#'
#' @examples
#'
#' iris %>%
#'    correlate()
#'
#'
#' @export
correlate <- function(.data, method = "pearson"){

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
    dplyr::mutate(variables = paste(var1, "-", var2)) %>%
    dplyr::arrange(dplyr::desc(cor)) %>%
    dplyr::mutate(variables = stats::reorder(variables, cor))

  return(df)

}
