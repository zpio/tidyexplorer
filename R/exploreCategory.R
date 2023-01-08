#' Description of all categorical variables
#'
#' @param .data A `tibble` or `data.frame`
#' @param top Number of levels to return for each variable
#'
#' @return A `tibble`
#'
#' @examples
#' iris %>%
#'    exploreCategory()
#'
#' @export
exploreCategory <- function(.data, top = 500) {

  if (!is.data.frame(.data)) {
    stop(call. = FALSE, ".data is not a data-frame or tibble. Please supply a data.frame or tibble.")
  }

  df <- dplyr::as_tibble(.data)

  df_cat <- df %>%
    dplyr::select(c(tidyselect::where(~ is.character(.x)|is.factor(.x)|is.ordered(.x))))

  if (length(df_cat)==0){
    stop("Please supply a data-frame or tibble with a categorical column")
  }

  dfDescription <- df_cat %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
    tibble::rownames_to_column(var = "row") %>%
    tidyr::gather(key = "variable", value = "value", -row) %>%
    dplyr::group_by(variable, levels = value) %>%
    dplyr::count() %>% dplyr::ungroup() %>%
    dplyr::group_by(variable) %>%
    dplyr::arrange(dplyr::desc(n), .by_group=TRUE) %>%
    dplyr::slice_head(n = top) %>%
    dplyr::transmute(
      variable = variable,
      levels = levels,
      freq = n,
      N = sum(n),
      ratio = round(n/N, 3),
      rank = rank(max(n) - n, ties.method = "min")
    ) %>% dplyr::ungroup()

  return(dfDescription)
}
