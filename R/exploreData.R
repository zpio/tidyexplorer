#' Explore a data frame or tibble
#'
#' This function explores a all variables in a data frame.
#'
#' @param .data A data frame or tibble to summarize.
#'
#' @return  A data frame with the following columns: column's data type, number of non-missing values,
#' number of missing values, number of unique values, and the percentage of unique values.
#'
#' @examples
#'
#' library(dplyr)
#'
#' mtcars <- mtcars %>% as_tibble() %>%
#'    mutate(across(c(am, carb, cyl, gear, vs), as.factor))
#'
#' iris <- as_tibble(iris)
#'
#' iris %>%
#'    exploreData()
#'
#' mtcars %>%
#'    exploreData()
#'
#' @export
exploreData <- function(.data) {

  if (!is.data.frame(.data)) {
    stop(call. = FALSE, ".data is not a data-frame or tibble. Please supply a data.frame or tibble.")
  }

  dfDescription <- .data %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::everything(),
        list(
          type = ~ class(.x)[1],
          n = ~ length(.x),
          NAs = ~ sum(is.na(.x) | as.character(.x)==""),
          NAsPct = ~ round(sum(is.na(.x) | as.character(.x)=="")/length(.x),3),
          unique = ~ length(unique(.x)),
          uniquePct = ~ round(length(unique(.x))/length(.x),3)
        ),
        .names = "{.col}%%{.fn}"
      )
    ) %>%
    tibble::rownames_to_column() %>%
    tidyr::pivot_longer(
      !c(rowname),
      names_to = c("set", ".value"),
      names_pattern = "(.*)%%(.*)"
    ) %>%
    dplyr::select(variable = set, dplyr::everything(), -rowname) %>%
    dplyr::arrange(variable) %>% dplyr::ungroup()

  return(dfDescription)
}





