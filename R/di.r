#' Display summary statistics
#'
#' \code{di} loads \code{View} of summary statistics about the dataset \code{df}
#'
#' This function displays summary statistics about \code{df} to help analyse
#'   the state of the data. It lists number of unique values, number of NAs, etc.
#'
#' @param df object (i.e. data.frame) to display properties of
#' @param vars vector of strings to indicate which columns to include in the
#'   output. By default all columns are included
#'
#' @examples
#' data = data.frame(apple = 1:4,
#'                   orange = letters[1:4],
#'                   orangutan = c("dfg", "sdf", NA, "134"),
#'                   applause = runif(4),
#'                   a = c(TRUE, FALSE, FALSE, NA))
#'
#' di(data)
#' @import dplyr
#' @export di
di <- function(df, vars = colnames(df)){
  temp <- df %>%
    dplyr::select_at(.vars = vars)

  data.frame(var_names = colnames(temp)) %>%
    dplyr::mutate(var_type = purrr::map_chr(temp, typeof)) %>%
    dplyr::mutate(num_unique_values = purrr::map_int(temp, function(x) length(unique(x)))) %>%
    dplyr::mutate(num_NAs = purrr::map_int(temp, function(x) sum(is.na(x)))) %>%
    dplyr::mutate(var_values = purrr::map(temp, unique)) %>%
    dplyr::mutate(sample_values = purrr::map(var_values, function(x) {if(length(x) <= 10) return(x); sample(x, 10)})) %>%
    dplyr::select(var_names, var_type, num_unique_values, num_NAs, sample_values) %>%
    dplyr::arrange(var_names) %>%
    View()
}
